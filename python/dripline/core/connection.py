'''
A connection to the AMQP broker.

This code violates DRY programming with the Portal class. A resolution would be
very desirable, but is probably best achieved by having a version of this class
which provides the existing interface of those to, to the AsynchronousConnection
in pika. Portal could then have an instance of this class to interact with. That
constitutes a non-trivial reworking of some of this code for future me (or someone).
'''


from __future__ import absolute_import

# standard libs
import multiprocessing
import threading
import traceback
import uuid

# 3rd party libs
import pika

# internal libs
from .message import Message, AlertMessage, ReplyMessage
from ..core import exceptions

__all__ = ['Connection']

import logging
logger = logging.getLogger(__name__)

class Connection(object):
    '''
    A simple API to the AMQP system
    '''
    def __init__(self, broker_host='localhost', queue_name=None):
        '''
        Keyword Args:
            broker_host (str): network path for the amqp broker to connect to
            queue_name (str|None): name of the queue to bind to for receiving replies
        '''
        self._make_request_lock = threading.Lock()
        if queue_name is None:
            queue_name = "reply_queue-{}".format(uuid.uuid1().hex[:12])
        self.broker_host = broker_host
        conn_params = pika.ConnectionParameters(broker_host)
        self.conn = pika.BlockingConnection(conn_params)
        self.chan = self.conn.channel()
        self.chan.confirm_delivery()
        self._response = None
        self._response_encoding = None

        self._setup_amqp(queue_name)

    def __del__(self):
        if hasattr(self, 'conn') and self.conn.is_open:
            self.conn.close()

    def _setup_amqp(self, queue_name):
        '''
        ensures all exchanges are present and creates a response queue.
        '''
        self.chan.exchange_declare(exchange='requests', type='topic')
        try:
            self.queue = self.chan.queue_declare(queue=queue_name,
                                                 exclusive=True,
                                                 auto_delete=True,
                                                )
        except pika.exceptions.ChannelClosed:
            logger.error('reply queue "{}"already exists. Config must use unique names'.format(self.queue.method.queue))
            import sys
            sys.exit()
        self.chan.queue_bind(exchange='requests',
                             queue=self.queue.method.queue,
                             routing_key=self.queue.method.queue,
                            )

        self.chan.exchange_declare(exchange='alerts', type='topic')

        self.chan.basic_consume(self._on_response, queue=self.queue.method.queue, no_ack=False)

    def _on_response(self, channel, method, props, response):
        logger.debug('got a response: {}'.format(repr(response)))
        if self.corr_id == props.correlation_id:
            self._response = response
            self._response_encoding = props.content_encoding
            self.chan.basic_ack(delivery_tag=method.delivery_tag)
            self.chan.stop_consuming()
            logger.info('stopping consumption')
        else:
            logger.debug('corr id received does not match target:')
            logger.debug('target: {}'.format(self.corr_id))
            logger.debug("recv'd: {}".format(props.correlation_id))

    def start(self):
        try:
            self.chan.start_consuming()
        except pika.exceptions.ConnectionClosed:
            logger.error('connection broken in process event loop')
            raise
        logger.critical("end of consume")

    def send_request(self, target, request, decode=False, timeout=10):
        '''
        '''
        result_queue = multiprocessing.Queue()
        process = multiprocessing.Process(target=self._send_request, kwargs={'result_queue':result_queue, 'target':target, 'request':request, 'decode':decode})
        process.start()
        process.join(timeout)
        if process.is_alive():
            process.terminate()
            raise exceptions.DriplineTimeoutError("sending request timed out")
        else:
            result = result_queue.get()
        return result
        
    def _send_request(self, result_queue, target, request, decode=False):
        '''
        send a request to a specific consumer.
        '''
        self._make_request_lock.acquire()
        if isinstance(request, Message):
            to_send = request.to_msgpack()
            decode = True
        else:
            to_send = request

        self._response = None
        self._response_encoding = None
        self.corr_id = str(uuid.uuid4())
        pr = None
        try:
            pr = self.chan.basic_publish(exchange='requests',
                                         routing_key=target,
                                         body=to_send,
                                         properties=pika.BasicProperties(
                                           reply_to=self.queue.method.queue,
                                           content_encoding='application/msgpack',
                                           correlation_id=self.corr_id,
                                         ),
                                         mandatory=True,
                                        )
            logger.debug('publish success is: {}'.format(pr))
        except KeyError as err:
            if err.message == 'Basic.Ack':
                logger.warning("pika screwed up...\nit's probably fine")
            else:
                raise
        if not pr:
            #logger.warning('pr is: {}'.format(pr))
            self._response = ReplyMessage(retcode=102, payload={'ret_msg':'key <{}> not matched'.format(target)}).to_msgpack()
            #logger.warning('return code is hard coded, should not be')
            self._response_encoding = 'application/msgpack'
            raise exceptions.DriplineAMQPRoutingKeyError('key <{}> not matched'.format(target))
        
        # consume until response
        self.chan.start_consuming()

        if decode and (self._response_encoding is not None):
            if self._response_encoding.endswith('json'):
                to_return = Message.from_json(self._response)
            elif self._response_encoding.endswith('msgpack'):
                to_return = Message.from_msgpack(self._response)
            else:
                to_return = self._response
        else:
            to_return = self._response
        self._response = None
        self._response_encoding = None
        if to_return is None:
            logger.warning('to return is None')
        self._make_request_lock.release()
        result_queue.put(to_return)
        return to_return

    def send_alert(self, alert, severity):
        '''
        send an alert
        '''
        self.__alert_lock.acquire()
        self._ensure_connection()
        try:
            logger.info('sending an alert message: {}'.format(repr(alert)))
            message = AlertMessage()
            message.update({'payload':alert})
            packed = message.to_msgpack()
            pr = self.chan.basic_publish(exchange='alerts',
                                         properties=pika.BasicProperties(
                                           content_encoding='application/msgpack',
                                         ),
                                         routing_key=severity,
                                         body=packed,
                                        )
            if not pr:
                logger.error('alert unable to send')
                logger.warning('pr is: {}'.format(pr))
            logger.info('alert sent, returned:{}'.format(pr))
        except KeyError as err:
            if err.message == 'Basic.Ack':
                logger.warning("pika screwed up...\nit's probably fine")
            else:
                raise
        except Exception as err:
            logger.error('an error while sending alert')
            logger.error('traceback follows:\n{}'.format(traceback.format_exc()))
            raise
        finally:
            self.__alert_lock.release()

    @staticmethod
    def send_reply(chan, properties, reply):
        '''
        '''
        logger.debug('sending reply')
        if not isinstance(reply, ReplyMessage):
            logger.warn('send_reply expects a dripline.core.ReplyMessage, packing reply as payload of such, fix your code')
            reply = ReplyMessage(payload=reply)

        body = reply.to_encoding(properties.content_encoding)
        try:
            logger.debug('about to publish')
            pr = chan.basic_publish(exchange='requests',
                                    immediate=True,
                                    mandatory=True,
                                    routing_key=properties.reply_to,
                                    properties=pika.BasicProperties(
                                       correlation_id=properties.correlation_id,
                                       content_encoding=properties.content_encoding,
                                    ),
                                    body=body,
                                   )
            logger.debug('pr is:{}'.format(pr))
        except KeyError as err:
            if err.message == 'Basic.Ack':
                logger.warning('pika screwed up... maybe')
            else:
                raise
        logger.debug('reply sent')
