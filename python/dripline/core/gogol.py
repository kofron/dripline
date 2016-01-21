'''
Basic abstraction for binding to the alerts exchange
'''

from __future__ import absolute_import

# standard libs
import logging
import re
import traceback
import uuid

# 3rd party libs
import pika

# internal imports
from . import exceptions
from .message import Message
from .service import Service
from .utilities import fancy_doc

__all__ = ['Gogol']

logger = logging.getLogger(__name__)


@fancy_doc
class Gogol(Service):
    def __init__(self, exchange='alerts', keys=['#'], name=None, **kwargs): 
        '''
        exchange (str): AMQP exchange on the broker to which we will be binding
        keys (list): list of strings, each string will be a routing key bound to the provided exchange.

        '''
        logger.debug('Gogol initializing')
        if name is None:
            name = __name__ + '-' + uuid.uuid1().hex[:12]
        Service.__init__(self, amqp_url=broker_host, exchange=exchange, keys=keys, name=name)

    def this_consume(self, message, basic_deliver=None):
        raise NotImplementedError('you must set this_consume to a valid function')

    def on_alert_message(self, channel, method, properties, message):
        logger.debug('in process_message callback')
        try:
            message_unpacked = Message.from_encoded(message, properties.content_encoding)
            self.this_consume(message_unpacked, method)
        except exceptions.DriplineException as err:
            logger.warning(str(err))
        except Exception as err:
            logger.warning('got an exception:\n{}'.format(str(err)))
            logger.debug('traceback follows:\n{}'.format(traceback.format_exc()))
            raise

    def start(self):
        logger.debug("AlertConsmer consume starting")
        self.run()
