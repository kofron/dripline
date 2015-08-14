'''
'''

from __future__ import absolute_import
__docformat__ = 'reStructuredText'

import abc
import datetime
import logging
import traceback
import uuid

from .endpoint import Endpoint

__all__ = ['DataLogger',
          ]
logger = logging.getLogger(__name__)


class DataLogger(object):
    '''
    Base class for objects which need to call their own methods periodically.
    '''
    __metaclass__ = abc.ABCMeta

    def __init__(self,
                 log_interval=0.,
                 max_interval=0,
                 max_fractional_change=0,
                 alert_routing_key='sensor_value',
                 **kwargs):
        '''
        Keyword Args:
            log_interval (float): minimum time in seconds between sequential log events (note that this may or may not produce an actual log broadcast)
            max_interval (float): If > 0, any log event exceding this number of seconds since the last broadcast will trigger a broadcast.
            max_fractional_change (float): If > 0, any log event which produces a value which differs from the previous value by more than this amount (expressed as a fraction, ie 10% change is 0.1) will trigger a broadcast
            alert_routing_key (str): routing key for the alert message send when broadcasting a logging event result. The default value of 'sensor_value' is valid for DataLoggers which represent physical quantities being stored to the slow controls database tables
        
        '''
        self.alert_routing_key=alert_routing_key
        self._log_interval = log_interval
        self._max_interval = max_interval
        self._max_fractional_change = max_fractional_change
        self._is_logging = False
        self._timeout_handle = None

        self._last_log_time = None
        self._last_log_value = None

    def get_value(self):
        raise NotImplementedError('get value in derrived class')

    def store_value(self, severity='sensor_value', value=None):
        raise NotImplementedError('store value in derrived class')

    @property
    def log_interval(self):
        return self._log_interval
    @log_interval.setter
    def log_interval(self, value):
        value = float(value)
        if value < 0:
            raise ValueError('Log interval cannot be < 0')
        self._log_interval = value

    @property
    def max_interval(self):
        return self._max_interval
    @max_interval.setter
    def max_interval(self, value):
        value = float(value)
        if value < 0:
            raise ValueError('max log interval cannot be < 0')
        self._max_interval = value

    @property
    def max_fractional_change(self):
        return self._max_fractional_change
    @max_fractional_change.setter
    def max_fractional_change(self, value):
        value = float(value)
        if value < 0:
            raise ValueError('fractional change cannot be < 0')
        self._max_fractional_change = value

    def _conditionally_send(self, to_send):
        '''
        consider sending value, but only if a send condition is met
        '''
        this_value = None
        try:
            this_value = float(to_send['values']['value_raw'])
        except TypeError:
            pass
        if self._last_log_value is None:
            logger.debug("log b/c no last log")
        elif (datetime.datetime.utcnow() - self._last_log_time).seconds > self._max_interval:
            logger.debug('log b/c too much time')
        elif (abs(self._last_log_value - this_value)/self._last_log_value) > self.max_fractional_change:
            logger.debug('log b/c change is too large')
        else:
            logger.debug('no log condition met, not logging')
            return
        self.store_value(to_send, severity=self.alert_routing_key)
        self._last_log_time = datetime.datetime.utcnow()
        self._last_log_value = this_value

    def _log_a_value(self):
        try:
            val = self.get_value()
            if val is None:
                raise UserWarning
                logger.warning('get returned None')
                if hasattr(self, 'name'):
                    logger.warning('for: {}'.format(self.name))
            to_send = {'from':self.name,
                       'values':val,
                      }
            self._conditionally_send(to_send)
        except UserWarning:
            logger.warning('get returned None')
            if hasattr(self, 'name'):
                logger.warning('for: {}'.format(self.name))
        except Exception as err:
            logger.error('got a: {}'.format(str(err)))
            logger.error('traceback follows:\n{}'.format(traceback.format_exc()))
        logger.info('value sent')
        if (self._log_interval <= 0) or (not self._is_logging):
            return
        self._timeout_handle = self.portal._connection.add_timeout(self._log_interval, self._log_a_value)

    def _stop_loop(self):
        try:
            self._is_logging = False
            self.portal._connection.remove_timeout(self._timeout_handle)
        except Warning:
            pass
        except:
            logger.error('something went wrong stopping')
            raise

    def _start_loop(self):
        self._is_logging = True
        if self._log_interval <= 0:
            raise Warning("log interval must be > 0")
        else:
            self.portal._connection.remove_timeout(self._timeout_handle)
            self._log_a_value()
            logger.info("log loop started")

    def _restart_loop(self):
        try:
            self._stop_loop()
        except Warning:
            pass
        self._start_loop()

    @property
    def logging_status(self):
        return self._is_logging
    @logging_status.setter
    def logging_status(self, value):
        logger.info('setting logging state to: {}'.format(value))
        if value in ['start', 'on']:
            self._start_loop()
        elif value in ['stop', 'off']:
            self._stop_loop()
        elif value in ['restart']:
            self._restart_loop()
        else:
            raise ValueError('unrecognized logger status setting')

