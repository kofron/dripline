'''
'''

from __future__ import absolute_import
import logging

import abc
import threading
import time
import msgpack

__all__ = ['DataLogger']
logger = logging.getLogger(__name__)

class DataLogger(object):
    __metaclass__ = abc.ABCMeta

    def __init__(self):
        self._log_interval = 0
        self._is_logging = False
        self._loop_process = threading.Timer([], {})

#    @abc.abstractmethod
    def get_value(self):
        raise NotImplementedError('get value in derrived class')

#    @abc.abstractmethod
    def store_value(self, value, severity='sensor_value'):
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

    def _log_a_value(self):
        to_send = {'from':self.name,
                   'value':self.get_value(),
                  }
        self.store_value(msgpack.packb(to_send), severity='sensor_value')
        if (self._log_interval <= 0) or (not self._is_logging):
            return
        self._loop_process = threading.Timer(self._log_interval, self._log_a_value, ())
        self._loop_process.start()

    def _stop_loop(self):
        self._is_logging = False
        if self._loop_process.is_alive():
            self._loop_process.cancel()
        else:
            raise Warning("loop process not running")

    def _start_loop(self):
        self._is_logging = True
        if self._loop_process.is_alive():
            raise Warning("loop process already started")
        elif self._log_interval <= 0:
            raise Exception("log interval must be > 0")
        else:
            self._loop_process = threading.Timer(self._log_interval, self._log_a_value, ())
            self._loop_process.start()

    def _restart_loop(self):
        try:
            self._stop_loop()
        except Warning:
            pass
        self._start_loop()

    @property
    def logging_status(self):
        translator = {True: 'running',
                      False: 'stopped'
                     }
        return translator[self._loop_process.is_alive()]
    @logging_status.setter
    def logging_status(self, value):
        if value in ['start', 'on']:
            self._start_loop()
        elif value in ['stop', 'off']:
            self._stop_loop()
        elif value in ['restart']:
            self._restart_loop()
        else:
            raise ValueError('unrecognized logger status setting')
