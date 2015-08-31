'''
'''

from __future__ import absolute_import

# standard imports
import logging

# internal imports
from .. import core
from .ethernet_scpi import EthernetSCPI

__all__ = []

logger = logging.getLogger(__name__)

__all__.append('DAQProvider')
class DAQProvider(core.Provider):
    '''
    Base class for providing a uniform interface to different DAQ systems
    '''
    def __init__(self,
                 daq_name,
                 run_table_endpoint,
                 directory_path,
                 **kwargs):
        '''
        '''
        core.Provider.__init__(self, **kwargs)

        self._stop_handle = None
        self.daq_name = daq_name
        self._run_name = None
        self.run_id = None
        self.directory_path = directory_path
        self.run_table_endpoint = run_table_endpoint
        self._acquisition_count = None

    @property
    def run_name(self):
        return self._run_name
    @run_name.setter
    def run_name(self, value):
        request = core.RequestMessage(msgop=core.OP_CMD,
                                      payload={'values':['do_insert'],
                                               'run_name':value,
                                              },
                                     )
        result = self.portal.send_request(self.run_table_endpoint,
                                          request=request,
                                         )
        self.run_id = result.payload['run_id']
        self._run_name = value
        self._acquisition_count = 0

    def end_run(self):
        run_was = self.run_id
        if self._stop_handle is not None:
            self.portal._connection.remove_timeout(self._stop_handle)
            self._stop_handle = None
        self._run_name = None
        self.run_id = None
        logger.info('run <{}> ended'.format(run_was))
    
    def start_run(self, run_name):
        '''
        '''
        self.run_name = run_name
    
    def start_timed_run(self, run_name, run_time):
        '''
        '''
        self._stop_handle = self.portal._connection.add_timeout(int(run_time), self.end_run)
        self.start_run(run_name)


__all__.append('MantisAcquisitionInterface')
class MantisAcquisitionInterface(DAQProvider, core.Spime):
    '''
    A DAQProvider for interacting with Mantis DAQ
    '''
    def __init__(self,
                 mantis_queue='mantis',
                 filename_prefix='',
                 **kwargs
                ):
        DAQProvider.__init__(self, **kwargs)
        core.Spime.__init__(self, **kwargs)
        self.alert_routing_key = 'daq_requests'
        self.mantis_queue = mantis_queue
        self.filename_prefix = filename_prefix

    def start_run(self, run_name):
        super(MantisAcquisitionInterface, self).start_run(run_name)
        self.on_get()
        self.logging_status = 'on'

    def on_get(self):
        '''
        Setting an on_get so that the logging functionality can be used to queue multiple acquisitions.
        '''
        if self.run_id is None:
            raise core.DriplineInternalError('run number is None, must request a run_id assignment prior to starting acquisition')
        filepath = '{}/{}{:09d}_{:09d}.egg'.format(
                                        self.directory_path,
                                        self.filename_prefix,
                                        self.run_id,
                                        self._acquisition_count
                                                  )
        request = core.RequestMessage(payload={'values':[], 'file':filepath},
                                      msgop=core.OP_RUN,
                                     )
        result = self.portal.send_request(self.mantis_queue,
                                          request=request,
                                         )
        if not result.retcode == 0:
            msg = ''
            if 'ret_msg' in result.payload:
                msg = result.payload['ret_msg']
            logger.warning('got an error from mantis: {}'.format(msg))
        else:
            self._acquisition_count += 1
            return "acquisition of [{}] requested".format(filepath)

    def end_run(self):
        self.logging_status = 'stop'
        super(MantisAcquisitionInterface, self).end_run()
        self._acquisition_count = 0


__all__.append('RSAAcquisitionInterface')
class RSAAcquisitionInterface(DAQProvider, EthernetSCPI):
    '''
    A DAQProvider for interacting with the RSA
    '''
    def __init__(self,
                 max_nb_files=10000,
                 **kwargs):
        DAQProvider.__init__(self, **kwargs)
        EthernetSCPI.__init__(self, **kwargs)
        self.max_nb_files = max_nb_files

    def start_run(self, run_name):
        super(RSAAcquisitionInterface, self).start_run(run_name)
        # ensure the output format is set to mat
        self.send(["SENS:ACQ:FSAV:FORM MAT;*OPC?"])
        # build strings for output directory and file prefix, then set those
        file_directory = "\\".join([self.directory_path, '{:09d}'.format(self.run_id)])
        file_base = "rid{:09d}".format(self.run_id)
        self.send(['SENS:ACQ:FSAV:LOC "{}"'.format(file_directory),
                   'SENS:ACQ:FSAV:NAME:BASE "{}"'.format(file_base),
                   "*OPC?"
                  ]
                 )
        # Set the maximum number of events (note that the default is 10k)
        self.send(['SENS:ACQ:FSAV:FILE:MAX {:d};*OPC?'.format(self.max_nb_files)])
        # ensure in triggered mode
        self.send(['TRIG:SEQ:STAT 1;*OPC?'])
        # actually start to FastSave
        self.send(['SENS:ACQ:FSAV:ENAB 1;*OPC?'])

    def end_run(self):
        # something to stop FastSave
        self.send(['SENS:ACQ:FSAV:ENAB 0'])
        self.send(['TRIG:SEQ:STAT 0;*OPC?'])
        super(RSAAcquisitionInterface, self).end_run()
