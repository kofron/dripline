# This is easy enough to put into a dripline configuration, but for now I'll just do it here

# First some variables for local config
telnet_arg='lagavulin.local 1234'

send_cmd() {
    echo ""
    echo "$1;*ESR?"
    echo "$1;*ESR?" | nc -w 1 $telnet_arg;
}

#Stop scan if in progress
send_cmd "ABOR;*OPC?"

#########################################
#configure 4 wire channels (CONF:FRES AUTO,DEF(@[channels]))
#cernox
send_cmd "CONF:FRES AUTO,DEF,(@102,108);*OPC?"
#linear encoder
send_cmd "CONF:FRES AUTO,DEF,(@301);*OPC?"

#########################################
#configure 2 wire channels (CONF:RES AUTO,DEF,(@[channels]))

#########################################
#configure 100 V measurements (CONF:VOLT:DC 100,(@[channels]))

#########################################
#configure 10 V measurements (CONF:VOLT:DC 10,(@[channels]))
send_cmd "CONF:VOLT:DC 10,(@302,312);*OPC?"

#########################################
#configure mV measurements (CONF:VOLT:DC 0.1,(@[channels]))
send_cmd "CONF:VOLT:DC 0.1,(@106);*OPC?"

#########################################
#configure PT100 (CONF:TEMP RTD,85 (@[channels]))
send_cmd "CONF:TEMP RTD,85,(@304,305,314,315);*OPC?"
#configure PT100 4-wire
send_cmd "CONF:TEMP FRTD,85,(@101,103);*OPC?"

#########################################
#setup the scan list <anything configured above must be added to the list here>
#   (ROUT:SCAN (@[channels]))
#send_cmd "ROUT:SCAN (@);*OPC?"
send_cmd "ROUT:SCAN (@101,102,103,106,108,301,302,304,305,312,314,315);*OPC?"

#start scanning
send_cmd "FORM:READ:CHAN ON;*OPC?"
send_cmd "FORM:READ:TIME ON;*OPC?"
send_cmd "FORM:READ:TIME:TYPE ABS;*OPC?"
send_cmd "FORM:READ:UNIT ON;*OPC?"
send_cmd "TRIG:COUN INF;*OPC?"
send_cmd "TRIG:SOUR TIM;*OPC?"
send_cmd "TRIG:TIM 30;*OPC?"
send_cmd "INIT"
