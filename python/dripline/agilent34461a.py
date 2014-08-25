from sensor import AutoReply
from factory import constructor_registry as reg

import socket
from provider import Provider

class Agilent34461a(Provider):
	def __init__(self, name, ip_addr='10.0.0.60', scpi_port=5025):
		self.name = name
		self.sensors = {}
		self.ip_addr = ip_addr
		self.scpi_port = scpi_port
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.sock.connect(('10.0.0.60',5025))

	def add_endpoint(self, sensor):
		self.sensors[sensor.name] = sensor
		sensor.set_provider(self)

	def endpoint(self, sensorname):
		return self.sensors[sensorname]

	def list_endpoints(self):
		return self.sensors.keys()

	def send_sync(self, to_send):
		self.sock.send(to_send + '\r\n')
		data = self.sock.recv(1024)
		return data

class VoltageInput(AutoReply):
	def __init__(self,name,*args,**kwargs):
		self.name = name

	def on_get(self):
		result = self.provider.send_sync("MEAS:VOLT:DC?")
		return result

	def on_set(self):
		raise NotImplementedError

	def provider(self):
		return self.provider

	def set_provider(self, provider):
		self.provider = provider

reg['agilent34461a'] = Agilent34461a
reg['agilent34461a_voltage_input'] = VoltageInput