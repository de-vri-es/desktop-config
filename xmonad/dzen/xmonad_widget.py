import fcntl
import os
import select

class XMonadWidget:
	
	def __init__(self, stream):
		self.message  = "";
		self.stream   = stream;
		self.closed   = False;
		
		self.__buffer = "";
		
		# Set stream to non blocking mode.
		mode = fcntl.fcntl(stream, fcntl.F_GETFL)
		fcntl.fcntl(stream, fcntl.F_SETFL, mode | os.O_NONBLOCK)
	
	def read(self, timeout):
		ready = select.select([self.stream], [], [], timeout)[0];
		if len(ready) > 0:
			# Read available input into buffer.
			try:
				input = self.stream.read();
			except Exception:
				return

			if len(input) == 0:
				self.closed = True;
			self.__buffer = self.__buffer + input;
				
			# Extract messages one by one.
			while True:
				newline = self.__buffer.find("\n");
				if (newline < 0): break;
				self.__process_message(self.__buffer[0:newline]);
				self.__buffer = self.__buffer[newline+1:];
		
	def __process_message(self, message):
		self.message = message;
