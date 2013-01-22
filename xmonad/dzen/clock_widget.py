import time

class ClockWidget:
	
	def __init__(self, format):
		self.format   = format;
		self.fg_color = None;
		self.bg_color = None;
		self.message  = "";
		
	def update(self):
		self.message = "";
		if (self.fg_color): self.message += "^fg(" + self.fg_color + ")";
		if (self.bg_color): self.message += "^bg(" + self.bg_color + ")";
		self.message += time.strftime(self.format);
