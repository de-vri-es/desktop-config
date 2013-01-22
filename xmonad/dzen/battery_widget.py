import os


# Known battery APIs.
_apis = [
	["charge_now", "charge_full"],
	["energy_now", "energy_full"]
]

class BatteryWidget:
	
	def __init__(self, ac, battery):
		self.ac         = ac;
		self.battery    = battery;
		self.ac_present = os.path.exists(self.ac);
		self.present    = os.path.exists(self.battery);
		self.message    = "";
		self.high_color     = "#009900";
		self.mid_color      = "#aa7700";
		self.low_color      = "#990000";
		self.charging_color = "#4455ff";
		self.empty_color          = "#000000";
		self.empty_color_charging = "#111133"
		self.bar_width      = 8;
		self.bar_height     = 16;
		
		# Test supported APIs.
		self.api = None
		if self.present:
			for api in _apis:
				if (self.__check_api(api)):
					self.api = api;
					break;
	
	def __check_api(self, api):
		for file in api:
			if not os.path.exists(self.battery + "/" + file):
				return False;
		return True;
	
	def __read_param(self, base, param):
		with open(base + "/" + param) as file:
			return file.read()[:-1];
	
	# Check if the AC adapter is connected.
	def ac_connected(self):
		return self.ac_present and self.__read_param(self.ac, "online") == "1";
	
	# Get the charge of the battery.
	def charge(self):
		if self.api == None:
			return 0.0;
		charge = int(self.__read_param(self.battery, self.api[0])[:-1]);
		full   = int(self.__read_param(self.battery, self.api[1])[:-1]);
		return min(float(charge) / full, 1.0);
	
	# Update the widget mesage.
	def update(self):
		if not self.present: return "";
		
		charging = self.ac_connected();
		charge   = self.charge();
		fill     = int(round(self.bar_width * charge));
		
		color = self.low_color;
		if charging:
			color = self.charging_color;
		elif charge > 0.7:
			color = self.high_color;
		elif charge > 0.2:
			color = self.mid_color;
		else:
			color = self.low_color;
		
		self.message   = "^fg()";
		self.message  += "^ba(70, _RIGHT)"
		self.message  += "%02.0f%% " % (charge * 100);
		self.message  += "^ba()";
		self.message  += "^fg(%s)"   % (self.empty_color_charging if charging else self.empty_color);
		self.message  += "^r(%dx%d)" % (self.bar_width, self.bar_height);
		self.message  += "^p(%s)"    % -self.bar_width;
		self.message  += "^fg(%s)"   % color;
		self.message  += "^ib(1)";
		self.message  += "^r(%dx%d)" % (self.bar_width, self.bar_height * charge);
		self.message  += "^ib(0)";
		self.message  += "  ";
		return self.message;
		
