import os
from math import ceil, floor

# Known battery APIs.
_apis = [
	["charge_now", "charge_full"],
	["energy_now", "energy_full"],
]

class BatteryWidget:
	def __init__(self, ac, battery, height):
		self.ac                   = ac;
		self.battery              = battery;
		self.message              = "";
		self.high_color           = "#009900";
		self.mid_color            = "#aa7700";
		self.low_color            = "#990000";
		self.charging_color       = "#4455ff";
		self.empty_color          = "#000000";
		self.empty_color_charging = "#111133"
		self.bar_height           = height;

		# Test supported APIs.
		self.api = None
		if self.battery is not None:
			for api in _apis:
				if (self.__check_api(api)):
					self.api = api;
					break;

		self.status_api = None;
		if self.__check_api("status"):
			self.status_api = "status"

	def __check_api(self, api):
		for file in api:
			if not os.path.exists(self.battery + "/" + file):
				return False;
		return True;

	def __read_param(self, base, param):
		with open(base + "/" + param) as file:
			return file.read()[:-1];

	def __read_int(self, base, param):
		param = self.__read_param(base, param)
		if param == '':
			return 0
		return int(param)

	def present(self):
		return self.battery is not None

	# Check if the battery is discharging.
	def is_discharging(self):
		if self.status_api is not None:
			status = self.__read_param(self.battery, self.status_api)
			return status != "Full" and status != "Charging"
		elif self.ac is not None:
			return self.__read_param(self.ac, "online") == "0"

	# Get the charge of the battery.
	def charge(self):
		if self.api == None:
			return 0.0;
		charge = self.__read_int(self.battery, self.api[0]);
		full   = self.__read_int(self.battery, self.api[1]);
		return min(float(charge) / full, 1.0);

	# Update the widget mesage.
	def update(self):
		if not self.battery: return "";

		charging = not self.is_discharging();
		charge   = self.charge();
		width    = int(round(self.bar_height / 3));
		height   = int(floor(self.bar_height * 0.80));
		fill     = int(round(height * charge));

		color = self.low_color;
		if charging:
			color = self.charging_color;
		elif charge > 0.7:
			color = self.high_color;
		elif charge > 0.2:
			color = self.mid_color;
		else:
			color = self.low_color;

		self.message   = f"^fg()"
		self.message  += f"^ba(70, _RIGHT)"
		self.message  += f"{charge * 100:02.0f}% "
		self.message  += f"^ba()"
		self.message  += f"^fg({self.empty_color_charging})"
		self.message  += f"^r({width}x{height})"
		self.message  += f"^p(-{width})"
		self.message  += f"^fg({color})"
		self.message  += f"^ib(1)"
		self.message  += f"^r({width}x{fill})"
		self.message  += f"^ib(0)"
		self.message  += f"  "
		return self.message;

