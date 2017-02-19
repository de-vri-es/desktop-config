#!/usr/bin/python3

import sys
import subprocess
import threading
import battery_widget
import xmonad_widget
import clock_widget


clock_offset   = 160
status_offset  = clock_offset + 85
trayer_offset  = status_offset


font = "sans-8"

screen = int(sys.argv[1]) + 1 if len(sys.argv) >= 2 else 1

# Create and configure widgets.
xmonad         = xmonad_widget.XMonadWidget(sys.stdin)
bat0           = battery_widget.BatteryWidget("/sys/class/power_supply/AC0", "/sys/class/power_supply/BAT0")
clock          = clock_widget.ClockWidget("%a %b %d %Y %H:%M:%S")
clock.fg_color = "#ffaa00"

if not bat0.present:
	trayer_offset = clock_offset


# Start dzen2.
dzen_arguments = [
	'dzen2',
	'-dock',
#	"-w", str(screen_width),
	'-h',  '16',
	'-xs', str(screen),
	'-ta', 'l',
	'-fg', '#dddddd',
	'-bg', "black",
	'-fn', font
]
dzen2 = subprocess.Popen(dzen_arguments, stdin=subprocess.PIPE)

# Start trayer half a second later.
trayer = None
timer = None
if screen == 1:
	def start_trayer():
		global trayer
		trayer_arguments = [
			"stalonetray",
			#"-bg",            "#000000",
			"-i",             "16",
			"--geometry",     "1x1-" + str(trayer_offset) + "+0",
			"--grow-gravity", "NE",
			"--transparent",
			"--kludges", "force_icons_size",
		]
		trayer = subprocess.Popen(trayer_arguments)

	subprocess.call(["killall", "stalonetray"])
	timer = threading.Timer(0.5, start_trayer)
	timer.start()


# Main update loop.
while True:
	xmonad.read(1)
	if xmonad.closed:
		print("Input stream closed.")
		break
	
	bat0.update()
	clock.update()
	
	message = xmonad.message
	message += "^p(_RIGHT)^p({})".format(-status_offset)
	message += bat0.message
	message += "^p(_RIGHT)^p({})".format(-clock_offset)
	message += clock.message
	
	#print(message)
	dzen2.stdin.write(bytes(message + "\n", "UTF-8"))
	dzen2.stdin.flush()

# Kill all spawned processes.
dzen2.terminate()
if (timer  != None): timer.stop()
if (trayer != None): trayer.terminate()
