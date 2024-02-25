import Data.Monoid
import System.Exit
import System.IO

import Numeric
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.RGBSpace.HSL

import XMonad
import XMonad.Core

import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D

import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

import XMonad.Layout.Grid
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Hooks.DynamicLog
import DynamicScreenLog
import DynamicScreenBars
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Hooks.UrgencyHook as UH

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Graphics.X11.ExtraTypes.XF86

myXpConfig = greenXPConfig {
	font = "xft:sans:size=12",
	height = 36
}

myWorkspaces = ["dev","doc","web", "media","mus","chat", "7", "8","9", "0"]


myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ [
		-- launch a terminal
		((modm, xK_Return), spawn $ XMonad.terminal conf),

		((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5"),
		((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5"),
		((0, xF86XK_AudioRaiseVolume), spawn "volume-control up 5"),
		((0, xF86XK_AudioLowerVolume), spawn "volume-control down 5"),
		((0, xF86XK_AudioMute), spawn "volume-control toggle-mute"),
		((shiftMask, xF86XK_AudioRaiseVolume), spawn "volume-control mic up 5"),
		((shiftMask, xF86XK_AudioLowerVolume), spawn "volume-control mic down 5"),
		((shiftMask, xF86XK_AudioMute), spawn "volume-control mic toggle-mute"),
		((0, xF86XK_AudioMicMute), spawn "volume-control mic toggle-mute"),
		((0, xF86XK_AudioPlay), spawn "playerctl play-pause"),
		((0, xF86XK_AudioPause), spawn "playerctl pause"),
		((0, xF86XK_AudioStop), spawn "playerctl stop"),
		((0, xF86XK_AudioNext), spawn "playerctl next"),
		((0, xF86XK_AudioPrev), spawn "playerctl previous"),

		((controlMask .|. mod1Mask, xK_t), spawn $ XMonad.terminal conf),

		-- launch application
		((modm    , xK_r ), shellPrompt myXpConfig),

		-- close focused window
		((modm    , xK_c ), kill),

		-- Rotate through the available layout algorithms
		((modm, xK_space ), sendMessage NextLayout),

		--  Reset the layouts on the current workspace to default
		((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf),

		-- Resize viewed windows to the correct size
		((modm, xK_n), refresh),

		-- Move focus to the next window
		((modm,                   xK_Tab), windows W.focusDown),
		((mod1Mask,               xK_Tab), windows W.focusDown),
		((mod1Mask .|. shiftMask, xK_Tab), windows W.swapDown),

		-- Start a prompt to bring a window to the front.
		-- ((modm, xK_g), windowPromptGoto myXpConfig),

		-- Move focus to the next window
		((modm, xK_j), windows W.focusDown),

		-- Move focus to the previous window
		((modm, xK_k), windows W.focusUp),

		-- Move focus to the master window
		((modm, xK_m), windows W.focusMaster),

		-- Directional navigation of windows
		((modm, xK_Right), windowGo R True),
		((modm, xK_Left ), windowGo L True),
		((modm, xK_Up   ), windowGo U True),
		((modm, xK_Down ), windowGo D True),

		-- Swap adjacent windows
		((modm .|. shiftMask, xK_Right), windowSwap R True),
		((modm .|. shiftMask, xK_Left ), windowSwap L True),
		((modm .|. shiftMask, xK_Up   ), windowSwap U True),
		((modm .|. shiftMask, xK_Down ), windowSwap D True),

		-- Directional navigation of screens
		((modm .|. controlMask, xK_Right), screenGo R True),
		((modm .|. controlMask, xK_Left ), screenGo L True),
		((modm .|. controlMask, xK_Up   ), screenGo U True),
		((modm .|. controlMask, xK_Down ), screenGo D True),

		-- Send window to adjacent screen
		((modm .|. controlMask .|. shiftMask, xK_Right), windowToScreen R True),
		((modm .|. controlMask .|. shiftMask, xK_Left ), windowToScreen L True),
		((modm .|. controlMask .|. shiftMask, xK_Up   ), windowToScreen U True),
		((modm .|. controlMask .|. shiftMask, xK_Down ), windowToScreen D True),

		-- Swap the focused window and the master window
		((modm .|. shiftMask, xK_m), windows W.swapMaster),

		-- Shrink the master area
		((modm, xK_semicolon), sendMessage Shrink),

		-- Expand the master area
		((modm, xK_quoteright), sendMessage Expand),

		-- Push window back into tiling
		((modm, xK_t), withFocused $ windows . W.sink),

		-- Increment the number of windows in the master area
		((modm, xK_period), sendMessage (IncMasterN (-1))),

		-- Deincrement the number of windows in the master area
		((modm, xK_comma), sendMessage (IncMasterN 1)),

		-- Toggle the status bar gap
		-- Use this binding with avoidStruts from Hooks.ManageDocks.
		-- See also the statusBar function from Hooks.DynamicLog.
		--
		((modm, xK_f), sendMessage ToggleStruts >> sendMessage (Toggle NOBORDERS)),

		-- Lock screen
		((modm, xK_l ), spawn "slock"),

		-- Restart xmonad
		((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
	] ++ [

		--
		-- mod-[1..9], Switch to workspace N
		--
		-- mod-[1..9], Switch to workspace N
		-- mod-shift-[1..9], Move client to workspace N
		((m .|. modm, k), windows $ f i)
			| (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
			, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
	] ++ [
		--
		-- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
		-- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
		((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
			| (key, sc) <- zip [xK_w, xK_e, xK_q] [0..]
			, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
	]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $ [
		-- mod-button1, Set the window to floating mode and move by dragging
		((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)),
		-- mod-button2, Raise the window to the top of the stack
		((modm, button2), (\w -> focus w >> windows W.shiftMaster)),
		-- mod-button3, Set the window to floating mode and resize by dragging
		((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
	]

layoutMods = mkToggle (single NOBORDERS) . avoidStruts . smartBorders

defaultLayout = tiled ||| Mirror tiled ||| grid ||| Full ||| tab where
	-- default tiling algorithm partitions the screen into two panes
	tiled = renamed [Replace "Tall"] mouseResizableTile {draggerType = BordersDragger}
	tab   = tabbed shrinkText theme
	grid  = renamed [Replace "Grid"] $ GridRatio (4/3)
	theme = def {
		activeColor         = "#009900",
		activeTextColor     = "#ffffff",
		activeBorderColor   = "#000000",
		inactiveColor       = "#999999",
		inactiveTextColor   = "#000000",
		inactiveBorderColor = "#000000",
		urgentColor         = "#000000",
		urgentTextColor     = "#ffaa00",
		urgentBorderColor   = "#ffaa00",
		fontName            = "xft:sans-8"
	}

myLayout = layoutMods $ defaultLayout

myManageHook = composeAll [
		className =? "Xfce4-notifyd"   --> doIgnore,
		className =? "Xmessage"        --> doFloat,
		(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat,
		isDialog                       --> doFloat,
		manageDocks
	]

withUrgency = UH.withUrgencyHookC UH.NoUrgencyHook def {UH.suppressWhen = UH.Focused}

barExec = "$HOME/.xmonad/dzen/topbar.py"

screenColor :: ScreenId -> Colour Double
screenColor 0 = toColor $ hsl 120 0.5 0.5
screenColor 1 = toColor $ hsl 30  0.5 0.5
screenColor 2 = toColor $ hsl 210 0.5 0.5
screenColor _ = toColor $ hsl 270 0.5 0.5

white = sRGB 1 1 1

toColor :: (Ord a, Floating a) => RGB a -> Colour a
toColor (RGB r g b) = sRGB r g b

slMultiply color s l = toColor $ hsl (hue rgb) (s * saturation rgb) (l * lightness rgb)
	where
		rgb = toSRGB color

fade       color = slMultiply color 0.8 0.7
discolor   color = slMultiply color 0.6 1.0
highlight  color = slMultiply color 1.5 1.0
background color = slMultiply color 0.7 0.4

myDzenColor fg bg = (++) ("^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")")

workspaceColor screenId hasFocus
	| hasFocus  = dzenColor (sRGB24show        $ white) (sRGB24show        . screenColor $ screenId)
	| otherwise = dzenColor (sRGB24show . fade $ white) (sRGB24show . fade . screenColor $ screenId)

myFormatWorkspace screenId hasFocus isCurrent isVisible hasUrgents hasWindows
	| hasUrgents                    = dzenColor "#ffffff" "#990000" . wrap " " " "
	| Just id <- screenId, hasFocus = workspaceColor id hasFocus . wrap " " " "
	| Just id <- screenId           = workspaceColor id hasFocus . wrap " " " "
	| hasWindows && hasFocus        = dzenColor "#000000" "#999999" . wrap " " " "
	| hasWindows                    = dzenColor "#000000" "#666666" . wrap " " " "
	| hasFocus                      = dzenColor "#ffffff" "#444444" . wrap " " " "
	| otherwise                     = dzenColor "#bbbbbb" "#333333" . wrap " " " "

myFormatLayout screenId hasFocus
	| hasFocus  = myDzenColor "#ffaa00" (sRGB24show . background . screenColor $ screenId) . (\x -> x ++ "^r(10000x0)^p(-10000)") . wrap "   " "   "
	| otherwise = myDzenColor "#b0b0b0" (sRGB24show . background . screenColor $ screenId) . (\x -> x ++ "^r(10000x0)^p(-10000)") . wrap "   " "   "

myFormatTitle screenId hasFocus
	| hasFocus  = myDzenColor (sRGB24show . highlight . screenColor $ screenId) (sRGB24show . background . screenColor $ screenId) . wrap " " " "
	| otherwise = myDzenColor "#bbbbbb"                             (sRGB24show . background . screenColor $ screenId) . wrap " " " "

format :: ScreenId -> X (X String)
format screen = do
	sort <- getSortByIndex
	return $ formatMultiple "" [
			formatWorkspaces myFormatWorkspace "" sort screen,
			formatLayout     myFormatLayout       screen,
			formatTitle      myFormatTitle        screen
		];

createBar :: ScreenId -> X [(Handle, X String)]
createBar (S screen) = do
	return []
	pipe <- spawnPipe $ barExec ++ " " ++ (show screen)
	format <- format $ S screen
	return [(pipe, format)]

destroyBar _ = return ()

main = do
	xmonad $ withUrgency $ docks $ ewmh def {
		-- simple stuff
		terminal           = "alacritty",
		focusFollowsMouse  = False,
		clickJustFocuses   = False,
		borderWidth        = 1,
		modMask            = mod4Mask,
		workspaces         = myWorkspaces,
		normalBorderColor  = "#555555",
		focusedBorderColor = "#008800",

		-- key bindings
		keys               = myKeys,
		mouseBindings      = myMouseBindings,

		-- hooks, layouts
		layoutHook         = myLayout,
		manageHook         = myManageHook,
		handleEventHook    = dynamicScreenBarEventHook createBar destroyBar,
		logHook            = dynamicScreenBarLogHook,
		startupHook        = dynamicScreenBarStartupHook createBar destroyBar
	}
