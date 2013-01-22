-----------------------------------------------------------------------------
-- |
-- Module      :  DynamicScreenBars
-- Copyright   :  (c) Maarten de Vries 2014
-- License     :  GPLv3
--
-- Maintainer  :  maarten@de-vri.es
-- Stability   :  unstable
-- Portability :  unportable
--
-- Manage per-screen status bars.
--
-----------------------------------------------------------------------------

module DynamicScreenBars (
	CreateScreenBar,
	DestroyScreenBar,
	dynamicScreenBarStartupHook,
	dynamicScreenBarEventHook,
	dynamicScreenBarLogHook,
) where

import Codec.Binary.UTF8.String (encodeString)
import Control.Concurrent.MVar
import Control.Monad

import Data.Maybe
import Data.Monoid

import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xrandr (xrrSelectInput)

import System.IO
import System.IO.Error
import System.IO.Unsafe

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog (PP)

data ScreenBarInfo = ScreenBarInfo {
	screen     :: ScreenId,
	pipe       :: Handle,
	formatter  :: X String
}

type CreateScreenBar  = ScreenId      -> X [(Handle, X String)]
type DestroyScreenBar = ScreenBarInfo -> X ()

-- Global list of screen bars.
screenBarInfo :: MVar [ScreenBarInfo]
screenBarInfo = unsafePerformIO $ newMVar []

-- Log hook to update all spawned bars.
dynamicScreenBarLogHook :: X ()
dynamicScreenBarLogHook = do
	barList <- io $ readMVar screenBarInfo
	barList <- filterM updateBar barList
	io $ swapMVar screenBarInfo barList
	return ()
	where updateBar bar = do
		output <- formatter bar
		result <- io $ tryIOError $ hPutStrLn (pipe bar) $ encodeString output
		result <-case result of
			Left error -> do
				io $ hClose (pipe bar)
				return False
			_ -> return True
		return result

-- Startup hook to set up the bars and enable screen change notifications.
dynamicScreenBarStartupHook :: CreateScreenBar -> DestroyScreenBar -> X ()
dynamicScreenBarStartupHook create destroy = do
	dpy <- asks display
	io $ xrrSelectInput dpy (defaultRootWindow dpy) rrScreenChangeNotifyMask
	updateScreenBars create destroy

-- Event hook to update all bars when the screen layout changes.
dynamicScreenBarEventHook :: CreateScreenBar -> DestroyScreenBar -> Event -> X All
dynamicScreenBarEventHook create destroy (RRScreenChangeNotifyEvent {}) = do
	updateScreenBars create destroy
	return (All True)
dynamicScreenBarEventHook _  _  _ = return (All True)

-- Spawn all bars for a screen and return twe spawned bars in a list.
spawnBar :: CreateScreenBar -> ScreenId -> X [(ScreenBarInfo)]
spawnBar create screen = do
	screen_bars <- create screen
	return $ map toScreenBarInfo screen_bars
	where toScreenBarInfo (handle, formatter) = ScreenBarInfo screen handle formatter

-- Remove existing bars and spawn bars for all screens.
updateScreenBars :: CreateScreenBar -> DestroyScreenBar -> X ()
updateScreenBars create destroy = do
	barList <- io $ takeMVar screenBarInfo
	screens <- getScreens
	mapM destroy barList
	io $ mapM (hClose . pipe) barList
	newInfo <- mapM (spawnBar create) screens
	io $ putMVar screenBarInfo $ concat newInfo

-- Get all screens of the current display.
getScreens :: X [ScreenId]
getScreens = do
	dpy     <- asks display
	screens <- io $ getScreenInfo dpy
	return $ map fst $ zip [0 .. ] screens
