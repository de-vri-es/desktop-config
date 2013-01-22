{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DynamicScreenLog
-- Copyright   :  Maarten de Vries <maarten@de-vri.es>
-- License     :  GPLv3
--
-- Maintainer  :  Maarten de Vries <maarten@de-vri.es>
-- Stability   :  unstable
-- Portability :  unportable
--
-- xmonad calls the logHook with every internal state update, which is
-- useful for (among other things) outputting status information to an
-- external status bar program such as xmobar or dzen.  DynamicLog
-- provides several drop-in logHooks for this purpose, as well as
-- flexible tools for specifying your own formatting.
--
-----------------------------------------------------------------------------

module DynamicScreenLog (
	WorkspaceFormatter,
	LayoutFormatter,
	TitleFormatter,
	formatWorkspaces,
	formatLayout,
	formatTitle,
	formatMultiple
) where

import Codec.Binary.UTF8.String(encodeString)
import Control.Monad(liftM2)
import Data.Ord(Ordering)
import Data.Maybe(isJust, catMaybes, listToMaybe)
import Data.List(intersperse, find, sortBy)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook(readUrgents)
import XMonad hiding (workspaces)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows(getName)
import XMonad.Util.WorkspaceCompare(WorkspaceSort)


type WorkspaceFormatter
	=  Maybe (ScreenId) -- The ID of the screen.
	-> Bool -- ^ Indicates if the workspace has the focus.
	-> Bool -- ^ Indicates if the workspace is visible on the context screen.
	-> Bool -- ^ Indicates if the workspace is visible on any screen.
	-> Bool -- ^ Indicates if the workspace contains any urgent windows.
	-> Bool -- ^ Indicates if the workspace contains any windows.
	-> String -- ^ The workspace tag.
	-> String -- ^ The formatted workspace tag.

type Formatter = ScreenId -> X String

type LayoutFormatter
	= ScreenId
	->  Bool   -- ^ Indicates if the workspace has the focus.
	-> String -- ^ The description of the workspace layout.
	-> String -- ^ The formatted layout description.

type TitleFormatter
	= ScreenId
	->  Bool   -- ^ Indicates if the workspace of the window has the focus.
	-> String -- ^ The title of the window window.
	-> String -- ^ The formatted window title.

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep . filter (not . null)

-- | Get the list of screens from a StackSet.
screens :: W.StackSet i l a sid sd -> [W.Screen i l a sid sd]
screens stackset = W.current stackset : W.visible stackset

-- | Get the list of workspaces from a StackSet.
workspaces :: WindowSet -> [W.Workspace WorkspaceId (Layout Window) Window]
workspaces stackset = (map (W.workspace) (screens stackset)) ++ (W.hidden stackset)

-- | Find a screen with the given screen ID in the window set.
findScreen :: Eq(sid) => sid -> W.StackSet i l a sid sd -> Maybe (W.Screen i l a sid sd)
findScreen screen_id windowset = find (\screen -> W.screen screen == screen_id) $ screens windowset

-- | Find the screen for a given workspace.
findWorkspaceScreen :: Eq(i) => i -> W.StackSet i l a sid sd -> Maybe (W.Screen i l a sid sd)
findWorkspaceScreen tag windowset = find (\screen -> (W.tag . W.workspace) screen == tag) $ screens windowset


-- | Format the workspace list with a specific screen as context.
formatWorkspaces
	:: WorkspaceFormatter       -- ^ The formatting function for workspaces.
	-> String                   -- ^ Seperator to place between workspaces.
	-> WorkspaceSort            -- ^ The compare function to sort the workspaces by.
	-> ScreenId                 -- ^ The context screen ID.
	-> X String                 -- ^ The formatted workspace list.

formatWorkspaces format seperator sort screen_id = do
	windowset <- gets windowset
	urgents <- readUrgents
	let screen = findScreen screen_id windowset
	return $ sepBy seperator $ map (doFormat screen windowset urgents) (sort $ workspaces windowset)
	where
		doFormat Nothing _ _ _ = ""
		doFormat (Just screen) windowset urgents workspace = format workspaceScreen hasFocus isCurrent isVisible hasUrgents hasWindows (W.tag workspace)
			where
				workspaceScreen = fmap (\x -> W.screen x) $ findWorkspaceScreen (W.tag workspace) windowset
				hasFocus        = screen_id == (W.screen . W.current $ windowset)
				isCurrent       = (W.tag $ W.workspace screen) == W.tag workspace
				isVisible       = W.tag workspace `elem` (map (W.tag . W.workspace) (screens windowset))
				hasUrgents      = any (\x -> maybe False (== W.tag workspace) (W.findTag x windowset)) urgents
				hasWindows      = isJust (W.stack workspace)

-- | Format the layout of the workspace of the context screen.
formatLayout
	:: LayoutFormatter          -- ^ The formatting function for the layout description.
	-> ScreenId                 -- ^ The context screen ID.
	-> X String                 -- ^ The formatted layout.

formatLayout format screen_id = do
	windowset <- gets windowset
	let workspace = fmap W.workspace $ findScreen screen_id windowset
	let hasFocus = screen_id == (W.screen . W.current $ windowset)
	let layout = maybe "" (description . W.layout) workspace
	return $ format screen_id hasFocus layout

-- | Format the title of the focussed window on the context screen.
formatTitle
	:: TitleFormatter           -- ^ The formatting function for the window title.
	-> ScreenId                 -- ^ The context screen ID.
	-> X String                 -- ^ The formatted workspace list.

formatTitle format screen_id = do
	windowset <- gets windowset
	let workspace = fmap W.workspace $ findScreen screen_id windowset
	let hasFocus = screen_id == (W.screen . W.current $ windowset)
	title <- maybe (return "") (fmap show . getName . W.focus) $ maybe Nothing W.stack workspace
	return $ format screen_id hasFocus title

-- | Run multiple formatters and combine the output.
formatMultiple
	:: String                   -- ^ String to seperate the output of the formatters with.
	-> [X String]               -- ^ The formatters to combine.
	-> X String                 -- ^ The output of the formatters seperated by the given string.

formatMultiple seperator formatters = do
	output <- sequence $ formatters
	return $ sepBy seperator output
