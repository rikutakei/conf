import XMonad
import Control.Monad (liftM2)
import XMonad.Config.Kde
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Hooks.InsertPosition
import XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.StackSet as W

main = do
	xmonad $ kde4Config
		{
		  modMask = myModMask
		, terminal = myTerminal
		, focusFollowsMouse = True
		, workspaces = myWorkSpaces
		, manageHook = manageHook kde4Config <+> myManageHook
		, layoutHook = smartBorders . avoidStruts $ layoutHook kde4Config ||| ResizableTall 1 (5/100) (1/2) []
		} `additionalKeys` myAdditionalKeys

myModMask = mod4Mask
myTerminal = "gnome-terminal"

myManageHook = composeAll
	[
	className =? "banshee" --> doShift "=",
	className =? "Chromium" --> viewShift "2:web",
	className =? "dolphin" --> viewShift "3:files",
	className =? "Gnome-terminal" --> doF W.swapDown,
	className =? "plasmashell" --> doFloat, -- for KDE volume popup (and other stuff too)
	manageDocks,
	isFullscreen --> doFullFloat
	]

viewShift = doF . liftM2 (.) W.greedyView W.shift

myWorkSpaces = ["1:default","2:web","3:files","4","5","6","7","8","9","0"] ++ (map snd myExtraWorkSpaces)
myExtraWorkSpaces = [(xK_0, "0"),(xK_minus, "-"),(xK_equal, "=")]
myAdditionalKeys =
	[-- Your other hotkeys
	((myModMask, xK_z), toggleWS),
	((myModMask, xK_c), moveTo Next NonEmptyWS),
	((myModMask, xK_x), moveTo Prev NonEmptyWS),
	((myModMask, xK_d), sendMessage MirrorShrink),
	((myModMask, xK_u), sendMessage MirrorExpand),
	((myModMask .|. shiftMask, xK_BackSpace), kill),
	((myModMask .|. shiftMask, xK_c), moveTo Next EmptyWS),
	((myModMask .|. shiftMask, xK_x), moveTo Prev EmptyWS),
	((myModMask .|. shiftMask, xK_l), shiftToNext >> nextWS),
	((myModMask .|. shiftMask, xK_h), shiftToPrev >> prevWS)
	] ++ [
		( (myModMask, xK_0), (windows $ W.greedyView ws))
		| (xk_0, ws) <- myExtraWorkSpaces
	] ++ [
		( (myModMask .|. shiftMask, xk_0), (windows $ W.shift ws))
		| (xk_0, ws) <- myExtraWorkSpaces
	] ++ [
		( (myModMask, xK_minus), (windows $ W.greedyView ws))
		| (xK_minus, ws) <- myExtraWorkSpaces
	] ++ [
		( (myModMask .|. shiftMask, xK_minus), (windows $ W.shift ws))
		| (xK_minus, ws) <- myExtraWorkSpaces
	] ++ [
		( (myModMask, xK_equal), (windows $ W.greedyView ws))
		| (xK_equal, ws) <- myExtraWorkSpaces
	] ++ [
		( (myModMask .|. shiftMask, xK_equal), (windows $ W.shift ws))
		| (xK_equal, ws) <- myExtraWorkSpaces
	] ++ [
	]
