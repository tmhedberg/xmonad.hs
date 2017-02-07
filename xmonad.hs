{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Applicative ((<*>), pure)

import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S

import System.Exit (exitSuccess)

import XMonad hiding (focus)
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.StackSet hiding (workspaces)
import XMonad.Util.EZConfig

-- Shell commands
cmd_browser = "exec google-chrome"
cmd_lockScreen = "slock"
cmd_volDown = "amixer set Master 1- unmute; amixer -c 1 set Speaker 1- unmute"
cmd_volUp = "amixer set Master 1+ unmute; amixer -c 1 set Speaker 1+ unmute"
cmd_volMute = "amixer set Master toggle"

-- Key codes
keyCode_volDown = 0x1008ff11
keyCode_volUp = 0x1008ff13
keyCode_volMute = 0x1008ff12
keyCode_suspend = 0x1008ff2f

-- Color definitions
colorDef_white = "#ffffff"
colorDef_darkGray = "#888888"
colorDef_midGray = "#111111"

-- Color assignments
color_focusedBorder = colorDef_darkGray
color_normalBorder = colorDef_midGray

-- Additional workspaces & associated hotkeys
addWorkspaces = [("0", xK_0)] ++ map fKeyWorkspace [1..12]
    where fKeyWorkspace n = ('F' : show n, xK_F1 + (n - 1))

-- List of X11 window classes which should never steal focus
noStealFocusWins = []

-- Misc constants
my_terminal = "urxvt"
my_modKey = mod4Mask

-- General configuration
myConfig = ewmh $ withUrgencyHook NoUrgencyHook defaultConfig
    { terminal = my_terminal
    , modMask = my_modKey
    , focusedBorderColor = color_focusedBorder
    , normalBorderColor = color_normalBorder
    , manageHook = floatNextHook
               <+> manageDocks
               <+> (isFullscreen --> doFullFloat)
               <+> composeAll
                    [className =? c --> doF focusDown | c <- noStealFocusWins]
               <+> manageHook defaultConfig
    , layoutHook = configurableNavigation noNavigateBorders $ smartBorders $
        avoidStruts myLayouts
    , workspaces = map show [1 .. 9 :: Int] ++ map fst addWorkspaces
    } `additionalKeys` myKeys

-- Custom key bindings
myKeys = [ ((my_modKey .|. shiftMask, xK_l), spawn cmd_lockScreen)
         , ((0, keyCode_volDown), spawn cmd_volDown)
         , ((0, keyCode_volUp), spawn cmd_volUp)
         , ((0, keyCode_volMute), spawn cmd_volMute)
         , ( (my_modKey, xK_backslash)
           , withFocused (sendMessage . maximizeRestore)
           )
         , ((my_modKey, xK_Up), sendMessage $ Go U)
         , ((my_modKey, xK_Down), sendMessage $ Go D)
         , ((my_modKey, xK_Left), sendMessage $ Go L)
         , ((my_modKey, xK_Right), sendMessage $ Go R)
         , ((my_modKey .|. shiftMask, xK_Up), sendMessage $ Swap U)
         , ((my_modKey .|. shiftMask, xK_Down), sendMessage $ Swap D)
         , ((my_modKey .|. shiftMask, xK_Left), sendMessage $ Swap L)
         , ((my_modKey .|. shiftMask, xK_Right), sendMessage $ Swap R)
         , ((my_modKey, xK_g), goToSelected myGridSelectConfig)
         , ((my_modKey .|. shiftMask, xK_backslash), spawn cmd_browser)
         , ((my_modKey, xK_f), toggleFloatNext >> runLogHook)
         , ((my_modKey, xK_d), toggleFloatAllNew >> runLogHook)
         , ((my_modKey, xK_a), sendMessage MirrorExpand)
         , ((my_modKey, xK_z), sendMessage MirrorShrink)
         , ((my_modKey .|. shiftMask, xK_q), return ()) -- Unbind default "exit xmonad" chord
         , ((my_modKey .|. shiftMask .|. mod1Mask, xK_q), io exitSuccess)   -- Exit with <Mod+Shift+Alt+Q>
         ] ++
         [((my_modKey .|. m, k), windows $ f ws)
            | (m, f) <- [(0, greedyView), (shiftMask, shift)]
            , (ws, k) <- addWorkspaces]

-- Status bar configuration
myStatusBar = statusBar ("dzen2 " ++ flags) dzenPP' $ const (my_modKey, xK_b)
  where
    fg = "white"  -- Default: #a8a3f7
    bg = "black"  -- Default: #3f3c6d
    flags = "-e 'onstart=lower' -ta l -fg "
         ++ fg
         ++ " -bg "
         ++ bg
         ++ " -fn 'DejaVu Sans Mono:pixelsize=10'"
    dzenPP' =
      let lt = colorDef_white
          md = colorDef_darkGray
          dk = "black"
      in
        defaultPP
          { ppCurrent = dzenColor dk lt . pad
          , ppVisible = dzenColor dk md . pad
          , ppHidden = dzenColor lt dk . pad
          , ppHiddenNoWindows = const ""
          , ppUrgent = dzenColor "blue" "red" . pad
          , ppWsSep = ""
          , ppSep = ""
          , ppLayout = dzenColor dk md . \x -> pad $ case x of
              "Maximize ResizableTall" -> "MRT"
              "Tabbed Simplest" -> "TAB"
              "SimplestFloat" -> "FLT"
              "ThreeCol" -> "3CL"
              _ -> x
          , ppTitle = dzenColor lt dk . pad . dzenEscape
          , ppExtras =
              (fmap (dzenColor dk md . pad . dzenEscape)
                <$> focusedWindowFloatingIndicator)
                : ([willFloatNextPP, willFloatAllNewPP]
                    <*> pure
                          (dzenColor dk md
                            . pad
                            . \s -> case s of "All" -> "*"
                                              "Next" -> "+"))
          }

-- Workspace layouts
myLayouts = maximize (ResizableTall 1 (3 / 100) (1 / 2) [])
        ||| simpleTabbed
        ||| simplestFloat
        ||| ThreeCol 1 (3 / 100) (1 / 2)

-- GridSelect configuration
myGridSelectConfig :: HasColorizer a => GSConfig a
myGridSelectConfig = defaultGSConfig {gs_navigate = myNavigation}
  where
    myNavigation = makeXEventhandler $ shadowWithKeymap navKeymap $
      const defaultNavigation
    navKeymap =
      M.fromList [ ((0, xK_Escape), cancel)
                 , ((0, xK_Return), select)
                 , ((0, xK_slash), substringSearch myNavigation)
                 , ((0, xK_h), move (-1, 0) >> myNavigation)
                 , ((0, xK_l), move (1, 0) >> myNavigation)
                 , ((0, xK_j), move (0, 1) >> myNavigation)
                 , ((0, xK_k), move (0, -1) >> myNavigation)
                 , ((0, xK_y), move (-1, -1) >> myNavigation)
                 , ((0, xK_u), move (1, -1) >> myNavigation)
                 , ((0, xK_b), move (-1, 1) >> myNavigation)
                 , ((0, xK_n), move (1, 1) >> myNavigation)
                 , ((0, xK_Tab), moveNext >> myNavigation)
                 ]

main = xmonad =<< myStatusBar myConfig

focusedWindow :: X (Maybe Window)
focusedWindow = (fmap focus) . stack . workspace . current . windowset <$> get

focusedWindowFloatingIndicator :: X (Maybe String)
focusedWindowFloatingIndicator = do
  xstate <- get
  let
    indicateFloating win
      | win `S.member` M.keysSet (floating $ windowset xstate) = Just "^"
      | otherwise = Nothing
  maybe Nothing indicateFloating <$> focusedWindow
