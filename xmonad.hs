{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

import Control.Applicative ((<*>), pure)
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import System.Exit (exitSuccess)

import XMonad hiding (focus)
import XMonad.Actions.GridSelect
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import qualified XMonad.Operations as Op
import XMonad.StackSet hiding (workspaces)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig
import qualified XMonad.Util.NamedScratchpad as NS
import XMonad.Util.Run

-- Shell commands
cmd_browser = "exec google-chrome"
cmd_lockScreen = "xscreensaver-command -lock"
cmd_volDown = "amixer set Master 1%- unmute; amixer -c 1 set Speaker 1- unmute"
cmd_volUp = "amixer set Master 1%+ unmute; amixer -c 1 set Speaker 1+ unmute"
cmd_volMute = "for ctrl in Master Headphone Speaker PCM; do amixer set $ctrl toggle; done"
cmd_inactiveDim = "compton-dimming"

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
noStealFocusWins = [ "crx_hmjkmjkepdijhoojdojkdfohbdgmmhki" -- Google Keep Chrome app
                   , "crx_knipolnnllmklapflnccelgolnpehhpl" -- Hangouts Chrome app
                   ]

-- Misc constants
my_terminal = "urxvt"
my_modKey = mod4Mask

-- Scratchpads
scratchpads =
  [ NS.NS "Scratch"
          "urxvt -title Scratch"
          (title =? "Scratch")
          NS.defaultFloating
  , NS.NS "Notes"
          "urxvt -title Notes -geometry 123x34 -e screen -x -p Notes"
          (title =? "Notes")
          NS.defaultFloating
  ]

allWorkspacesKeys :: [(WorkspaceId, KeySym)]
allWorkspacesKeys = zip (map show [1 .. 9 :: Int] ++ map fst addWorkspaces)
                        (map (xK_0+) [1 .. 9] ++ map snd addWorkspaces)

-- General configuration
myConfig = docks $ ewmh $ withUrgencyHook NoUrgencyHook def
    { terminal = my_terminal
    , modMask = my_modKey
    , focusedBorderColor = color_focusedBorder
    , normalBorderColor = color_normalBorder
    , manageHook = floatNextHook
               <+> manageDocks
               <+> (isFullscreen --> doFullFloat)
               <+> composeAll
                    [className =? c --> doF focusDown | c <- noStealFocusWins]
               <+> NS.namedScratchpadManageHook scratchpads
               <+> manageHook def
    , layoutHook = configurableNavigation noNavigateBorders $ smartBorders $
        avoidStruts myLayouts
    , handleEventHook = fullscreenEventHook
    , workspaces = map fst allWorkspacesKeys
    } `additionalKeys` myKeys

-- Custom key bindings
myKeys =
  [ ((my_modKey .|. shiftMask, xK_l), spawn cmd_lockScreen)
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
  , ((my_modKey, xK_s), NS.namedScratchpadAction scratchpads "Scratch")
  , ( (my_modKey .|. shiftMask, xK_s)
    , NS.namedScratchpadAction scratchpads "Notes"
    )
  , ((my_modKey, xK_x), renameWorkspace def)
  , ((my_modKey, xK_v), spawn cmd_inactiveDim)
  , ((my_modKey, xK_i), makePIPWin)
  , ((my_modKey .|. shiftMask, xK_i), clearPIPWin)
  , ((my_modKey .|. shiftMask, xK_q), return ()) -- Unbind default "exit xmonad" chord
  , ((my_modKey .|. shiftMask .|. mod1Mask, xK_q), io exitSuccess)   -- Exit with <Mod+Shift+Alt+Q>
  ]
    ++ [((my_modKey, k), toggleWorkspace ws) | (ws, k) <- allWorkspacesKeys]
    ++ [ ((my_modKey .|. shiftMask, k), windows $ shift ws)
       | (ws, k) <- allWorkspacesKeys
       ]

newtype PreviousWorkspace = PreviousWorkspace WorkspaceId
  deriving (Read, Show, Typeable)

instance ExtensionClass PreviousWorkspace where
  initialValue = PreviousWorkspace "1"
  extensionType = PersistentExtension

newtype PIPWindow = PIPWindow (Maybe Window) deriving (Read, Show, Typeable)

instance ExtensionClass PIPWindow where
  initialValue = PIPWindow Nothing
  extensionType = PersistentExtension

-- | Make the focused window become the PIP (picture-in-picture) window
--
-- This floats the window and makes it follow the current workspace.
makePIPWin :: X ()
makePIPWin = withFocused $ \win ->
  clearPIPWin >> Op.float win >> XS.put (PIPWindow $ Just win)

-- | Clear the PIP window
clearPIPWin :: X ()
clearPIPWin = XS.put $ PIPWindow Nothing

-- | Switch to a workspace, or if it's already selected, switch back to the
-- previous one
toggleWorkspace :: WorkspaceId -> X ()
toggleWorkspace wsid = do
  PreviousWorkspace prevWSID <- XS.get
  xstate <- get
  let curWSID = tag $ workspace $ current $ windowset xstate
      nextWSID = if curWSID == wsid then prevWSID else wsid

  -- Move the PIP window, if one exists and is on the current workspace, to the
  -- destination workspace
  PIPWindow mPIPWin <- XS.get
  let
    movePIPWin win = windows $ \ss ->
      maybe
        ss
        (\pipWinWSID ->
          if pipWinWSID == curWSID then shiftWin nextWSID win ss else ss)
        (findTag win ss)
  maybe (return ()) movePIPWin mPIPWin

  XS.put $ PreviousWorkspace curWSID
  windows $ greedyView nextWSID

-- statusBar implementation copied from XMonad.Hooks.DynamicLog source code,
-- modified to show workspace names
statusBar' :: LayoutClass l Window
           => String    -- ^ the command line to launch the status bar
           -> PP        -- ^ the pretty printing options
           -> (XConfig Layout -> (KeyMask, KeySym))
                        -- ^ the desired key binding to toggle bar visibility
           -> XConfig l -- ^ the base config
           -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar' cmd pp k conf = do
    h <- spawnPipe cmd
    return $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
                        logHook conf
                        pp' <- workspaceNamesPP pp
                        dynamicLogWithPP pp' { ppOutput = hPutStrLn h }
        , manageHook = manageHook conf <+> manageDocks
        , keys       = liftM2 M.union keys' (keys conf)
        }
 where
    keys' = (`M.singleton` sendMessage ToggleStruts) . k

-- Status bar configuration
myStatusBar = statusBar' ("dzen2 " ++ flags) dzenPP' $ const (my_modKey, xK_b)
  where
    fg = "white"  -- Default: #a8a3f7
    bg = "black"  -- Default: #3f3c6d
    flags = "-dock -e 'onstart=lower' -w 2335 -ta l -fg "
         ++ fg
         ++ " -bg "
         ++ bg
         ++ " -fn 'DejaVu Sans Mono:pixelsize=10'"
    dzenPP' =
      let lt = colorDef_white
          md = colorDef_darkGray
          dk = "black"
      in
        NS.namedScratchpadFilterOutWorkspacePP $ def
          { ppCurrent = dzenColor dk lt . pad
          , ppVisible = dzenColor dk md . pad
          , ppHidden = dzenColor lt dk . pad
          , ppHiddenNoWindows = const ""
          , ppUrgent = dzenColor "blue" "red" . pad
          , ppWsSep = ""
          , ppSep = ""
          , ppLayout = dzenColor dk md . \x -> pad $ case x of
              "Maximize ResizableTall" -> "MRT"
              "ReflectX Maximize ResizableTall" -> "TRM"
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
                                              "Next" -> "+"
                                              _ -> "?"))
          }

-- Workspace layouts
myLayouts = maximize (ResizableTall 1 (3 / 100) (1 / 2) [])
        ||| reflectHoriz (maximize (ResizableTall 1 (3 / 100) (1 / 2) []))
        ||| simpleTabbed
        ||| simplestFloat
        ||| ThreeCol 1 (3 / 100) (1 / 2)

-- GridSelect configuration
myGridSelectConfig :: GSConfig Window
myGridSelectConfig = def { gs_navigate = myNavigation
                         , gs_colorizer = fromClassName
                         }
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
