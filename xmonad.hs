import Control.Applicative ((<*>), pure)

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.StackSet hiding (workspaces)
import XMonad.Util.EZConfig

-- Shell commands
cmd_browser = "exec firefox || ([ $? -eq 127 ] && exec chromium)"
cmd_lockScreen = "slock"
cmd_volDown = "amixer set Master 1- unmute"
cmd_volUp = "amixer set Master 1+ unmute"
cmd_volMute = "amixer set Master toggle"
cmd_touchpadToggle = "touchtoggle"
cmd_lockSuspend = cmd_lockScreen ++ " & sleep 3 && sudo pm-suspend"

-- Key codes
keyCode_volDown = 0x1008ff11
keyCode_volUp = 0x1008ff13
keyCode_volMute = 0x1008ff12
keyCode_suspend = 0x1008ffa7

-- Color definitions
colorDef_white = "#ffffff"
colorDef_darkGray = "#888888"
colorDef_midGray = "#111111"

-- Color assignments
color_focusedBorder = colorDef_darkGray
color_normalBorder = colorDef_midGray

-- Additional workspaces & associated hotkeys
addWorkspaces = [("0", xK_0)]

-- List of X11 window classes which should never steal focus
noStealFocusWins = ["Pidgin"]

-- Misc constants
my_terminal = "urxvt"
my_modKey = mod4Mask

-- General configuration
myConfig = withUrgencyHook NoUrgencyHook defaultConfig
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
         , ((0, keyCode_suspend), spawn cmd_lockSuspend)
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
         , ((my_modKey, xK_g), goToSelected defaultGSConfig)
         , ((my_modKey .|. shiftMask, xK_backslash), spawn cmd_browser)
         , ((my_modKey, xK_f), toggleFloatNext >> runLogHook)
         , ((my_modKey, xK_d), toggleFloatAllNew >> runLogHook)
         , ((my_modKey, xK_F12), spawn cmd_touchpadToggle)
         ] ++
         [((my_modKey .|. m, k), windows $ f ws)
            | (m, f) <- [(0, greedyView), (shiftMask, shift)]
            , (ws, k) <- addWorkspaces]

-- Status bar configuration
myStatusBar = statusBar ("dzen2 " ++ flags) dzenPP' $ const (my_modKey, xK_b)
    where fg = "white"  -- Default: #a8a3f7
          bg = "black"  -- Default: #3f3c6d
          flags = "-e 'onstart=lower' -w 1055 -ta l -fg "
               ++ fg
               ++ " -bg "
               ++ bg
               ++ " -fn '-*-profont-*-*-*-*-11-*-*-*-*-*-*-*'"
          dzenPP' = let lt = colorDef_white
                        md = colorDef_darkGray
                        dk = "black"
                    in defaultPP
                        { ppCurrent = dzenColor dk lt . pad
                        , ppVisible = dzenColor dk md . pad
                        , ppHidden = dzenColor lt dk . pad
                        , ppHiddenNoWindows = const ""
                        , ppUrgent = dzenColor "blue" "red" . pad
                        , ppWsSep = ""
                        , ppSep = ""
                        , ppLayout = dzenColor dk md . \x -> pad $ case x of
                            "Maximize Tall" -> "MXT"
                            "Tabbed Simplest" -> "TAB"
                            _ -> x
                        , ppTitle = dzenColor lt dk . pad . dzenEscape
                        , ppExtras = [ willFloatNextPP
                                     , willFloatAllNewPP
                                     ] <*> pure
                                         ( dzenColor dk md
                                         . pad
                                         . \s -> case s of "All" -> "*"
                                                           "Next" -> "+"
                                         )
                        }

-- Workspace layouts
myLayouts = (maximize $ Tall 1 (3/100) (1/2))
        ||| simpleTabbed

main = xmonad =<< myStatusBar myConfig
