import Data.Char
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.Decoration
import XMonad.Layout.Grid
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.StackSet (focusDown)
import XMonad.Util.EZConfig(additionalKeys)

cmd_browser = "firefox || ([ $? -eq 127 ] && chromium)"
cmd_lockScreen = "slock"
cmd_volDown = "amixer set Master 1- unmute"
cmd_volUp = "amixer set Master 1+ unmute"
cmd_volMute = "amixer set Master toggle"

keyCode_volDown = 0x1008ff11
keyCode_volUp = 0x1008ff13
keyCode_volMute = 0x1008ff12

color_focusedBorder = "#8f7100"
color_normalBorder = "#111111"

myConfig = withUrgencyHook NoUrgencyHook defaultConfig 
    { terminal = "urxvt" 
    , modMask = mod4Mask
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
    } `additionalKeys` myKeys

myKeys = [ ((mod4Mask .|. shiftMask, xK_l), spawn cmd_lockScreen)
         , ((0, keyCode_volDown), spawn cmd_volDown)
         , ((0, keyCode_volUp), spawn cmd_volUp)
         , ((0, keyCode_volMute), spawn cmd_volMute)
         , ((mod4Mask, xK_i), withFocused (\f -> sendMessage (MinimizeWin f)))
         , ((mod4Mask .|. shiftMask, xK_i), sendMessage RestoreNextMinimizedWin)
         , ( (mod4Mask .|. controlMask, xK_i)
           , withFocused (\f -> sendMessage (RestoreMinimizedWin f))
           )
         , ( (mod4Mask, xK_backslash)
           , withFocused (sendMessage . maximizeRestore)
           )
         , ((mod4Mask, xK_Up), sendMessage $ Go U)
         , ((mod4Mask, xK_Down), sendMessage $ Go D)
         , ((mod4Mask, xK_Left), sendMessage $ Go L)
         , ((mod4Mask, xK_Right), sendMessage $ Go R)
         , ((mod4Mask .|. shiftMask, xK_Up), sendMessage $ Swap U)
         , ((mod4Mask .|. shiftMask, xK_Down), sendMessage $ Swap D)
         , ((mod4Mask .|. shiftMask, xK_Left), sendMessage $ Swap L)
         , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
         , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
         , ((mod4Mask .|. shiftMask, xK_backslash), spawn cmd_browser)
         , ((mod4Mask, xK_f), toggleFloatNext)
         , ((mod4Mask, xK_d), toggleFloatAllNew)
         ]

myStatusBar conf = statusBar ("dzen2 " ++ flags) dzenPP' toggleStrutsKey conf
    where fg = "white"  -- Default: #a8a3f7
          bg = "black"  -- Default: #3f3c6d
          flags = "-e 'onstart=lower' -w 1055 -ta l -fg "
               ++ fg
               ++ " -bg "
               ++ bg
               ++ " -fn '-*-profont-*-*-*-*-11-*-*-*-*-*-*-*'"
          dzenPP' = let amber = "#dcae00"
                        darkamber = "#8f7100"
                        lt = amber
                        md = darkamber
                        dk = "black"
                    in defaultPP
                        { ppCurrent = dzenColor dk lt . pad
                        , ppVisible = dzenColor dk md . pad
                        , ppHidden = dzenColor lt dk . pad
                        , ppHiddenNoWindows = const ""
                        , ppUrgent = dzenColor "blue" "red" . dzenStrip
                        , ppWsSep = ""
                        , ppSep = ""
                        , ppLayout = dzenColor dk md . \x -> pad $ case x of
                            "Maximize Minimize Tall" -> "MMT"
                            "Tabbed Simplest" -> "TAB"
                            _ -> x
                        , ppTitle = dzenColor lt dk . pad . dzenEscape
                        }

toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myLayouts = (maximize $ minimize $ Tall 1 (3/100) (1/2)) 
        ||| simpleTabbed 

greekLCaseWorkspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]
greekUCaseWorkspaces =
    [[chr (ord (head ch) - 0x20)] | ch <- greekLCaseWorkspaces]
latinUCaseWorkspaces = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]

-- List of X11 window classes which should never steal focus
noStealFocusWins = ["Pidgin"]

main = xmonad =<< myStatusBar myConfig
