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
import XMonad.Util.EZConfig(additionalKeys)

main = xmonad =<< myStatusBar myConfig
        
myConfig = withUrgencyHook NoUrgencyHook defaultConfig { terminal = "urxvt" 
                                                       , modMask = mod4Mask
                                                       , focusedBorderColor = "#0000FF"
                                                       , normalBorderColor = "#111111"
                                                       , manageHook = floatNextHook <+> manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
                                                       , layoutHook = configurableNavigation noNavigateBorders $ smartBorders $ avoidStruts myLayouts
                                                       } `additionalKeys`
                                                       [ ((mod4Mask .|. shiftMask, xK_l), spawn "slock")
                                                       , ((0, 0x1008ff11), spawn "amixer set Master 1- unmute")
                                                       , ((0, 0x1008ff13), spawn "amixer set Master 1+ unmute")
                                                       , ((0, 0x1008ff12), spawn "amixer set Master toggle")
                                                       , ((mod4Mask, xK_i), withFocused (\f -> sendMessage (MinimizeWin f)))
                                                       , ((mod4Mask .|. shiftMask, xK_i), sendMessage RestoreNextMinimizedWin)
                                                       , ((mod4Mask .|. controlMask, xK_i), withFocused (\f -> sendMessage (RestoreMinimizedWin f)))
                                                       , ((mod4Mask, xK_backslash), withFocused (sendMessage . maximizeRestore))
                                                       , ((mod4Mask, xK_Up), sendMessage $ Go U)
                                                       , ((mod4Mask, xK_Down), sendMessage $ Go D)
                                                       , ((mod4Mask, xK_Left), sendMessage $ Go L)
                                                       , ((mod4Mask, xK_Right), sendMessage $ Go R)
                                                       , ((mod4Mask .|. shiftMask, xK_Up), sendMessage $ Swap U)
                                                       , ((mod4Mask .|. shiftMask, xK_Down), sendMessage $ Swap D)
                                                       , ((mod4Mask .|. shiftMask, xK_Left), sendMessage $ Swap L)
                                                       , ((mod4Mask .|. shiftMask, xK_Right), sendMessage $ Swap R)
                                                       , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
                                                       , ((mod4Mask .|. shiftMask, xK_backslash), spawn "chromium")
                                                       , ((mod4Mask, xK_f), toggleFloatNext)
                                                       , ((mod4Mask, xK_d), toggleFloatAllNew)
                                                       ]

myStatusBar conf = statusBar ("dzen2 " ++ flags) dzenPP toggleStrutsKey conf
        where fg = "'#a8a3f7'"  -- Default: #a8a3f7
              bg = "'#101f3e'"  -- Default: #3f3c6d
              flags = "-e 'onstart=lower' -w 1055 -ta l -fg " ++ fg ++ " -bg " ++ bg ++ " -fn '-*-profont-*-*-*-*-11-*-*-*-*-*-*-*'"

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)

myLayouts = (maximize $ minimize $ Tall 1 (3/100) (1/2)) 
        ||| simpleTabbed 
--      ||| (maximize $ minimize $ Mirror $ Tall 1 (3/100) (1/2)) 
        ||| Full 
--      ||| (maximize $ minimize $ Grid) 
--      ||| (maximize $ minimize $ simpleFloat)

greekLCaseWorkspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]
greekUCaseWorkspaces = [[chr (ord (head ch) - 0x20)] | ch <- greekLCaseWorkspaces]
latinUCaseWorkspaces = ["A", "B", "C", "D", "E", "F", "G", "H", "I"]
