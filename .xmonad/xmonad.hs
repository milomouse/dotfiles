-- ~/.xmonad/xmonad.hs
-- milomouse <vincent[at]fea.st>

-- Imports
import XMonad
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.Search
import XMonad.Actions.WindowGo
import XMonad.Actions.SinkAll
import XMonad.Operations
import XMonad.Prompt
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.MultiToggle
import XMonad.Layout.Master
import XMonad.Layout.Tabbed
import XMonad.Util.Run
import System.IO
import System.Exit
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Basic Variables
modMask' :: KeyMask
modMask' = mod4Mask
myTerminal   = "urxvt"
myStatusBar  = "dzen2 -x '0' -y '0' -h '13' -w '130' -ta 'l' -bg '#161616' -fg '#a9a6af' -fn '-*-fixed-medium-r-normal-*-10-*-*-*-*-*-*-*'"
myWorkspaces = map show [1..5]
myLayouts    = windowNavigation $ smartBorders (noBorders ((avoidStruts $ Full ||| mkToggle (single NBFULL) (mastered 0.02 0.4 $ tabbedAlways shrinkText myTabConfig))))

-- Hooks
layoutHook' = myLayouts
-- <window management>
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r     --> doIgnore       |   r   <- myIgnores] -- ignore desktop
    , [className    =? c     --> doShift  "2"   |   c   <- myWork   ] -- shift myWork's to 2
    , [className    =? c     --> doShift  "3"   |   c   <- myInet   ] -- shift myInet's to 3
    , [className    =? c     --> doShift  "4"   |   c   <- myFoto   ] -- shift myFoto's to 4
    , [className    =? c     --> doCenterFloat  |   c   <- myFloatsC] -- float center geometry by class
    , [name         =? n     --> doCenterFloat  |   n   <- myFloatsN] -- float center geometry by name
    , [name         =? n     --> doFullFloat    |   n   <- myTrueFSN] -- float true fullscreen
    ])
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        -- <<classnames>>
        myFloatsC = ["MPlayer","Zenity","Save As...","Downloads"]
        myFoto    = ["Gliv","Display"]
        myInet    = ["Navigator","Minefield","Firefox","Uzbl","uzbl","Uzbl-core","uzbl-core"]
        myWork    = ["Eclipse","eclipse","Netbeans"]
        -- <<resources>>
        myIgnores = ["desktop","desktop_window","notify-osd"]
        -- <<names>>
        myFloatsN = ["gcolor2"]
        myTrueFSN = ["GLiv in fullscreen"]
-- <xmonad's statusbar>
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorBlueAlt    colorDarkGray . pad
      , ppVisible           =   dzenColor colorCream      colorDarkGray . pad
      , ppHidden            =   dzenColor colorDarkCream  colorDarkGray . pad
      , ppHiddenNoWindows   =   dzenColor colorDarkWhite  colorDarkGray . pad
      , ppUrgent            =   dzenColor colorMagenta    colorDarkGray . pad
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppLayout            =   dzenColor colorMagentaAlt colorDarkGray .
                                (\x -> case x of
                                    "Full"                     -> "*"
                                    "Mastered Tabbed Simplest" -> "="
                                    _                          -> x
                                )
      , ppTitle             =   (" " ++) . dzenColor colorWhiteAlt colorDarkGray . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

-- Main Config
main = do
    dzenTopBar <- spawnPipe myStatusBar
    xmonad $ defaultConfig
      { terminal           = myTerminal
      , workspaces         = myWorkspaces
      , keys               = keys'
      , modMask            = modMask'
      , startupHook        = ewmhDesktopsStartup >> setWMName "LG3D"
      , layoutHook         = layoutHook'
      , manageHook         = manageHook'
      , logHook            = myLogHook dzenTopBar >> setWMName "LG3D"
      , normalBorderColor  = colorNormalBorder
      , focusedBorderColor = colorFocusedBorder
      , borderWidth        = 2 -- for floating windows (noBorders on layouts)
      , focusFollowsMouse  = False
}

-- Tab-bar Settings
myTabConfig = defaultTheme { fontName            = xftFont
                           , inactiveBorderColor = colorGrayAlt
                           , inactiveColor       = colorDarkGray
                           , inactiveTextColor   = colorGrayAlt
                           , activeBorderColor   = colorGrayAlt
                           , activeColor         = colorDarkMagenta
                           , activeTextColor     = colorDarkGray
                           , urgentBorderColor   = colorBlackAlt
                           , urgentTextColor     = colorWhite
                           , decoHeight          = 12}

-- Prompt Settings
smallXPConfig :: XPConfig
smallXPConfig =
    defaultXPConfig { font                  = xftFont
                    , bgColor               = colorBlackAlt
                    , fgColor               = colorMagenta
                    , bgHLight              = colorDarkMagenta
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 16
                    , historyFilter         = deleteConsecutive
                    }

-- Colors
colorBlack           = "#000000"
colorBlackAlt        = "#040404"
colorGray            = "#606060"
colorGrayAlt         = "#282828"
colorDarkGray        = "#161616"
colorWhite           = "#cfbfad"
colorWhiteAlt        = "#8c8b8e"
colorDarkWhite       = "#444444"
colorCream           = "#a9a6af"
colorDarkCream       = "#5f656b"
colorMagenta         = "#a488d9"
colorMagentaAlt      = "#7965ac"
colorDarkMagenta     = "#8e82a2"
colorBlue            = "#98a7b6"
colorBlueAlt         = "#598691"
colorDarkBlue        = "#464a4a"
colorNormalBorder    = colorGray
colorFocusedBorder   = colorMagenta

-- Fonts
barFont  = "fixed"
barXFont = "fixed:size=10"
xftFont = "xft: fixed-10"

-- Key-Bindings
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- <basic commands>
    [ ((modMask .|. shiftMask,     xK_q         ), io (exitWith ExitSuccess)) -- exit xmonad
    , ((modMask .|. shiftMask,     xK_r         ), restart "xmonad" True) -- restart xmonad
    , ((modMask .|. controlMask,   xK_r         ), spawn "xmonad --recompile && xmonad --restart") -- recompile and restart xmonad
    , ((modMask,                   xK_b         ), refresh) -- bump window to correct size
    , ((modMask .|. controlMask,   xK_BackSpace ), kill) -- kill selected window
    -- <prompts/utils>
    , ((0,                         xK_F1        ), manPrompt smallXPConfig) -- manpage prompt
    , ((0,                         xK_F2        ), shellPrompt smallXPConfig) -- shell prompt
    , ((0,                         xK_F3        ), windowPromptGoto smallXPConfig) -- goto window on it's workspace on in it's frame
    , ((modMask,                   xK_F3        ), windowPromptBring smallXPConfig) -- bring window to current workspace in current frame
    , ((0,                         xK_F4        ), promptSearchBrowser smallXPConfig "firefox" multi) -- internet search (engine:string (google default))
    , ((modMask,                   xK_F4        ), submap . M.fromList $ -- internet seach (pre-selected engines)
                                [ ((0, xK_g       ), promptSearchBrowser smallXPConfig "firefox" google)
                                , ((0, xK_l       ), promptSearchBrowser smallXPConfig "firefox" lucky)
                                , ((0, xK_i       ), promptSearchBrowser smallXPConfig "firefox" images)
                                , ((0, xK_w       ), promptSearchBrowser smallXPConfig "firefox" wikipedia)
                                , ((0, xK_h       ), promptSearchBrowser smallXPConfig "firefox" hoogle)
                                , ((0, xK_a       ), promptSearchBrowser smallXPConfig "firefox" amazon)
                                ])
    , ((0,                         xK_F12       ), appendFilePrompt smallXPConfig "/home/milo/othe/.TODO_now") -- add one-liner to file
    -- <common programs>
    , ((modMask,                   xK_Escape    ), spawn "banishmouse") -- hide and freeze the mouse cursor (or bring back to original location)
    , ((0,                         xK_Print     ), spawn "import -window root /home/milo/foto/shot/$(date +%Y_%m_%d-%H%M%S).png") -- take screenshot of current workspace
    , ((modMask .|. shiftMask,     xK_Delete    ), spawn "alock -bg image:file=/home/milo/foto/wall/beheading.jpg -cursor glyph -auth pam >&/dev/null") -- lock screen
    , ((modMask .|. shiftMask,     xK_Return    ), spawn $ XMonad.terminal conf) -- spawn terminal by itself
    , ((modMask,                   xK_Return    ), spawn "urxvt -e tmux") -- spawn terminal in tmux
    , ((modMask,                   xK_grave     ), spawn "urxvt -e ncmpcpp") -- spawn audio player
    , ((modMask,                   xK_f         ), runOrRaiseMaster "firefox" (className =? "Firefox")) -- run or goto/raise firefox
    -- <function/media keys>
    , ((0 .|. controlMask,         0x1008ff02   ), spawn "moodlight -m") -- maximum screen brightness ((XF86MonBrightnessUp [max]))
    , ((0,                         0x1008ff02   ), spawn "moodlight -u") -- increase screen brightness ((XF86MonBrightnessUp))
    , ((0,                         0x1008ff03   ), spawn "moodlight -d") -- decrease screen brightness ((XF86MonBrightnessDown))
    , ((0,                         0x1008ff12   ), spawn "mossrat -m")   -- mute volume, via "mossrat" ((XF86AudioMute))
    , ((0,                         0x1008ff11   ), spawn "mossrat -d 1") -- decrease volume, via "mossrat" ((XF86AudioLowerVolume))
    , ((0,                         0x1008ff13   ), spawn "mossrat -i 1") -- increase volume, via "mossrat" ((XF86AudioRaiseVolume))
    , ((modMask,                   xK_a         ), submap . M.fromList $ -- "mossrat" commong sub-bindings (music playing script)
                                [ ((0, xK_t       ), spawn "mossrat --toggle") -- <toggle song>
                                , ((0, xK_s       ), spawn "mossrat --stop") -- <stop song>
                                , ((0, xK_p       ), spawn "mossrat --prev") -- <play previous song>
                                , ((0, xK_n       ), spawn "mossrat --next") -- <play next song>
                                ])
    , ((modMask,                   xK_s         ), submap . M.fromList $ -- "songrem" common sub-bindings (fav. song script)
                                [ ((0, xK_a       ), spawn "songrem --add") -- <add current song to list>
                                , ((0, xK_r       ), spawn "songrem --remove") -- <remove current song, if in list>
                                , ((0, xK_e       ), spawn "songrem --edit") -- <manually edit list in terminal>
                                , ((0, xK_n       ), spawn "songrem --play") -- <play song from list>
                                ])
    -- <tiled windows (bindings only suitable for my specific layout it seems)>
    , ((modMask,                   xK_m         ), windows W.focusMaster) -- immediately focus on master
    , ((modMask .|. shiftMask,     xK_m         ), promote) -- swap with & focus on master (if xK_m in master; like "Swap D" but keeps focus)
    , ((modMask,                   xK_Tab       ), rotSlavesUp) -- rotate all slaves
    , ((modMask,                   xK_h         ), rotFocusedDown) -- rotate prev focused (master or slave not already visible)
    , ((modMask,                   xK_l         ), rotFocusedUp) -- rotate next (master or slave not already visible)
    , ((modMask .|. shiftMask,     xK_h         ), rotSlavesDown)  -- rotate prev (slaves only)
    , ((modMask .|. shiftMask,     xK_l         ), rotSlavesUp)  -- rotate next (slaves only)
    , ((modMask .|. controlMask,   xK_h         ), rotUnfocusedDown)  -- rotate prev unfocused (master too, if slave is focused)
    , ((modMask .|. controlMask,   xK_l         ), rotUnfocusedUp)  -- rotate next unfocused (master too, if slave is focused)
    , ((modMask,                   xK_n         ), windows W.focusDown) -- focus down/next (in Full layout, or if you'd rather see tabs move (undesirable))
    , ((modMask,                   xK_p         ), windows W.focusUp) -- focus up/prev (in Full layout, or if you'd rather see tabs move (undesirable))
    , ((modMask .|. shiftMask,     xK_n         ), windows W.swapDown) -- swap down/next (in Full layout, or for tabbed slave movement (undesirable))
    , ((modMask .|. shiftMask,     xK_p         ), windows W.swapUp) -- swap up/prev (in Full layout, or for tabbed slave movement (undesirable))
    , ((modMask,                   xK_j         ), sendMessage $ Go D) -- focus down a frame
    , ((modMask,                   xK_k         ), sendMessage $ Go U) -- focus up a frame
    , ((modMask .|. shiftMask,     xK_j         ), sendMessage $ Swap D) -- swap window with lower frame and focus on it
    , ((modMask .|. shiftMask,     xK_k         ), sendMessage $ Swap U) -- swap window with above frame and focus on it
    , ((modMask .|. controlMask,   xK_j         ), sendMessage Expand) -- expand size of master frame
    , ((modMask .|. controlMask,   xK_k         ), sendMessage Shrink) -- compress size of master frame
    -- <floating windows (rarely use these)>
    , ((modMask,                   xK_w         ), withFocused $ windows . W.sink) -- push a focused floating window back into tiling
    , ((modMask .|. shiftMask,     xK_w         ), sinkAll) -- push all floating windows in workspace into tiling
    , ((modMask,                   xK_u         ), withFocused (keysMoveWindow (0,10))) -- move down
    , ((modMask .|. shiftMask,     xK_u         ), withFocused (keysResizeWindow (0,-10) (0,1))) -- decrease down
    , ((modMask .|. controlMask,   xK_u         ), withFocused (keysResizeWindow (0,10) (0,1))) -- increase down
    , ((modMask,                   xK_i         ), withFocused (keysMoveWindow (0,-10))) -- move up
    , ((modMask .|. shiftMask,     xK_i         ), withFocused (keysResizeWindow (0,-10) (1,0))) -- decrease up
    , ((modMask .|. controlMask,   xK_i         ), withFocused (keysResizeWindow (0,10) (1,0))) -- increase up
    , ((modMask,                   xK_y         ), withFocused (keysMoveWindow (-10,0))) -- move left
    , ((modMask .|. shiftMask,     xK_y         ), withFocused (keysResizeWindow (-10,0) (1,1))) -- decrease left
    , ((modMask .|. controlMask,   xK_y         ), withFocused (keysResizeWindow (10,0) (1,1))) -- increase left
    , ((modMask,                   xK_o         ), withFocused (keysMoveWindow (10,0))) -- move right
    , ((modMask .|. shiftMask,     xK_o         ), withFocused (keysResizeWindow (-10,0) (0,1))) -- decrease right
    , ((modMask .|. controlMask,   xK_o         ), withFocused (keysResizeWindow (10,0) (0,1))) -- increase right
    -- <layout/workspace common>
    , ((modMask,                   xK_t         ), sendMessage $ Toggle NBFULL) -- toggle Full with noBorders (like "only"), and back again
    , ((modMask,                   xK_space     ), sendMessage NextLayout) -- cycle to next layout
    , ((modMask .|. shiftMask,     xK_space     ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                   xK_period    ), nextWS) -- focus next workspace
    , ((modMask,                   xK_comma     ), prevWS) -- focus previous workspace
    , ((modMask .|. shiftMask,     xK_period    ), shiftToNext) -- move current frame to next workspace
    , ((modMask .|. shiftMask,     xK_comma     ), shiftToPrev) -- move current frame to previous workspace
    ]
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-- vim:sw=2 sts=2 ts=2 tw=0 et ai nowrap
