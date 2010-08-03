-------------------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-------------------------------------------------------------------------------------------
-- author: milomouse <vincent[at]fea.st>
-- credit: serrghi     -> config used as my starting grounds--very clean/workable.
--         serverninja -> too many thanks to mention (formatting, layout ideas, etc.)
--         pbrisbin    -> scratchpad 'NSP' workspace hiding, and "versions" idea.
-------------------------------------------------------------------------------------------
-- versions used atoc:
-- |  ghc                  -> 6.12.1-4
-- |  haskell-mtl          -> 1.1.0.2-3
-- |  haskell-utf8-string  -> 0.3.6-3
-- |  haskell-x11          -> 1.5.0.0-2
-- |  haskell-x11-xft      -> 0.3-13.1
-- |  xmonad-darcs         -> 20100423-1
-- |  xmonad-contrib-darcs -> 20100424-1
-- |  dzen2-svn            -> 271-1
-------------------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

-- IMPORTS {{{

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio
import System.IO
import System.Exit

-- <actions>
import XMonad.Actions.CycleWS (nextWS,prevWS,toggleWS,shiftToNext,shiftToPrev)
import XMonad.Actions.RotSlaves (rotAllDown,rotSlavesDown,rotSlavesUp)
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.FloatKeys (keysMoveWindow,keysResizeWindow)
import XMonad.Actions.WithAll
import XMonad.Actions.Search
import XMonad.Actions.Submap
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.Submap as SM

-- <hooks>
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks (avoidStruts,avoidStrutsOn,ToggleStruts(..))
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsStartup)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook

-- <utilities>
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.Scratchpad (scratchpadManageHook,scratchpadSpawnActionCustom)
import XMonad.Util.SpawnOnce

-- <prompts>
import XMonad.Prompt
import qualified XMonad.Prompt as P
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Window (windowPromptBring,windowPromptGoto)

-- <layouts>
import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

-- <layout helpers>
import XMonad.Layout.Drawer
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Named
import XMonad.Layout.WindowNavigation

-- end of IMPORTS }}}





-- MAIN CONFIGURATION {{{

main = do
    dzenStatusBar <- spawnPipe myStatusBar
    xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-fn", barFont, "-bg", colorDarkCream, "-fg", colorBlue]} $ defaultConfig
      { modMask             = myModMask
      , keys                = myKeyBindings
      , terminal            = "urxvt"
      , workspaces          = map show [1..6]
      , layoutHook          = myLayouts
      , manageHook          = insertPosition Below Newer <+> myManageHook
      , startupHook         = myStartHook
      , logHook             = myLogHook dzenStatusBar >> setWMName "LG3D"
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth         = 2 -- for floating windows ((..)Borders `Or` withBorder Int $ on Layouts)
      , focusFollowsMouse   = False
      }
myStartHook = spawnOnce ". $HOME/.xmonad/dzen4xmonad" <+>
              setDefaultCursor xC_left_ptr <+>
              ewmhDesktopsStartup >> setWMName "LG3D"

-- end of MAIN-CONFIGURATION }}}





-- COLORS, FONTS, AND PROMPTS {{{

-- <colors>
colorBlack          = "#000000"
colorBlackAlt       = "#040404"
colorGray           = "#606060"
colorGrayAlt        = "#282828"
colorDarkGray       = "#161616"
colorWhite          = "#cfbfad"
colorWhiteAlt       = "#8c8b8e"
colorDarkWhite      = "#444444"
colorCream          = "#a9a6af"
colorDarkCream      = "#5f656b"
colorMagenta        = "#a488d9"
colorMagentaAlt     = "#7965ac"
colorDarkMagenta    = "#8e82a2"
colorBlue           = "#98a7b6"
colorBlueAlt        = "#598691"
colorDarkBlue       = "#464a4a"
colorNormalBorder   = colorGray
colorFocusedBorder  = colorMagenta

-- <fonts>
barFont   = "-*-fixed-medium-r-normal-*-*-*-10-*-*-*-*-*-*-*"
barXFont  = "fixed:size=10"
xftFont   = "xft: fixed-10"

-- <tab-bar configuration>
myTabTheme =
    defaultTheme { fontName            = xftFont
                 , inactiveBorderColor = colorGrayAlt
                 , inactiveColor       = colorDarkGray
                 , inactiveTextColor   = colorGrayAlt
                 , activeBorderColor   = colorGrayAlt
                 , activeColor         = colorDarkMagenta
                 , activeTextColor     = colorDarkGray
                 , urgentBorderColor   = colorBlackAlt
                 , urgentTextColor     = colorWhite
                 , decoHeight          = 12
                 }

-- <prompts>
myXPConfig :: XPConfig
myXPConfig =
    defaultXPConfig { font                  = xftFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorMagenta
                    , bgHLight              = colorDarkMagenta
                    , fgHLight              = colorDarkGray
                    , borderColor           = colorBlackAlt
                    , promptBorderWidth     = 1
                    , height                = 14
                    , position              = Bottom
                    , historySize           = 100
                    , historyFilter         = deleteConsecutive
                    }

-- end of COLORS, FONTS, AND PROMPTS }}}





-- UTILITY FUNCTIONS {{{

-- <grid-select> (magenta color scheme)
myColorizer = colorRangeFromClassName
    (0x00,0x00,0x00) -- lowest inactive bg
    (0xBB,0xAA,0xFF) -- highest inactive bg
    (0x88,0x66,0xAA) -- active bg
    (0xBB,0xBB,0xBB) -- inactive fg
    (0x00,0x00,0x00) -- active fg
  where
    black = minBound
    white = maxBound

myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 50
    , gs_cellwidth    = 200
    , gs_cellpadding  = 10
    , gs_font         = xftFont
    }

-- <scratchpad>
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect (1/6) (1/4) (2/3) (2/5))
scratchPad = scratchpadSpawnActionCustom $ -- make sure to indent the next line:
  "urxvt -name scratchpad +sb -fn '-*-fixed-medium-r-normal-*-9-*-*-*-*-*' -e tmux -f $XDG_CONFIG_DIR/tmux/tmux.conf -L sp new-session 'ncmpcpp || zsh'"

-- end of UTILITY FUNCTIONS }}}





-- LAYOUTS {{{

myLayouts = avoidStruts $ gaps [(U,14)]   $
            windowNavigation              $
            mkToggle (single NBFULL)      $
            mkToggle (single REFLECTX)    $
            mkToggle (single REFLECTY)    $
            mkToggle (single NOBORDERS)   $
            onWorkspace "4" inetLayouts   $
            onWorkspace "5" fotoLayouts   $
            (collectiveLayouts)
  where
    collectiveLayouts = myTile ||| myOneB ||| myFull ||| myTabd

    -- <define layouts>
    myFull = named "*" (smartBorders $ Full)
    myTabd = named "-" (smartBorders $ tabbedAlways shrinkText myTabTheme)
    myOneB = named "@" (lessBorders (OnlyFloat) (withBorder 1 (limitWindows 10 (OneBig 0.75 0.65))))
    myTile = named "#" (lessBorders (OnlyFloat) (withBorder 1 (drawer `onBottom` (ResizableTall 1 0.03 0.66 []))))
      where
        drawer = simpleDrawer 0 0.36 (ClassName "Firefox" `Or` ClassName "Zathura")

    -- <layouts per workspace>
    inetLayouts = myOneB ||| myFull ||| myTabd
    fotoLayouts = myFull ||| myTabd

-- end of LAYOUTS }}}





-- WORKSPACES/STATUSBAR {{{

-- <window management>
myManageHook :: ManageHook
myManageHook = (composeAll . concat $
    [ [resource     =? r     --> doIgnore       |   r   <- myIgnores] -- ignore desktop
    , [className    =? c     --> doShift  "4"   |   c   <- myInetC  ] -- move myInetC windows to workspace 4
    , [className    =? c     --> doShift  "5"   |   c   <- myFotoC  ] -- move myFotoC windows to workspace 5
    , [className    =? c     --> doShift  "6"   |   c   <- myElseC  ] -- move myElseC windows to workspace 6
    , [className    =? c     --> doCenterFloat  |   c   <- myFloatsC] -- float center geometry by 
    , [name         =? n     --> doCenterFloat  |   n   <- myFloatCN] -- float center geometry by name
    , [name         =? n     --> doSideFloat SE |   n   <- myFloatSN] -- float side geometry by name
    , [name         =? n     --> doFullFloat    |   n   <- myTrueFSN] -- float true fullscreen by name
    ]) <+> manageScratchPad
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        -- <<class>>
        myFloatsC = ["MPlayer","Save As...","Downloads","xskat"]
        myElseC   = ["xskat"]
        myInetC   = ["Minefield","Firefox","Jumanji","Dwb","Surf"]
        myFotoC   = ["Gliv","Display"]
        -- <<resource>>
        myIgnores = ["desktop","desktop_window"]
        -- <<name>>
        myFloatSN = ["gcolor2"]
        myFloatCN = ["Add-ons"]
        myTrueFSN = ["GLiv in fullscreen"]

-- <statusbar/logging>
myStatusBar = "dzen2 -x '0' -y '0' -h '13' -w '238' -ta 'l' -bg '#161616' -fg '#a9a6af' -fn '-*-fixed-medium-r-normal-*-10-*-*-*-*-*-*-*'"
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorBlueAlt    colorDarkGray . hideScratchpad
      , ppVisible           =   dzenColor colorCream      colorDarkGray . hideScratchpad
      , ppHidden            =   dzenColor colorDarkCream  colorDarkGray . hideScratchpad
      , ppHiddenNoWindows   =   dzenColor colorDarkWhite  colorDarkGray . hideScratchpad
      , ppUrgent            =   dzenColor colorMagenta    colorDarkGray . pad
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppLayout            =   dzenColor colorMagentaAlt colorDarkGray .
                                (\x -> case x of
                                    "Full" -> "*"
                                    "ReflectX *" -> "*"
                                    "ReflectX -" -> "-"
                                    "ReflectX =" -> "="
                                    "ReflectX +" -> "+"
                                    "ReflectX %" -> "%"
                                    "ReflectX @" -> "@"
                                    "ReflectX #" -> "#"
                                    "ReflectY *" -> "*"
                                    "ReflectY -" -> "-"
                                    "ReflectY =" -> "="
                                    "ReflectY +" -> "+"
                                    "ReflectY %" -> "%"
                                    "ReflectY @" -> "@"
                                    "ReflectY #" -> "#"
                                    "ReflectX ReflectY *" -> "*"
                                    "ReflectX ReflectY -" -> "-"
                                    "ReflectX ReflectY =" -> "="
                                    "ReflectX ReflectY +" -> "+"
                                    "ReflectX ReflectY %" -> "%"
                                    "ReflectX ReflectY @" -> "@"
                                    "ReflectX ReflectY #" -> "#"
                                    _      -> x
                                )
      , ppTitle             =   (" " ++) . dzenColor colorWhiteAlt colorDarkGray . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
    where
      hideScratchpad ws = if ws == "NSP" then "" else pad ws -- hide sp in ws list (thanks to p.brisbin)

-- end of WORKSPACES/STATUSBAR }}}





-- KEY-BINDINGS {{{

myModMask :: KeyMask
myModMask =  mod4Mask

myKeyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeyBindings conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- <basic commands>
    [ ((modMask .|. shiftMask,     xK_q         ), io (exitWith ExitSuccess)) -- exit xmonad
    , ((modMask .|. shiftMask,     xK_r         ), restart "xmonad" True) -- restart xmonad
    , ((modMask .|. controlMask,   xK_r         ), unsafeSpawn "xmonad --recompile && xmonad --restart") -- recompile and restart xmonad
    , ((modMask,                   xK_b         ), refresh) -- bump window to correct size
    , ((modMask .|. controlMask,   xK_BackSpace ), kill) -- kill selected window
    -- <prompts/utils>
    , ((modMask,                   xK_semicolon ), shellPrompt myXPConfig) -- shell prompt
    , ((modMask .|. shiftMask,     xK_semicolon ), manPrompt myXPConfig) -- manpage prompt
    , ((0,                         xK_F1        ), windowPromptGoto myXPConfig) -- goto window on it's workspace and focus in it's frame
    , ((0,                         xK_F2        ), windowPromptBring myXPConfig) -- bring window to current workspace and into currently focused frame
    , ((0,                         xK_F3        ), promptSearchBrowser myXPConfig "firefox" multi) -- internet search (engine:string (google default))
    , ((modMask,                   xK_F3        ), SM.submap $ searchEngineMap $ promptSearchBrowser myXPConfig "firefox") -- internet seach (sub-bindings at end of section)
    , ((0,                         xK_F4        ), appendFilePrompt myXPConfig "/home/milo/othe/.TODO_now") -- add one-liner to file (cannot expand $HOME)
    , ((modMask,                   xK_g         ), goToSelected $ myGSConfig myColorizer) -- show a grid of windows to jump to
    , ((modMask .|. shiftMask,     xK_g         ), bringSelected $ myGSConfig myColorizer) -- show a grid of windows to bring here
    -- <common programs>
    , ((modMask,                   xK_Escape    ), safeSpawnProg "banishmouse") -- hide and lock the mouse cursor (or bring back to original location and unlock mouse)
    , ((modMask,                   xK_Print     ), unsafeSpawn "import -window root $HOME/foto/shot/$(date +%Y_%m_%d-%H%M%S).png") -- take screenshot of current workspace
    , ((modMask .|. shiftMask,     xK_Delete    ), unsafeSpawn "alock -bg image:file=$HOME/foto/wall/beheading.jpg -cursor glyph -auth pam >&/dev/null") -- lock screen
    , ((modMask .|. shiftMask,     xK_Return    ), safeSpawnProg $ XMonad.terminal conf) -- spawn terminal by itself
    , ((modMask,                   xK_Return    ), unsafeSpawn "urxvt -e tmux -f $XDG_CONFIG_DIR/tmux/tmux.conf -L xorg") -- spawn terminal in tmux
    , ((modMask,                   xK_grave     ), scratchPad) -- spawn floating "scratchpad" window
    , ((modMask .|. shiftMask,     xK_f         ), runOrRaise "firefox" (className =? "Firefox")) -- <run or raise firefox>
    , ((modMask,                   xK_f         ), submap . M.fromList $ -- frequently used programs; sub-bindings
                                [ ((0, xK_m       ), runInTerm "" "tmux -f $XDG_CONFIG_DIR/tmux/tmux.conf -L xorg new-session 'mutt -F $XDG_CONFIG_DIR/mutt/muttrc'") -- <open email>
                                , ((0, xK_n       ), runInTerm "" "tmux -f $XDG_CONFIG_DIR/tmux/tmux.conf -L xorg new-session 'nsudoku 12'") -- <open a sudoku game>
                                , ((0, xK_w       ), safeSpawnProg "wallie") -- <change wallpaper to a random one>
                                , ((0, xK_f       ), runOrRaise "firefox" (className =? "Firefox")) -- <run or raise firefox>
                                , ((0, xK_s       ), unsafeSpawn "xskat -opt $XDG_CONFIG_DIR/xorg/xskat.opt -list $XDG_CONFIG_DIR/xorg/xskat.lst") -- <xskat with preferred dirs>
                                ])
    -- <function/media keys>
    , ((0 .|. controlMask,         0x1008ff02   ), unsafeSpawn "sudo moodlight -m") -- maximum screen brightness ((XF86MonBrightnessUp [max]))
    , ((0,                         0x1008ff02   ), unsafeSpawn "sudo moodlight -u") -- increase screen brightness ((XF86MonBrightnessUp))
    , ((0,                         0x1008ff03   ), unsafeSpawn "sudo moodlight -d") -- decrease screen brightness ((XF86MonBrightnessDown))
    , ((0,                         0x1008ff13   ), unsafeSpawn "mossrat -i 1") -- increase volume, via "mossrat" ((XF86AudioRaiseVolume))
    , ((0,                         0x1008ff11   ), unsafeSpawn "mossrat -d 1") -- decrease volume, via "mossrat" ((XF86AudioLowerVolume))
    , ((0,                         0x1008ff12   ), safeSpawn "mossrat" ["-m"])   -- mute volume, via "mossrat" ((XF86AudioMute))
    , ((modMask,                   xK_a         ), submap . M.fromList $ -- "mossrat" common sub-bindings (music playing script)
                                [ ((0, xK_t       ), safeSpawn "mossrat" ["--toggle"]) -- <toggle song>
                                , ((0, xK_s       ), safeSpawn "mossrat" ["--stop"]) -- <stop song>
                                , ((0, xK_p       ), safeSpawn "mossrat" ["--prev"]) -- <play previous song>
                                , ((0, xK_n       ), safeSpawn "mossrat" ["--next"]) -- <play next song>
                                ])
    , ((modMask,                   xK_s         ), submap . M.fromList $ -- "songrem" common sub-bindings (forked fav. song script)
                                [ ((0, xK_a       ), safeSpawn "songrem" ["--add"]) -- <add current song to list>
                                , ((0, xK_r       ), safeSpawn "songrem" ["--remove"]) -- <remove current song, if in list>
                                , ((0, xK_e       ), safeSpawn "songrem" ["--edit"]) -- <manually edit list in [internally called] terminal>
                                , ((0, xK_n       ), safeSpawn "songrem" ["--play"]) -- <play song from list>
                                ])
    , ((modMask .|. shiftMask,     xK_e         ), safeSpawnProg "eject") -- open disc tray
    -- <tiled windows>
    , ((modMask,                   xK_m         ), windows W.focusMaster) -- immediately focus on master
    , ((modMask .|. shiftMask,     xK_m         ), windows W.swapMaster) -- swap with master and focus on it
    , ((modMask,                   xK_equal     ), sendMessage $ IncMasterN 1) -- increase number of masters
    , ((modMask,                   xK_minus     ), sendMessage $ IncMasterN (-1)) -- decrease number of masters
    , ((modMask,                   xK_0         ), sendMessage $ Expand) -- expand size of master frame
    , ((modMask,                   xK_9         ), sendMessage $ Shrink) -- shrink size of master frame
    , ((modMask .|. shiftMask,     xK_0         ), sendMessage $ MirrorShrink) -- shrink size of slave frame (ResizableTile(..))
    , ((modMask .|. shiftMask,     xK_9         ), sendMessage $ MirrorExpand) -- expand size of slave frame (ResizableTile(..))
    , ((modMask,                   xK_j         ), sendMessage $ Go D) -- focus down a frame
    , ((modMask,                   xK_k         ), sendMessage $ Go U) -- focus up a frame
    , ((modMask,                   xK_h         ), sendMessage $ Go L) -- focus left a frame
    , ((modMask,                   xK_l         ), sendMessage $ Go R) -- focus right a frame
    , ((modMask .|. shiftMask,     xK_j         ), sendMessage $ Swap D) -- swap window with lower frame and focus on it
    , ((modMask .|. shiftMask,     xK_k         ), sendMessage $ Swap U) -- swap window with above frame and focus on it
    , ((modMask .|. shiftMask,     xK_h         ), sendMessage $ Swap L) -- swap window with left frame and focus on it
    , ((modMask .|. shiftMask,     xK_l         ), sendMessage $ Swap R) -- swap window with right frame and focus on it
    , ((modMask .|. controlMask,   xK_j         ), rotSlavesDown) -- rotate all slaves down/next
    , ((modMask .|. controlMask,   xK_k         ), rotSlavesUp) -- rotate slaves up/prev
    , ((modMask,                   xK_Tab       ), rotAllDown) -- rotate all windows [slaves/master] down/next
    , ((modMask,                   xK_n         ), windows W.focusDown) -- focus next
    , ((modMask,                   xK_p         ), windows W.focusUp) -- focus prev
    , ((modMask .|. controlMask,   xK_n         ), windows W.swapDown) -- swap next
    , ((modMask .|. controlMask,   xK_p         ), windows W.swapUp) -- swap prev
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
    , ((modMask,                   xK_t         ), submap . M.fromList $ -- common toggle sub-bindings
                                [ ((0, xK_o       ), sendMessage $ Toggle NBFULL) -- <toggle Full with noBorders (like "only"), and back again>
                                , ((0, xK_b       ), sendMessage $ Toggle NOBORDERS) -- <toggle borders>
                                , ((0, xK_s       ), sendMessage $ ToggleStruts) -- <toggle struts>
                                , ((0, xK_g       ), sendMessage $ ToggleGaps) -- <toggle gaps>
                                , ((0, xK_x       ), sendMessage $ Toggle REFLECTX) -- <toggle mirrored layout by X axis>
                                , ((0, xK_y       ), sendMessage $ Toggle REFLECTY) -- <toggle mirrored layout by Y axis>
                                ])
    , ((modMask,                   xK_space     ), sendMessage NextLayout) -- cycle to next layout
    , ((modMask .|. shiftMask,     xK_space     ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                   xK_period    ), nextWS) -- focus next workspace
    , ((modMask,                   xK_comma     ), prevWS) -- focus previous workspace
    , ((modMask,                   xK_slash     ), toggleWS) -- toggle between last viewed workspace and current
    , ((modMask .|. shiftMask,     xK_slash     ), focusUrgent) -- quickly focus on urgent window [in it's original workspace]
    , ((modMask .|. shiftMask,     xK_period    ), shiftToNext) -- move current frame to next workspace
    , ((modMask .|. shiftMask,     xK_comma     ), shiftToPrev) -- move current frame to previous workspace
    , ((modMask .|. controlMask,   xK_period    ), shiftToNext >> nextWS) -- move current frame to next workspace and go there
    , ((modMask .|. controlMask,   xK_comma     ), shiftToPrev >> prevWS) -- move current frame to previous workspace and go there
    ]
    ++ -- view Nth workspace (modMask+Int) or send focused window to workspace (modMask+Shift+Int)
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    ++ -- swap windows from current workspace with another (modMask+Control+Int)
    [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_6]
    ]
    where
      searchEngineMap method = M.fromList $ -- search engines for modMask+F3
          [ ((0, xK_g), method S.google )
          , ((0, xK_i), method S.images )
          , ((0, xK_w), method S.wikipedia )
          , ((0, xK_a), method S.amazon )
          , ((0, xK_b), method $ S.searchEngine "ArchBBS" "http://bbs.archlinux.org/search.php?action=search&keywords=")
          ]

-- end of KEY-BINDINGS }}}

-- vim:foldmethod=marker foldmarker={{{,}}} sw=2 sts=2 ts=2 tw=0 et ai nowrap
