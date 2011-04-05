-------------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs                                                       --
-------------------------------------------------------------------------------
-- author: milomouse <vincent[at]fea.st>                                     --
-- credit: serrghi     -> config used as my starting grounds--clean/workable --
--         serverninja -> too many thanks to mention (format/ideas, etc.)    --
--         pbrisbin    -> scratchpad 'NSP' ws hiding, and "versions" idea    --
-------------------------------------------------------------------------------
-- versions used atoc (on ArchLinux):                                        --
-- |  ghc                           -> 7.0.2-2                               --
-- |  haskell-haskeline             -> 0.6.3.2-2.1                           --
-- |  haskell-mtl                   -> 2.0.1.0-2                             --
-- |  haskell-parsec                -> 3.1.1-2                               --
-- |  haskell-stm                   -> 2.2.0.1-2                             --
-- |  haskell-terminfo              -> 0.3.1.3-4.1                           --
-- |  haskell-utf8-string           -> 0.3.6-7.1                             --
-- |  haskell-x11                   -> 1.5.0.0-7.2                           --
-- |  haskell-x11-xft               -> 0.3-19.2                              --
-- |  xmonad-darcs                  -> 20110331-1                            --
-- |  xmonad-contrib-darcs          -> 20110331-1                            --
-- |  dzen2 (svn)                   -> 271-1                                 --
-------------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}

-- IMPORTS {{{

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio
import System.IO
import System.Exit

-- <actions>
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.CycleWS (nextWS,prevWS,toggleWS,shiftToNext,shiftToPrev)
import XMonad.Actions.DwmPromote
import XMonad.Actions.RotSlaves (rotAllUp,rotAllDown,rotSlavesDown,rotSlavesUp)
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.FloatKeys (keysMoveWindow,keysResizeWindow)
import XMonad.Actions.WithAll
import XMonad.Actions.Submap

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
import XMonad.Prompt.XMonad
import XMonad.Prompt.Shell
import XMonad.Prompt.AppLauncher
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.Man (manPrompt)
import XMonad.Prompt.Window (windowPromptBring,windowPromptGoto)

-- <layouts>
import XMonad.Layout.OneBig
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile

-- <layout helpers>
import XMonad.Layout.Minimize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Combo
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Layout.Reflect
import XMonad.Layout.Master
import XMonad.Layout.CenteredMaster
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
      , borderWidth         = 3 -- for floating windows ((..)Borders `Or` withBorder Int $ on Layouts)
      , focusFollowsMouse   = False
      }
myStartHook = spawnOnce "/bin/zsh -c \"source /howl/conf/.xmonad/dzen4xmonad\"" <+>
              setDefaultCursor xC_left_ptr <+>
              ewmhDesktopsStartup >> setWMName "LG3D"

-- end of MAIN-CONFIGURATION }}}





-- COLORS, FONTS, AND PROMPTS {{{

-- <colors>
colorBlack          = "#000000"
colorBlackAlt       = "#040404"
colorGray           = "#444444"
colorGrayAlt        = "#282828"
colorDarkGray       = "#161616"
colorWhite          = "#cfbfad"
colorWhiteAlt       = "#8c8b8e"
colorDarkWhite      = "#606060"
colorCream          = "#a9a6af"
colorDarkCream      = "#5f656b"
colorMagenta        = "#a488d9"
colorMagentaAlt     = "#7965ac"
colorDarkMagenta    = "#8e82a2"
colorBlue           = "#98a7b6"
colorBlueAlt        = "#598691"
colorDarkBlue       = "#464a4a"
colorNormalBorder   = colorDarkWhite
colorFocusedBorder  = colorMagenta

-- <font>
barFont = "-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r"

-- <tab-bar configuration>
myTabTheme =
    defaultTheme { fontName            = barFont
                 , inactiveBorderColor = colorGrayAlt
                 , inactiveColor       = colorDarkGray
                 , inactiveTextColor   = colorGrayAlt
                 , activeBorderColor   = colorGrayAlt
                 , activeColor         = colorDarkMagenta
                 , activeTextColor     = colorDarkGray
                 , urgentBorderColor   = colorBlackAlt
                 , urgentTextColor     = colorWhite
                 , decoHeight          = 14
                 }

-- <prompts>
myXPConfig :: XPConfig
myXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorMagenta
                    , bgHLight              = colorDarkMagenta
                    , fgHLight              = colorDarkGray
                    , borderColor           = colorBlackAlt
                    , promptBorderWidth     = 1
                    , height                = 15
                    , position              = Top
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
    , gs_font         = barFont
    }

-- <scratchpad>
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect (1/6) (1/4) (2/3) (2/5))
scratchPad = scratchpadSpawnActionCustom "urxvt -name scratchpad +sb -fn '-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r' -fb '-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r' -fi '-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r' -tn 'rxvt-256color' -cr '#a488d9' -e tmux -S /tmp/.${UID}/tmux/sp"

-- end of UTILITY FUNCTIONS }}}





-- LAYOUTS {{{

myLayouts = avoidStruts $ windowNavigation  $
--            gaps [(U,15)] $ minimize        $
            minimize                        $
            mkToggle (single NBFULL)        $
            mkToggle (single REFLECTX)      $
            mkToggle (single REFLECTY)      $
            mkToggle (single NOBORDERS)     $
            onWorkspace "1" favorLayout     $
            onWorkspace "4" inetLayouts     $
            onWorkspace "5" fotoLayouts     $
            (collectiveLayouts)
  where
    collectiveLayouts = myOneB ||| myTile ||| myFull

    -- <define layouts>
    myFull = named "*" (smartBorders (noBorders Full))
    myOneB = named "@" (smartBorders (withBorder 1 (limitWindows 10 (OneBig 0.75 0.65))))
    myTile = named "+" (lessBorders (OnlyFloat) (withBorder 1 (limitWindows 5 (ResizableTall 1 0.03 0.5 []))))
    myUniq = named "=" (toggleLayouts (lessBorders (OnlyFloat) (noBorders (topRightMaster (mastered 0.01 0.4 $ (tabbedAlways shrinkText myTabTheme)))))
                                      (lessBorders (OnlyFloat) (noBorders (mastered 0.01 0.4 $ tabbedAlways shrinkText myTabTheme))))

    -- <layouts per workspace>
    favorLayout = myUniq
    inetLayouts = myOneB ||| myFull
    fotoLayouts = myFull ||| myTile

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
    , [name         =? n     --> doFullFloat    |   n   <- myTrueFLN] -- float true fullscreen by name
    ]) <+> manageScratchPad
    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
        -- <<class>>
        myFloatsC = ["MPlayer","Save As...","Downloads","xskat"]
        myInetC   = ["Minefield","Firefox","Jumanji","Dwb","Surf","luakit"]
        myFotoC   = ["Gliv","sxiv"]
        myElseC   = ["xskat"]
        -- <<resource>>
        myIgnores = ["desktop","desktop_window"]
        -- <<name>>
        myFloatSN = ["gcolor2"]
        myFloatCN = ["Add-ons"]
        myTrueFLN = ["GLiv in fullscreen"]

-- <statusbar/logging>
myStatusBar = "dzen2 -x '0' -y '0' -h '14' -w '260' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorCream ++ "' -fn '" ++ barFont ++ "'"
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorBlueAlt    colorDarkGray . hideScratchpad
      , ppVisible           =   dzenColor colorCream      colorDarkGray . hideScratchpad
      , ppHidden            =   dzenColor colorDarkCream  colorDarkGray . hideScratchpad
      , ppHiddenNoWindows   =   dzenColor colorGray       colorDarkGray . hideScratchpad
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
                                    "Minimize *" -> "*"
                                    "Minimize -" -> "-"
                                    "Minimize =" -> "="
                                    "Minimize +" -> "+"
                                    "Minimize %" -> "%"
                                    "Minimize @" -> "@"
                                    "Minimize #" -> "#"
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
    [ ((modMask .|. shiftMask,     xK_q         ), unsafeSpawn "pkill -9 -f '/bin/zsh -c source /howl/conf/.xmonad/dzen4xmonad'" >> io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask,     xK_r         ), restart "xmonad" True)
    , ((modMask .|. controlMask,   xK_r         ), unsafeSpawn "xmonad --recompile && xmonad --restart")
    , ((modMask,                   xK_b         ), refresh)
    , ((modMask .|. controlMask,   xK_x         ), kill)
    -- <prompts/utils>
    , ((modMask,                   xK_semicolon ), shellPrompt myXPConfig)
    , ((modMask .|. shiftMask,     xK_semicolon ), manPrompt myXPConfig)
    , ((modMask .|. controlMask,   xK_semicolon ), xmonadPrompt myXPConfig)
    , ((0,                         xK_F1        ), windowPromptGoto myXPConfig)
    , ((0,                         xK_F2        ), windowPromptBring myXPConfig)
    , ((0,                         xK_F3        ), AL.launchApp myXPConfig "ddg")
    , ((0,                         xK_F4        ), appendFilePrompt myXPConfig "/howl/othe/.TODO_now")
    , ((modMask,                   xK_g         ), goToSelected $ myGSConfig myColorizer)
    , ((modMask .|. shiftMask,     xK_g         ), bringSelected $ myGSConfig myColorizer)
    -- <common programs>
    , ((modMask,                   xK_Escape    ), safeSpawnProg "banishmouse")
    , ((modMask,                   xK_Print     ), unsafeSpawn "import -window root /howl/foto/shot/$(date +%Y_%m_%d-%H%M%S).png")
    , ((modMask .|. shiftMask,     xK_Delete    ), unsafeSpawn "alock -bg image:file=/howl/foto/wall/beheading.jpg -cursor glyph -auth pam >&/dev/null")
    , ((modMask,                   xK_Return    ), unsafeSpawn "urxvt -e tmux -S /tmp/.${UID}/tmux/xorg")
    , ((modMask .|. shiftMask,     xK_Return    ), safeSpawnProg $ XMonad.terminal conf)
    , ((modMask,                   xK_grave     ), scratchPad)
    , ((modMask,                   xK_f         ), submap . M.fromList $ -- frequently used programs [sub-bindings]
                                [ ((0, xK_l       ), runOrRaise "luakit" (className =? "luakit"))
                                , ((0, xK_m       ), runInTerm "" "tmux -S /tmp/.${UID}/tmux/xorg new-session 'mutt -F $XDG_CONFIG_DIR/mutt/muttrc'")
                                , ((0, xK_w       ), safeSpawnProg "wallie")
                                , ((0, xK_x       ), unsafeSpawn "xskat -opt $XDG_CONFIG_DIR/xorg/xskat.opt -list $XDG_CONFIG_DIR/xorg/xskat.lst")
                                ])
    -- <function/media keys>
    , ((0,                         0x1008ff13   ), unsafeSpawn "ossvol --increase 1 --quiet")
    , ((0,                         0x1008ff11   ), unsafeSpawn "ossvol --decrease 1 --quiet")
    , ((0,                         0x1008ff12   ), unsafeSpawn "ossvol --mute --quiet")
    , ((modMask .|. shiftMask,     xK_e         ), safeSpawnProg "eject")
    , ((modMask .|. shiftMask,     xK_d         ), AL.launchApp myXPConfig "mifo --command")
    , ((modMask,                   xK_d         ), submap . M.fromList $ -- mplayer daemon [sub-bindings] (mplayer fifo script)
                                [ ((0, xK_d       ), unsafeSpawn "sudo /etc/rc.d/mifo start")
                                , ((0, xK_t       ), safeSpawn "mifo" ["--toggle"])
                                , ((0, xK_r       ), safeSpawn "mifo" ["--random"])
                                , ((0, xK_l       ), safeSpawn "mifo" ["--next"])
                                , ((0, xK_h       ), safeSpawn "mifo" ["--prev"])
                                , ((0 .|. shiftMask, xK_l ), AL.launchApp myXPConfig "mifo --next")
                                , ((0 .|. shiftMask, xK_h ), AL.launchApp myXPConfig "mifo --prev")
                                , ((0, xK_j       ), unsafeSpawn "mifo --next dir")
                                , ((0, xK_k       ), unsafeSpawn "mifo --prev dir")
                                , ((0, xK_s       ), safeSpawn "mifo" ["--stop"])
                                , ((0, xK_q       ), unsafeSpawn "sudo /etc/rc.d/mifo stop")
                                , ((0 .|. shiftMask, xK_q ), unsafeSpawn "sudo /etc/rc.d/mifo kill")
                                , ((0, xK_f       ), safeSpawn "mifo" ["--fullscreen"])
                                , ((0 .|. shiftMask, xK_s ), AL.launchApp myXPConfig "mifo --save")
                                , ((0, xK_a       ), AL.launchApp myXPConfig "mifo --load")
                                , ((0, xK_p       ), AL.launchApp myXPConfig "mifo --playlist")
                                , ((0 .|. shiftMask, xK_a ), AL.launchApp myXPConfig "mifo --append")
                                , ((0, xK_equal   ), safeSpawn "mifo" ["--fav-add"])
                                , ((0, xK_minus   ), safeSpawn "mifo" ["--fav-delete"])
                                , ((0, xK_Return  ), AL.launchApp myXPConfig "mifo --reload")
                                ])
    , ((modMask, xK_s       ), submap . M.fromList $ -- seek/navigation [sub-bindings]
                                                [ ((0, xK_l                 ), unsafeSpawn "mifo -c seek 15")
                                                , ((0, xK_h                 ), unsafeSpawn "mifo -c seek -17")
                                                , ((0 .|. shiftMask,   xK_l ), unsafeSpawn "mifo -c seek 45")
                                                , ((0 .|. shiftMask,   xK_h ), unsafeSpawn "mifo -c seek -47")
                                                , ((0 .|. controlMask, xK_l ), unsafeSpawn "mifo -c seek 405")
                                                , ((0 .|. controlMask, xK_h ), unsafeSpawn "mifo -c seek -407")
                                                , ((0 .|. shiftMask,   xK_1 ), unsafeSpawn "mifo -c seek_chapter -1")
                                                , ((0 .|. shiftMask,   xK_2 ), unsafeSpawn "mifo -c seek_chapter 1")
                                                , ((0, xK_BackSpace         ), unsafeSpawn "mifo -c seek 0 1")
                                                ])
    -- <tiled windows>
    , ((modMask,                   xK_BackSpace ), withFocused minimizeWindow)
    , ((modMask .|. shiftMask,     xK_BackSpace ), sendMessage RestoreNextMinimizedWin)
    , ((modMask,                   xK_equal     ), sendMessage $ IncMasterN 1)
    , ((modMask,                   xK_minus     ), sendMessage $ IncMasterN (-1))
    , ((modMask,                   xK_0         ), sendMessage $ Expand)
    , ((modMask,                   xK_9         ), sendMessage $ Shrink)
    , ((modMask .|. shiftMask,     xK_0         ), sendMessage $ MirrorShrink)
    , ((modMask .|. shiftMask,     xK_9         ), sendMessage $ MirrorExpand)
    , ((modMask,                   xK_j         ), sendMessage $ Go D)
    , ((modMask,                   xK_k         ), sendMessage $ Go U)
    , ((modMask,                   xK_h         ), sendMessage $ Go L)
    , ((modMask,                   xK_l         ), sendMessage $ Go R)
    , ((modMask .|. shiftMask,     xK_j         ), sendMessage $ Swap D)
    , ((modMask .|. shiftMask,     xK_k         ), sendMessage $ Swap U)
    , ((modMask .|. shiftMask,     xK_h         ), sendMessage $ Swap L)
    , ((modMask .|. shiftMask,     xK_l         ), sendMessage $ Swap R)
    , ((modMask .|. controlMask,   xK_j         ), rotSlavesDown)
    , ((modMask .|. controlMask,   xK_k         ), rotSlavesUp)
    , ((modMask,                   xK_m         ), windows W.focusMaster)
--    , ((modMask .|. shiftMask,     xK_m         ), windows W.swapMaster)
--    , ((modMask,                   xK_Tab       ), rotAllDown)
--    , ((modMask .|. shiftMask,     xK_Tab       ), rotAllUp)
--    , ((modMask,                   xK_n         ), windows W.focusDown)
--    , ((modMask,                   xK_p         ), windows W.focusUp)
--    , ((modMask .|. controlMask,   xK_n         ), windows W.swapDown)
--    , ((modMask .|. controlMask,   xK_p         ), windows W.swapUp)
--    , ((modMask,                   xK_space     ), sendMessage NextLayout)
--    , ((modMask .|. shiftMask,     xK_space     ), setLayout $ XMonad.layoutHook conf)
    , ((modMask .|. shiftMask,     xK_m         ), bindOn [("1", dwmpromote), ("", windows W.swapMaster)])
    , ((modMask,                   xK_Tab       ), bindOn [("1", rotSlavesUp), ("", rotAllDown)])
    , ((modMask .|. shiftMask,     xK_Tab       ), bindOn [("1", rotSlavesDown), ("", rotAllUp)])
    , ((modMask,                   xK_n         ), bindOn [("1", rotSlavesUp), ("", windows W.focusDown)])
    , ((modMask,                   xK_p         ), bindOn [("1", rotSlavesDown), ("", windows W.focusUp)])
    , ((modMask .|. controlMask,   xK_n         ), bindOn [("1", rotAllUp), ("", windows W.swapDown)])
    , ((modMask .|. controlMask,   xK_p         ), bindOn [("1", rotAllDown), ("", windows W.swapUp)])
    , ((modMask,                   xK_space     ), bindOn [("1", sendMessage ToggleLayout), ("", sendMessage NextLayout)])
    , ((modMask .|. shiftMask,     xK_space     ), bindOn [("1", withFocused $ windows . W.sink), ("", setLayout $ XMonad.layoutHook conf)])
    -- <floating windows (rarely use these)>
    , ((modMask,                   xK_w         ), withFocused $ windows . W.sink)
    , ((modMask .|. shiftMask,     xK_w         ), sinkAll)
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
    , ((modMask,                   xK_t         ), submap . M.fromList $ -- toggle [sub-bindings]
                                [ ((0, xK_o       ), sendMessage $ XMonad.Layout.MultiToggle.Toggle NBFULL)
                                , ((0, xK_b       ), sendMessage $ XMonad.Layout.MultiToggle.Toggle NOBORDERS)
                                , ((0, xK_s       ), sendMessage $ ToggleStruts)
                                , ((0, xK_g       ), sendMessage $ ToggleGaps)
                                , ((0, xK_d       ), sendMessage $ ToggleGap D)
                                , ((0, xK_u       ), sendMessage $ ToggleGap U)
                                , ((0, xK_x       ), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)
                                , ((0, xK_y       ), sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)
                                ])
    , ((modMask,                   xK_period    ), nextWS)
    , ((modMask,                   xK_comma     ), prevWS)
    , ((modMask,                   xK_slash     ), toggleWS)
    , ((modMask .|. shiftMask,     xK_slash     ), focusUrgent)
    , ((modMask .|. shiftMask,     xK_period    ), shiftToNext)
    , ((modMask .|. shiftMask,     xK_comma     ), shiftToPrev)
    , ((modMask .|. controlMask,   xK_period    ), shiftToNext >> nextWS)
    , ((modMask .|. controlMask,   xK_comma     ), shiftToPrev >> prevWS)
    ]
    ++ -- view Nth workspace (modMask+Int) or send focused window to workspace (modMask+Shift+Int)
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++ -- swap windows from current workspace with another (modMask+Control+Int)
    [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_6]]

-- end of KEY-BINDINGS }}}

-- vim:foldmethod=marker foldmarker={{{,}}} sw=2 sts=2 ts=2 tw=0 et ai nowrap
