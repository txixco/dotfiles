-- Imports

import Data.Monoid
import Data.Ratio

import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Actions.CycleWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_AudioPlay, xF86XK_AudioStop, xF86XK_AudioNext, xF86XK_AudioPrev, xF86XK_HomePage)
  

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.Exit

import MyVariables

scratchpads = 
  [
-- run the preferred term, find it by title, use default floating window placement
    NS "term" (myTerminal ++ " -T scratch") (title =? "scratch") defaultFloating ,

-- run Gnome System Monitor, find it by class name, place it in the floating window
-- 1/6 of screen width from the left, 1/6 of screen height
-- from the top, 2/3 of screen width by 2/3 of screen height
    NS "monitor" "gnome-system-monitor" (className =? "Gnome-system-monitor")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

-- run htop in xterm, find it by title, use default floating window placement
    NS "htop" "xterm -e htop" (title =? "htop") defaultFloating ,

-- run copyq, find it by class name, place it in the floating window
-- 1/6 of screen width from the left, 1/6 of screen height
-- from the top, 2/3 of screen width by 2/3 of screen height
    NS "copyq" "copyq show" (className =? "copyq")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

-- open Oryx in Qutebrowser, find it by title, place it fullscreen
    NS "oryx" "qutebrowser -R --target window https://configure.zsa.io/ergodox-ez/layouts/P5DJE/latest/0" (title =? "Oryx: The ZSA Keyboard Configurator - qutebrowser") 
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

-- run tzclock, find by class name, place it in a floating window
    NS "tzclock" "tzclock" (className =? "Tzclock") nonFloating ,

-- run Joplin in the terminal
    NS "joplin" (myTerminal ++ " -e joplin -T Joplin") (title =? "Joplin")
       (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ] where role = stringProperty "WM_WINDOW_ROLE"

openUrlOnRead  = "~/scripts/openurl.sh -k -e "

-- Get the hostname
hostname = do
  hnFile <- readFile "/etc/hostname"
  return $ lines hnFile !! 0

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altMask   = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
termIcon            = "\xf120"
chatIcon             = "\xf086"
browserIcon     = "\xf269"
documentIcon = "\xf15c"
codingIcon        = "\xf121"
remoteIcon       = "\xe066"
noteIcon             = "\xf249"
syncIcon             = "\xf021"
musicIcon          = "\xf1bc"

myWorkspaces = [ termIcon,chatIcon,browserIcon,documentIcon,codingIcon,remoteIcon,noteIcon,syncIcon,musicIcon ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- some scratchpads
  , ((modm .|. controlMask .|. shiftMask, xK_t), namedScratchpadAction scratchpads "term")
  , ((modm .|. controlMask .|. shiftMask, xK_m), namedScratchpadAction scratchpads "monitor")
  , ((modm .|. controlMask .|. shiftMask, xK_c), namedScratchpadAction scratchpads "tzclock")
  , ((modm .|. controlMask .|. shiftMask, xK_h), namedScratchpadAction scratchpads "htop")
  , ((modm .|. controlMask .|. shiftMask, xK_n), namedScratchpadAction scratchpads "joplin")
  , ((modm .|. controlMask .|. shiftMask, xK_o), namedScratchpadAction scratchpads "oryx")
  , ((modm, xK_Escape), namedScratchpadAction scratchpads "copyq")

     -- screen capturing
   , ((modm, xK_Print), spawn "flameshot gui")

     -- volume key bindings
   , ((0, xF86XK_AudioMute), spawn "pactl set-sink- 0 toggle")
   , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
   , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")

     -- Other multimedia keys
   , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
   , ((0, xF86XK_AudioStop), spawn "playerctl stop")
   , ((0, xF86XK_AudioNext), spawn "playerctl next")
   , ((0, xF86XK_AudioPrev), spawn "playerctl previous")

   , ((0, xF86XK_HomePage), spawn "qutebrowser")

   , ((modm, xK_Up), spawn "playerctl play-pause")
   , ((modm, xK_Down), spawn "playerctl stop")
   , ((modm, xK_Right), spawn "playerctl next")
   , ((modm, xK_Left), spawn "playerctl previous")

    -- launch the menus
    , ((altMask,               xK_space ), spawn myMenu)
    , ((altMask .|. shiftMask, xK_space ), spawn myPowerMenu)

    -- launch gmrun
    , ((modm,               xK_r ), spawn "gmrun")

    -- launch file managers
    , ((modm .|. shiftMask,  xK_e ), spawn "doublecmd")
    , ((modm,                xK_e ), spawn myFilesManager)

    -- close focused window
    , ((modm .|. shiftMask,  xK_c    ), kill)

    -- set the window floating
    , ((altMask .|. modm, xK_w ), withFocused $ (\w -> windows $ W.float w $ W.RationalRect (1/6) (1/6) (2/3) (2/3)))

    -- set the window floating in read mode
    , ((controlMask .|. modm, xK_w ), withFocused (\windowId -> do 
                keysResizeWindow (0, 0) (-1%33, -1%33) windowId
                keysMoveWindowTo (800, 450) (1%2, 1%2) windowId
            ))

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,            xK_Tab ), cycleRecentWindows [xK_Super_L] xK_Tab xK_Tab)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Restart xmobar
    , ((modm .|. altMask , xK_q      ), spawn "killall xmobar; xmonad --restart")

    -- Edit xmonad.hs
    , ((modm .|. controlMask , xK_q  ), spawn (myEditor ++ " ~/.xmonad/xmonad.hs"))

    -- Edit xmobarrc
    , ((modm .|. controlMask .|. altMask, xK_q  ), spawn (myEditor ++ " ~/.config/xmobarrc"))

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- Open Feedly
    --
    , ((modm .|. shiftMask, xK_f     ), spawn (openUrlOnRead ++ "http://feedly.com/"))
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
altLayout = avoidStruts (my3col ||| Full) -- See comments on defLayout
  where 
    my3col  = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio   = 60/100
    delta   = 5/100

partyLayout = avoidStruts (tiled ||| Full)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 75/100
    delta   = 5/100

defLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 50/100

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Vlc"              --> doFloat
    , className =? "Gimp"             --> doFloat
    , className =? "gnome-calculator" --> doFloat
    , className =? "gnome-screenshot" --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , className =? "Signal"           --> doShift chatIcon
    , className =? "teams-for-linux"            --> doShift chatIcon
    , role      =? "browser"          --> doShift browserIcon
    , className =? "Spotify"          --> doShift musicIcon
    , className =? "Nextcloud"        --> doShift syncIcon
    , className =? "Joplin"           --> doShift noteIcon
    , isFullscreen                    --> doFullFloat
    , namedScratchpadManageHook scratchpads ]
    where role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--myLogHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn xmproc }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "/usr/libexec/polkit-gnome-authentication-agent-1"
    spawnOnce "dunst"
    spawnOnce myWallpaper
    spawnOnce myBrowser
    spawnOnce "teams-for-linux"
    spawnOnce "flatpak run org.signal.Signal"
    spawnOnce "doublecmd"
    spawnOnce "autokey-gtk"
    spawnOnce "copyq"
    spawnOnce "compton"
    spawnOnce "nm-applet"
    spawnOnce "volumeicon"
    spawnOnce "flatpak run com.spotify.Client"
    spawnOnce "setxkbmap us intl"
    --spawnOnce "setxkbmap us dvorak-intl"
    spawnOnce "syncthing --no-browser"
    spawnOnce "nextcloud"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe myBar
    xmonad $ docks $ defaults { logHook = dynamicLogWithPP $ def { ppOutput = hPutStrLn xmproc } }
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ onWorkspace browserIcon altLayout $ onWorkspace remoteIcon partyLayout $ defLayout,
        -- layoutHook         = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ defLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch the terminal",
    "Alt-Space        Launch the menu",
    --"mod-r            Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-Alt-q    Restart xmobar",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
