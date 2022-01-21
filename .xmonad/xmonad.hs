-------------
-- imports --
-------------

-- Basics
import XMonad -- standard library
import System.IO
import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Running scripts
import XMonad.Util.SpawnOnce

-- Key bindings
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86

-- Sizing of floating windows
import XMonad.Hooks.ManageHelpers (doRectFloat)
import qualified XMonad.StackSet as W
import Data.Ratio

-- eliminate unnecessary window borders
import XMonad.Layout.NoBorders (smartBorders)
-- gaps around windows
import XMonad.Layout.Spacing
-- full-screen view automation
import XMonad.Hooks.EwmhDesktops

-- communicating with processes
import XMonad.Util.Run
-- customizing logHook --> xmobar
import XMonad.Hooks.DynamicLog
-- no overlap for windows over xmobar
import XMonad.Hooks.ManageDocks
-- cursor
import XMonad.Util.Cursor
-- window manager name
import XMonad.Hooks.SetWMName

----------------------------------------------------------------

---------------------
-- my applications --
---------------------

myTerminal       = "alacritty"
myBrowser        = "qutebrowser"
myGuiFM          = "thunar"
myPrintScreen    = "$HOME/.xmonad/select_screenshot"
myGuiTextEditor  = "emacsclient -c"
myMail           = "emacsclient -c -a '' --eval '(mu4e)'"

----------------
-- workspaces --
----------------

myWorkspaces :: [String]
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]


------------------
-- Key bindings --
------------------

-- https://www.stackage.org/haddock/lts-13.2/xmonad-0.15/src/XMonad.Config.html#keys

myModMask       = mod4Mask
altMask         = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run -fn monofur-10")

    -- launch dmenu shortcuts
    , ((modm .|. shiftMask, xK_o     ), spawn "$HOME/.xmonad/dmenu_shortcuts")

    -- launch dmenu gtd shortcuts
    , ((modm .|. shiftMask, xK_d     ), spawn "$HOME/.xmonad/dmenu_gtd")

    -- launch dmenu zotero
    , ((modm .|. shiftMask, xK_l    ), spawn "$HOME/.xmonad/dmenu_zotero")

    -- launch dmenu formula sheets
    , ((modm .|. shiftMask, xK_i    ), spawn "$HOME/.xmonad/dmenu_formulas")

    -- launch browser
    , ((modm .|. shiftMask, xK_b     ), spawn myBrowser)

    -- launch file manager
    , ((modm .|. shiftMask, xK_f     ), spawn myGuiFM)

    -- new emacsclient frame
    , ((modm .|. shiftMask, xK_t     ), spawn myGuiTextEditor)

    -- email
    , ((modm .|. shiftMask, xK_m     ), spawn myMail)

    -- suspend
    , ((altMask .|. controlMask, xK_s     ), spawn "systemctl suspend")

    -- change brightness
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")

    -- volume control
    , ((0, 0x1008ff11), spawn "amixer -q sset Master 2%-")
    , ((0, 0x1008ff12), spawn "pactl set-sink-mute 1 toggle")
    , ((0, 0x1008ff13), spawn "amixer -q sset Master 2%+")

    -- media player control
    , ((0, xF86XK_AudioPrev ), safeSpawn "dbus-send" ["--print-reply"," --dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.Previous"] )
    , ((0, xF86XK_AudioNext ), safeSpawn "dbus-send" ["--print-reply", "--dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.Next"] )
    , ((0, xF86XK_AudioPlay ), safeSpawn "dbus-send" ["--print-reply", "--dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.PlayPause"] )

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- change keyboard layout
    , ((mod1Mask,  xK_space   ),      spawn "/home/john_vm/.xmonad/keyboard_layout_switch.sh")

    -- print screen
    , ((0,  xK_Print), spawn myPrintScreen)
    
    -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_j     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapUp    )

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

    -- Quit, Restart xmonad
    , ((controlMask .|. altMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
  
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

--------------------
-- Mouse bindings --
--------------------

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




-----------------------
-- window aisthetics --
-----------------------

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Gaps around and between windows
-- Changes applied after relogging in
-- Dimensions are given as (Border top bottom right left)
mySpacing1 = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 7 7 7 7) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 7 7 7 7) -- Size of window gaps
                       True             -- Enable window gaps

-- zoom screen sharing controls...
mySpacing2 = spacingRaw False
                       (Border 80 7 7 7)
                       True
                       (Border 7 7 7 7)
                       True

mySpacing = mySpacing1

myBorderWidth :: Dimension
myBorderWidth = 4

myNormalBorderColor, myFocusedBorderColor :: [Char]
myNormalBorderColor = "#111111"
myFocusedBorderColor = "#268bd2"





-------------
-- Layouts --
-------------

myLayout = avoidStruts $ mySpacing $ smartBorders (tiled ||| tiled_h ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     tiled_h = Mirror $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100



------------------
-- Window rules --
------------------

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
    [ className =? "Gnome-calculator" --> doFloat
    , className =? "vlc"              --> doFloat
    , className =? "Gimp-2.10"        --> doFloat
    , className =? "gimp-2.10"        --> doFloat
    , className =? "gimp"             --> doFloat
    , className =? "Gimp"             --> doFloat
    , className =? "Matplotlib"       --> doFloat
    , className =? "matplotlib"       --> doFloat
    , className =? "XClock"           --> doFloat
    , className =? "zoom"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore ]


--------------------
-- Event handling --
--------------------

myEventHook = mempty


-----------------------------
-- Status bars and logging --
-----------------------------

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()


------------------
-- Startup hook --
------------------

myStartupHook = do
                spawnOnce "xrandr --dpi 122"
                spawnOnce "setxkbmap us &"
                spawnOnce "xmodmap -e \"add mod3 = Scroll_Lock\""
                spawnOnce "thunar --daemon"
                spawnOnce "picom &"
		spawnOnce "nitrogen --restore &"
                spawnOnce "redshift &"
                spawnOnce "offlineimap &"
                spawnOnce "emacs -bg black --daemon &"
                setWMName "LG3D"
                setDefaultCursor xC_left_ptr
                spawnOnce "sleep 10 && firefox -new-window https://piazza.com/class/kym2za609j41l0"

----------
-- MAIN --
----------

main = do
       spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
       xmonad $ docks defaults


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
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
