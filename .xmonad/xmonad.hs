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

----------------------------------------------------------------

---------------------
-- my applications --
---------------------

myTerminal       = "gnome-terminal"
myBrowser        = "qutebrowser"
myGuiFM          = "nautilus"
myPrintScreen    = "gnome-screenshot"
myGuiTextEditor  = "emacs"

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

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm .|. shiftMask, xK_p     ), spawn "dmenu_run")

    -- launch browser
    , ((modm .|. shiftMask, xK_b     ), spawn myBrowser)

    -- launch file manager
    , ((modm .|. shiftMask, xK_f     ), spawn myGuiFM)

    -- launch GUI text editor
    , ((modm .|. shiftMask, xK_e     ), spawn myGuiTextEditor)

    -- change brightness
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")

    -- volume control
    , ((0, 0x1008ff11), spawn "amixer -q sset Master 2%-")
    , ((0, 0x1008ff12), spawn "amixer -q sset Master toggle")
    , ((0, 0x1008ff12), spawn "amixer -q sset Master toggle && amixer -q sset Headphone toggle")
    , ((0, 0x1008ff13), spawn "amixer -q sset Master 2%+")

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

    -- Restart xmonad
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
mySpacing = spacingRaw True             -- Only for >1 window
                       -- The bottom edge seems to look narrower than it is
                       (Border 5 5 5 5) -- Size of screen edge gaps
                       True             -- Enable screen edge gaps
                       (Border 5 5 5 5) -- Size of window gaps
                       True             -- Enable window gaps

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalBorderColor, myFocusedBorderColor :: [Char]
myNormalBorderColor = "#111111"
myFocusedBorderColor = "#268bd2"





-------------
-- Layouts --
-------------

myLayout = avoidStruts $ mySpacing $ smartBorders (tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100



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
    [ className =? "Gimp"        --> doFloat
    , className =? "Calculator"     --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]


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
                spawnOnce "compton &"
		spawnOnce "nitrogen --restore &"
                spawnOnce "setxkbmap us &"
                spawnOnce "redshift -l 42.652:-73.756 &"
                spawnOnce "duplicati &"



----------
-- MAIN --
----------


main = do
       spawnPipe "xmobar -x 0 /home/john_vm/.config/xmobar/xmobarrc"
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
