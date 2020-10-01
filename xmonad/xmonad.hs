 import Control.Applicative
 import Control.Monad
 import Control.Monad.Instances ()
 import Control.Monad.Writer
 import Data.Default
 import Data.List
 import Data.Maybe
 import Data.Ratio ((%))   
 import Data.Traversable(traverse)
 import Graphics.X11.ExtraTypes.XF86
 import Graphics.X11.Xinerama
 import Graphics.X11.Xlib
 import qualified Data.Map    as M
 import qualified Data.Map as Mz
 import qualified XMonad.StackSet as S
 import qualified XMonad.Util.ExtensibleState as XS
 import System.Exit
 import System.IO
 import XMonad
 import XMonad.Actions.CycleWS
 import XMonad.Actions.DwmPromote
 import XMonad.Actions.DynamicWorkspaces
 import XMonad.Actions.FloatSnap
 import XMonad.Actions.GridSelect
 import XMonad.Actions.PhysicalScreens
 import XMonad.Actions.Search
 import XMonad.Actions.SpawnOn
 import XMonad.Actions.Submap
 import XMonad.Actions.TopicSpace
 import XMonad.Actions.UpdatePointer
 import XMonad.Actions.Warp
 import XMonad.Actions.Workscreen
 import XMonad.Hooks.DynamicHooks
 import XMonad.Hooks.DynamicLog
 import XMonad.Hooks.EwmhDesktops
 import XMonad.Hooks.FadeInactive       as FI
 import XMonad.Hooks.ManageDocks
 import XMonad.Hooks.ManageHelpers
 import XMonad.Hooks.UrgencyHook
 import XMonad.Layout.Grid
 import XMonad.Layout.IM
 import XMonad.Layout.LayoutCombinators
-- import XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
 import XMonad.Layout.LayoutHints
 import XMonad.Layout.LayoutModifier
 import XMonad.Layout.NoBorders
 import XMonad.Layout.TwoPane
 import XMonad.Layout.PerWorkspace
 import XMonad.Layout.ResizableTile
 import XMonad.Layout.Tabbed
 import XMonad.Prompt
 import XMonad.Prompt.Input
 import XMonad.Prompt.AppendFile
 import XMonad.Prompt.RunOrRaise
 import XMonad.Prompt.Ssh
 import XMonad.Prompt.Window
 import XMonad.Prompt.XMonad
 import XMonad.Util.EZConfig
 import XMonad.Util.NamedScratchpad
 import XMonad.Util.Paste
 import XMonad.Util.Replace
 import XMonad.Util.Run
 import XMonad.Util.Scratchpad
 import XMonad.Util.WindowProperties
 import XMonad.Util.WorkspaceCompare 


--SCRATCHPAD NAMED-- one scratchpad
 myScratchPads = [ NS "vpnu2" spawnVPN findVPN manageVPN
                 , NS "terminal" spawnTerm  findTerm  manageTerm  -- and a second
                 --, NS "zoom"     spawnZoom  findZoom  manageZoom  -- and a second
                 , NS "zoomMain"     spawnZoomMain  findZoomMain  manageZoomMain  
                 , NS "chatty"     spawnChatty findChatty  manageChatty  
                 --, NS "zoomMeeting"     spawnZoomMeeting  findZoomMeeting  manageZoomMeeting -- and a second
                 --, NS "zoomv"     spawnZoomv  findZoomv  manageZoomv -- and a second
                ]

  where
---{{{ 117 Comment lines
    spawnZoomMain  = "zoom"                          -- launch my mixer
    findZoomMain   = stringProperty "WM_NAME" =? "Zoom - Licensed Account"                  -- its window has a ClassName of "Ossxmix"
--Zoom - Licensed Account

-- Zoom normal WM_NAME(STRING) = "Zoom - Licensed Account"
-- Zoom Controls WM_NAME(STRING) = "Zoom Meeting"

    manageZoomMain = customFloating $ S.RationalRect l t w h -- and I'd like it fixed using the geometry below:

      where

        h = 0.7       -- height, 90%
        w = 0.7       -- width, 90% 
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right
--------------------------------------------------------------------------------------------------------------
---}}}
    spawnVPN  = "vpnui"                               -- launch my mixer
    findVPN   = className =? "Cisco AnyConnect Secure Mobility Client"                  
    manageVPN = customFloating $ S.RationalRect l t w h -- and I'd like it fixed using the geometry below:
      where
        h = 0.6       -- height, 60% 
        w = 0.6       -- width, 60% 
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right
--------------------------------------------------------------------------------------------------------------
    spawnTerm  = myTerminal ++ " -name scratchpad"       -- launch my terminal
    findTerm   = resource  =? "scratchpad"               -- its window will be named "scratchpad" (see above)
    manageTerm = customFloating $ S.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 0.1       -- height, 10% 
        w = 1         -- width, 100%
        t = 1 - h     -- bottom edge
        l = (1 - w)/2 -- centered left/right
--------------------------------------------------------------------------------------------------------------
    spawnChatty = "pidgin"
    findChatty = stringProperty "WM_WINDOW_ROLE"=?"conversation"
    manageChatty = customFloating $ S.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = 0.4       -- height, 10% 
        w = 0.6         -- width, 100%
        t = (1 - h)/2     -- bottom edge
        l = (1 - w)/2 -- centered left/right

----------------------------------------------------------------------------------------------------
--manageHook
----------------------------------------------------------------------------------------------------
---- Avoid changing master on new window creation
 avoidMaster :: S.StackSet i l a s sd -> S.StackSet i l a s sd
 avoidMaster = S.modify' $ \c -> case c of
  S.Stack t [] (r:rs) -> S.Stack t [r] rs
  otherwise           -> c
-- myManageHook = manageScratchPad <+> composeAll
 myManageHook =  manageDocks <+> composeAll
    ([ className =? "Vncviewer" --> doShift "9"
    , className  =? "sun-awt-X11-XFramePeer" --> doFloat
    , className  =? "sun-awt-X11-XFramePeer" --> doFloat
    , className  =? "Pidgin"    --> doShift "4:I"
    , classNotRole ("Pidgin","roster")    --> nachoCheese
    , classNotRole ("Pidgin","roster")    --> doFloat
    , className  =? "term-4"    --> doShift "4:I"
    , className  =? "Slack"    --> doShift "4:I"
    , stringProperty "WM_NAME" =? "Zoom" <&&> className =? "zoom" --> doShift "7"
    , className  =? "Firefox"   --> doShift "2:W"
    , className  =? "Chromium"   --> doShift "2:W"
    , resource   =? "mutt"       --> doShift "3:M"
    , className  =? "Evolution" --> doShift "5:E"
    , className  =? "com-netscape-management-client-console-Console" --> doFloat
    , stringProperty "WM_ICON_NAME" =? "All your base are belong to ME - Mozilla Firefox"              --> doShift "4:I"
--    , title =? "Zoom  - Licensed Account" --> doShift "2:W"
--    , title =? "Zoom  - Licensed Account" --> doShift "2:W"
    ]) <+> (fmap not isDialog --> doF avoidMaster) <+> manageDocks <+> namedScratchpadManageHook myScratchPads
    where
        classNotRole :: (String, String) -> XMonad.Query Bool
        classNotRole (c,r) = className =? c <&&> role /=? r
        role = stringProperty "WM_WINDOW_ROLE"
 nachoCheese = customFloating $ S.RationalRect l t w h 
     -- and I'd like it fixed using the geometry below
  where
 -- -- reusing these variables is ok since they're confined to their own 
 -- -- where clauses 
        h = 0.6       -- height, 60% 
        w = 0.6       -- width, 60% 
        t = (1 - h)/2 -- centered top/bottom
        l = (1 - w)/2 -- centered left/right



 myTabConfig = def { activeColor         = "#002b36"
                   , inactiveColor       = "#586e75"
                   , urgentColor         = "#cb4b16"
                   , activeBorderColor   = "#6c71c4"
                   , inactiveBorderColor = "#268bd2"
                   , urgentBorderColor   = "#cb4b16"
                   , activeTextColor     = "#6c71c4"
                   , inactiveTextColor   = "#b58900"
                   , urgentTextColor     = "#cb4b16"
                   , fontName = "xft:roboto condensed:size=14:antialias=true"
                   }
----------------------------------------------------------------------------------------------       
-- Set up the Layout Hook
----------------------------------------------------------------------------------------------       
-- myLayout = onWorkspace "4:I" imLayout $ smartBorders $(resizableTile XMonad.Layout.LayoutCombinators.||| Mirror resizableTile XMonad.Layout.LayoutCombinators.||| Full)
 myLayout = smartBorders $(resizableTile XMonad.Layout.LayoutCombinators.||| zoomy XMonad.Layout.LayoutCombinators.||| imLayout XMonad.Layout.LayoutCombinators.||| power XMonad.Layout.LayoutCombinators.||| Grid XMonad.Layout.LayoutCombinators.||| v50 XMonad.Layout.LayoutCombinators.||| h50)
  where
   zoomy         = noBorders (tabbed shrinkText myTabConfig)
   power         = noBorders (Full)
   v50           = TwoPane zdelta zratio
   h50           = Mirror v50
   zdelta        = 3/100
   zratio        = 1/2
   grid          = Grid
-- numMasters, resizeIncr, splitRatio
   resizableTile = ResizableTall nmaster delta xatio []
   nmaster = 1     -- The default number of windows in the master pane
   delta   = 1/100 -- Percent of screen to increment by when resizing panes
   xatio   = 3/5   -- Default proportion of screen occupied by master pane
                   -- notice that withIM, which normally acts on one layout, can also 
                   -- work on a list of layouts (yay recursive data types!)
   imLayout      =  withIM iratio pidginRoster grid where
    iratio       = 11%100
    pidginRoster = (ClassName "Pidgin") `And` (Role "buddy_list")
    chatLayout   = Grid
 

----------------------------------------------------------------------------------------------       
-- Set up the Layout prompt
----------------------------------------------------------------------------------------------       
 myLayoutPrompt :: X ()
 myLayoutPrompt = inputPromptWithCompl myXPConfig "Layout"
                (mkComplFunFromList' allLayouts) ?+ (sendMessage . JumpToLayout)
  where
   allLayouts = ["tall", "wide", "circle", "full", "grid", "tabbed", "zoomy", "xoomy"]
 myXPConfig :: XPConfig
 myXPConfig = greenXPConfig { position = CenteredAt 0.5 0.5
                            , font = "xft:roboto condensed:size=14:antialias=true" }
----------------------------------------------------------------------------------------------       
-- myLogHook
----------------------------------------------------------------------------------------------       
 myPP = def 
           { 
           ppOrder     = \(ws:_:t:_) -> [ws,t] 
           , ppTitle   = xmobarColor "#1ABC9C" "" . shorten 20
           , ppSort    = getSortByXineramaPhysicalRule def
           , ppSep     = "]["
           , ppExtras  = [windowCount]
           , ppHidden  = xmobarColor "#268bd2" "" . wrap "(" ")". noScratchPad
           , ppCurrent = xmobarColor "#e8f40d" "" . wrap "[" "]"
           , ppVisible = wrap "<" ">"
           , ppUrgent = xmobarColor "red" "yellow"
           , ppLayout  =   (\x -> case x of 
                               "ResizableTall"        ->      "RTall"
                               "zoomy"                ->      "why"
                               "IM ResizableTall"     ->      "IM"
                               "IM Grid"              ->      "IM"
                               "Mirror ResizableTall" ->      "Mir"
                               "Full"                 ->      "Full"
                               "Simple Float"         ->      "~"
                               "Grid"                 ->      "#"
                               "Tabbed Simplest"      ->      "Tab"
                               "Tall"                 ->      "iX"
                               _                      ->      x
                               )
           }
           where
--    -- then define it down here: if the workspace is NSP then print
--    -- nothing, else print it as-is
            noScratchPad ws = if ws == "NSP" then "" else ws
--------------------------------------------------------------------------------------------------------
-- Varibles
--------------------------------------------------------------------------------------------------------
 spawnShellIn dir = do
     -- color <- randomBg' (HSV 255 255)
     t <- asks (terminal . config)
     spawnHere $ "cd " ++ dir ++ " && " ++ t -- ++ " -bg " ++ color
 myWorkspaces = ["1:S","2:W","3:M","4:I","5:E","6","7","8","9"]
 myBitmapsDir        = "/home/tgooch/.xmonad/resources"
 myNormalBorderColor = "#1B1D1E"
 myFocusedBorderColor = "#00ffff"
 myRestart = "pkil xmobar; pkill xmobar; pkill stalonetray  && xmonad --recompile && xmonad --restart"
 myTerminal  = "urxvt"
--------------------------------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------------------------------
-- myppHidden = "#268bd2"
-- myppCurrent = "#cb4b16"
 windowCount = gets $ Just . show . length . S.integrate' . S.stack . S.workspace . S.current . windowset
 warpToCentre = gets (S.screen . S.current . windowset) >>= \x -> warpToScreen x  0.5 0.5
 main = do
    xmprocS <- spawnPipe "/usr/bin/python ~/newsun.py > /home/tgooch/sun.pipe"
    xmprocX <- spawnPipe "echo 'December 22, 2020 5:02 Solstice' > marquee.pipe"
-- Yo, DumDum you named the files ignore the -x here, the file names are right for your machines
    xmproc0 <- spawnPipe "xmobar -x0 ~/.xmonad/center.xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x1 ~/.xmonad/right.xmobarrc"
    xmproc2 <- spawnPipe "xmobar -x2 ~/.xmonad/left.xmobarrc"
    barT    <- spawnPipe "stalonetray --dockapp-mode"
    xmonad  $ docks $ withUrgencyHook NoUrgencyHook $ def { 
     manageHook = myManageHook -- make sure to include myManageHook definition from above
        <+> manageHook def
     , startupHook = ewmhDesktopsStartup 
     , layoutHook = avoidStruts (myLayout)
     , normalBorderColor  = myNormalBorderColor
     , focusedBorderColor = myFocusedBorderColor
     , terminal   = myTerminal
     , handleEventHook = ewmhDesktopsEventHook 
     --,logHook = myLogHook h
     ,logHook = ewmhDesktopsLogHook >> dynamicLogWithPP  myPP 
       {
         ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x
       }
     , XMonad.workspaces = myWorkspaces
     } `additionalKeys`
       [ 
         ((mod4Mask .|. shiftMask, xK_z)     , spawn "xscreensaver-command -lock")
       , ((controlMask, xK_Print)            , spawn "sleep 0.5; scrot -s '%Y-%m-%d-%H%M-%S_$wx$h_scrot.png' -e 'mv $f /home/tgooch/images/'")
       , ((0, xK_Print)                      , spawn "scrot '%Y-%m-%d-%H%M-%S_$wx$h_scrot.png' -e 'mv $f /home/tgooch/images/'")
       , ((controlMask .|. shiftMask, xK_m)  , spawn "urxvt -name mutt -e mutt")
       , ((0, 0x1008ff18)                    , spawn "evolution")
       , ((shiftMask, xK_Insert)             , pasteSelection)
       , ((mod1Mask, xK_q     )              , spawn myRestart)
       , ((mod1Mask, xK_s     )              , scratchTerm)
       , ((mod1Mask, xK_p     )              , shellPromptHere myXPConfig)
       --, ((mod4Mask, xK_t     ), scratchZoom)
       , ((mod4Mask .|. shiftMask, xK_t     ), scratchZoomMain)
       , ((mod4Mask .|. shiftMask, xK_c     ), scratchChatty)
       --, ((mod4Mask, xK_z     ), scratchZoom3)
       , ((mod4Mask, xK_a     )              , spawn myANG)
       --, ((mod4Mask .|. shiftMask, xK_x     ), scratchZoom4)
       , ((mod1Mask, xK_v     )              , scratchVPN)
       , ((mod4Mask, xK_n     )              , appendFilePrompt myXPConfig "/home/tgooch/xmonadnote")
       -- volume key
       , ((0, xF86XK_AudioMute)              , spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
       , ((0, xF86XK_AudioLowerVolume)       , spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
       , ((0, xF86XK_AudioRaiseVolume)       , spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
       ]
       where 
        scratchVPN  = namedScratchpadAction myScratchPads "vpnu2"
        scratchTerm = namedScratchpadAction myScratchPads "terminal"
        --    scratchZoom = namedScratchpadAction myScratchPads "zoom"
        scratchZoomMain = namedScratchpadAction myScratchPads "zoomMain"
        scratchChatty = namedScratchpadAction myScratchPads "chatty" 
        --    scratchZoom3 = namedScratchpadAction myScratchPads "zoomMeeting"
        --    scratchZoom4 = namedScratchpadAction myScratchPads "zoomv"
        myANG = "angband -mx11 -- -n8"
