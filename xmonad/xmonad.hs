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
 import XMonad.Layout.LayoutCombinators (JumpToLayout (..), (|||))
 import XMonad.Layout.LayoutHints
 import XMonad.Layout.LayoutModifier
 import XMonad.Layout.NoBorders
 import XMonad.Layout.PerWorkspace
 import XMonad.Layout.ResizableTile
 import XMonad.Layout.Tabbed
 import XMonad.Prompt
 import XMonad.Prompt.Input
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

--------------------------------------------------------------------------------------------------------------
--manageHook
--------------------------------------------------------------------------------------------------------------
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
 myLayout = smartBorders $(zoomy XMonad.Layout.LayoutCombinators.||| imLayout XMonad.Layout.LayoutCombinators.||| resizableTile XMonad.Layout.LayoutCombinators.||| Mirror resizableTile XMonad.Layout.LayoutCombinators.||| Full XMonad.Layout.LayoutCombinators.||| Grid)
  where
      zoomy = tabbed shrinkText myTabConfig
      --xoomy   = Tall tallNMaster tallRationIncrement tallRatio 
      --tallNMaster = 1
      --tallRationIncrement   = 1/100
      --tallRatio  = 1/100
-- numMasters, resizeIncr, splitRatio
      resizableTile  = ResizableTall nmaster delta xatio []
-- The default number of windows in the master pane
      nmaster = 1
      -- Percent of screen to increment by when resizing panes
      delta   = 1/100
       -- Default proportion of screen occupied by master pane
      xatio   = 3/5
      -- notice that withIM, which normally acts on one layout, can also 
      -- work on a list of layouts (yay recursive data types!)
      imLayout        =  withIM iratio rosters resizableTile where
       iratio          = 11%100
       rosters        = pidginRoster 
       pidginRoster   = (ClassName "Pidgin") `And` (Role "buddy_list")
       chatLayout     = Grid
 

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
-- myLogHook

  --------------------------------------------------------------------------------------------------------
 -- LogHook Pretty Print Config
 --------------------------------------------------------------------------------------------------------
 -- dynamicLogXinerama :: X ()
 --  dynamicLogXinerama = withWindowSet $ io . putStrLn . XMonad.Hooks.DynamicLog.pprWindowSetXinerama
 -- pprWindowSetXinerama :: WindowSet -> String
 --  pprWindowSetXinerama ws = "[" ++ unwords onscreen ++ "] " ++ unwords offscreen
 --   where onscreen  = map (W.tag . W.workspace)
 --                        . sortBy (comparing W.screen) $ W.current ws : W.visible ws
--         offscreen = map W.tag . filter (isJust . W.stack)
--                        . sortBy (comparing W.tag) $ W.hidden ws

 myPP = def 
           { 
           -- ppOrder = \(ws:_:t:_) -> [ws,t] 
           ppOrder = \(ws:l:t:_) -> [ws,l,t]
           , ppTitle = xmobarColor "green" "" . shorten 20
           , ppSort = mkWsSort getXineramaWsCompare
           --, ppSort = getSortByXineramaPhysicalRule XMonad.Actions.TopicSpace.pprWindowSet myTopicConfig, dynamicLogString . onlyTitle
           , ppExtras  = [windowCount]
           --, ppHidden = xmobarColor "#268bd2" ""
           , ppHidden = xmobarColor "#268bd2" "" . wrap "(" ")". noScratchPad
           , ppCurrent = xmobarColor "#e8f40d" "" . wrap "[" "]"
           , ppVisible = wrap "<" ">"
           , ppLayout            =   (\x -> case x of
                                     "ResizableTall"        ->      "RTall"
                                     "IM ResizableTall"     ->      "IM"
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
 -------------------- Support for per-screen xmobars ---------
 -- Some parts of this should be merged into contrib sometime
 getScreens :: IO [Int]
 getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
     where f = fmap (zipWith const [0..]) . getScreenInfo

 mergePPOutputs :: [PP -> X String] -> PP -> X String
 mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

 onlyTitle :: PP -> PP
 onlyTitle pp = defaultPP { ppCurrent = const ""
                          , ppHidden = const ""
                          , ppVisible = const ""
                          , ppLayout = ppLayout pp
                          , ppTitle = ppTitle pp }


 -------------------- Support for per-screen xmobars ---------
 -- Some parts of this should be merged into contrib sometime
 multiPP :: PP -- ^ The PP to use if the screen is focused
         -> PP -- ^ The PP to use otherwise
         -> [Handle] -- ^ Handles for the status bars, in order of increasing X
                     -- screen number
         -> X ()
 multiPP = multiPP' dynamicLogString

 multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
 multiPP' dynlStr focusPP unfocusPP handles = do
     state <- get
     let pickPP :: WorkspaceId -> WriterT (Last XState) X String
         pickPP ws = do
             let isFoc = (ws ==) . S.tag . S.workspace . S.current $ windowset state
             put state{ windowset = S.view ws $ windowset state }
             out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
             when isFoc $ get >>= tell . Last . Just
             return out
     traverse put . getLast
         =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
         =<< mapM screenWorkspace (zipWith const [0..] handles)
     return ()

 myTopicConfig = TopicConfig
   { topicDirs = M.fromList $
       [ ("a", "./")
       , ("haskell", "haskell")
       , ("xm-conf", ".xmonad")
       , ("xme", "wip/x11-wm/xmonad/extras/xmonad-extras/XMonad")
       , ("xm", "wip/x11-wm/xmonad/core/xmonad")
       , ("xmc", "wip/x11-wm/xmonad/contrib/XMonadContrib/XMonad")
       , ("xmobar", "wip/x11-wm/xmobar")
       , ("movie", "media/movie")
       , ("music", "media/music")
       , ("doc", "doc")
       , ("pdf", "ref")
       , ("gitit", "wip/gitit")
       , ("gimp", "./")
       , ("wip", "wip")
       ]
   , defaultTopicAction = const $ spawnShell >*> 2
   , defaultTopic = "a"
   , maxTopicHistory = 10
   , topicActions = M.fromList $
       [ ("xm-conf", spawnShellIn ".xmonad/lib/XMonad/Layout" >>
                         spawn "urxvt -e vim ~/.xmonad/xmonad.hs")
        , ("xmc"    , spawnShell >*> 2)
        , ("xmobar" , spawnShellIn "wip/x11-wm/xmobar/Plugins" >*> 2)
        , ("music"  , spawn "urxvt -e ncmpc -h /home/aavogt/.mpd/socket" >> spawn "export MPD_HOST=192.168.1.2; mpc && urxvt -e ncmpc -h 192.168.1.2")
        , ("mail"   , spawnOn "mail" "urxvt -e mutt")
        , ("irc"    , spawnOn "irc" "urxvt --title irc -e ssh engage")
        , ("web"    , spawnOn "web" "firefox")
        , ("pdf"    , spawnOn "pdf" "okular")
        , ("gimp"   , spawnHere "gimp")
       ]
   }



--------------------------------------------------------------------------------------------------------
-- Varibles
--------------------------------------------------------------------------------------------------------
 spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

 spawnShellIn dir = do
     -- color <- randomBg' (HSV 255 255)
     t <- asks (terminal . config)
     spawnHere $ "cd " ++ dir ++ " && " ++ t -- ++ " -bg " ++ color

 myWorkspaces :: [Topic]
 myWorkspaces = ["1:S","2:W","3:M","4:I","5:E","6","7","8","9"]
 myBitmapsDir        = "/home/tgooch/.xmonad/resources"
 myNormalBorderColor = "#1B1D1E"
 myFocusedBorderColor = "#00ffff"
-- Kill zombie dzens before normal xmonad restart
 myRestart = "pkil xmobar; pkill xmobar; pkill stalonetray  && xmonad --recompile && xmonad --restart"
 myTerminal  = "urxvt"
-- Main
--------------------------------------------------------------------------------------------------------
-- myppHidden = "#268bd2"
-- myppCurrent = "#cb4b16"
 windowCount = gets $ Just . show . length . S.integrate' . S.stack . S.workspace . S.current . windowset
 warpToCentre = gets (S.screen . S.current . windowset) >>= \x -> warpToScreen x  0.5 0.5
 main = do
    xmprocS <- spawnPipe "/usr/bin/python ~/newsun.py > /home/tgooch/sun.pipe"
    xmprocX <- spawnPipe "echo 'September 22, 2020 9:30 Equinox' > marquee.pipe"
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
                ,logHook = ewmhDesktopsLogHook >> dynamicLogWithPP myPP
                      {
                       ppOutput = \x -> hPutStrLn xmproc1 x >> hPutStrLn xmproc0 x >> hPutStrLn xmproc2 x
                      }
                , XMonad.workspaces = myWorkspaces
                --, logHook = do
                --    multiPP'
                --       (mergePPOutputs [XMonad.Actions.TopicSpace.pprWindowSet myTopicConfig,
                --                        dynamicLogString . onlyTitle])
                --       myPP
                --       myPP{ ppTitle = const ""
                --           , ppOutput = \x -> hPutStrLn h x >> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                --           }
                    --       let 
                    --updatePointer (0, 0) (0, 0)
       } `additionalKeys`
       [ 
         ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
       , ((controlMask, xK_Print), spawn "sleep 0.5; scrot -s '%Y-%m-%d-%H%M-%S_$wx$h_scrot.png' -e 'mv $f /home/tgooch/images/'")
       , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%H%M-%S_$wx$h_scrot.png' -e 'mv $f /home/tgooch/images/'")
       , ((controlMask .|. shiftMask, xK_m), spawn "urxvt -name mutt -e mutt")
       , ((0, 0x1008ff18), spawn "evolution")
       , ((shiftMask, xK_Insert), pasteSelection)
       , ((mod1Mask, xK_q     ), spawn myRestart)
       , ((mod1Mask, xK_s     ), scratchTerm)
       , ((mod1Mask, xK_p     ), shellPromptHere myXPConfig)
       --, ((mod4Mask, xK_t     ), scratchZoom)
       , ((mod4Mask .|. shiftMask, xK_t     ), scratchZoomMain)
       , ((mod4Mask .|. shiftMask, xK_c     ), scratchChatty)
       --, ((mod4Mask, xK_z     ), scratchZoom3)
       , ((mod4Mask, xK_a     ), spawn myANG)
       --, ((mod4Mask .|. shiftMask, xK_x     ), scratchZoom4)
       , ((mod1Mask, xK_v     ), scratchVPN)
       -- volume keys
       , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
       , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
       , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
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
