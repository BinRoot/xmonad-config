import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Prompt.DirExec

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 

myKeys c = mkKeymap c $
           [ ("M-<Return>",   spawn $ XMonad.terminal c)
           , ("M-<Space>",    sendMessage NextLayout)
           , ("M-<Tab>",      windows W.focusDown)
           , ("M-S-<Return>", windows W.swapMaster)
           , ("M-,",          sendMessage (IncMasterN 1))  
           , ("M-.",          sendMessage (IncMasterN (-1)))    
           , ("M-S-c",        kill)
           , ("M-S-q",        io (exitWith ExitSuccess))
           , ("M-b",          sendMessage ToggleStruts)
           , ("M-h",          sendMessage Shrink)
           , ("M-l",          sendMessage Expand)
           , ("M-n",          refresh)
           , ("M-q",          broadcastMessage ReleaseResources >> restart "xmonad" True)
           , ("M-t",          withFocused $ windows . W.sink)
           , ("M-x",          shellPrompt defaultXPConfig)
           , ("M-[",          spawn "nm-applet")
           , ("M-]",          spawn trackPointSetup)
           , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")       
           , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2dB-")  
           , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2dB+")
           ]  
           ++
           [(m ++ k, windows $ f w)
                | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
                , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]

trackPointSetup = "xinput set-int-prop \"TPPS/2 IBM TrackPoint\" \"Evdev Wheel Emulation\" 8 1 & xinput set-int-prop \"TPPS/2 IBM TrackPoint\" \"Evdev Wheel Emulation Button\" 8 2 & xinput set-int-prop \"TPPS/2 IBM TrackPoint\" \"Evdev Wheel Emulation Timeout\" 8 200 & xinput set-int-prop \"TPPS/2 IBM TrackPoint\" \"Evdev Wheel Emulation Axes\" 8 6 7 4 5"

startUpApps = spawn (trackPointSetup ++ " & nm-applet")

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]
 
 
myLayoutHook = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 4/100
 
 
myManageHook = composeAll
               [ floatC "MPlayer"
               , floatC "Gimp"
               , moveToC "Conkeror" "2"
               ]
    where moveToC c w = className =? c --> doF (W.shift w)
          moveToT t w = title     =? t --> doF (W.shift w)
          floatC  c   = className =? c --> doFloat
 
 
myLogHook xmobar = dynamicLogWithPP $ defaultPP {
                     ppOutput = hPutStrLn xmobar
                   , ppTitle = xmobarColor "white" "" . shorten 110
                   , ppCurrent = xmobarColor "white" "black" . pad
                   , ppHidden = pad
                   , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                   , ppSep = xmobarColor "#555" "" " / "
                   , ppWsSep = ""
                   , ppLayout = \x -> case x of
                                        "Tall" -> "T"
                                        "Mirror Tall" -> "M"
                                        "Full" -> "F"
                                        _ -> "?"
                   }
 

myWorkspaces = ["1:main","2","3","4","5","6","7","8","9:services"]
 
main = do startUpApps
	  xmobar <- spawnPipe "xmobar"
          xmonad $ defaultConfig {
                       terminal           = "gnome-terminal",
                       focusFollowsMouse  = True,
                       borderWidth        = 2,
                       modMask            = mod4Mask,
                       numlockMask        = 0,
                       workspaces         = myWorkspaces,
                       normalBorderColor  = "#444",
                       focusedBorderColor = "#f00",
                       keys               = myKeys,
                       mouseBindings      = myMouseBindings,
                       layoutHook         = myLayoutHook,
                       manageHook         = myManageHook,
                       logHook            = myLogHook xmobar
                     }
