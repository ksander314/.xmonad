import qualified Data.Map as M (fromList, Map)
import System.IO (hPutStrLn)

import XMonad
import qualified XMonad.Actions.SpawnOn as S (manageSpawn, spawnOn)
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppOutput, ppTitle, shorten, 
                                xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(ToggleStruts))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Util.Replace (replace)
import XMonad.Util.Run (spawnPipe)

main :: IO()
main = do replace
          xmproc <- spawnPipe "xmobar $HOME/.xmobar/xmobarrc"
          xmonad kde4Config
                 { borderWidth = myBorderWidth
                 , keys = myKeys <+> keys kde4Config
                 , modMask = mod4Mask
                 , layoutHook = myLayout
                 , logHook = dynamicLogWithPP xmobarPP
                             { ppOutput = hPutStrLn xmproc
                             , ppTitle = xmobarColor "green" "" . shorten 50
                             }
                 , terminal = "urxvt"
                 , workspaces = myWorkSpaces
                 , manageHook = myManageHook
                 }
myLayout = avoidStruts (smartBorders $ Tall 1 (3/100) (1/2)) ||| smartBorders Full ||| Mirror (Tall 1 (3/100) (1/2))
myKeys :: (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ()))
myKeys (XConfig {XMonad.modMask = modm}) = M.fromList
       [ ((modm, xK_p), spawn "dmenu_run")
       , ((modm, xK_b), sendMessage ToggleStruts)
       , ((modm .|. shiftMask, xK_l), spawn "slock")
       , ((modm .|. shiftMask, xK_p), spawn "poweroff")
       , ((modm .|. controlMask, xK_e), emacsStart)
       , ((modm .|. controlMask, xK_f), firefoxStart)
       , ((modm .|. controlMask, xK_s), smplayerStart)
       , ((modm .|. controlMask, xK_t), ktorrentStart)
       , ((modm .|. controlMask, xK_g), steamStart)
       , ((modm .|. controlMask, xK_x), keepassxStart)
       ]
myWorkSpaces :: [String]
myWorkSpaces = ["home", "web", "media", "utils", "games", "null"] ++ map show [7 .. 9 :: Int]
myManageHook :: ManageHook
myManageHook = S.manageSpawn <+> manageHook kde4Config
emacsStart :: X ()
emacsStart = S.spawnOn "home" "emacs"
firefoxStart :: X ()
firefoxStart = S.spawnOn "web" "firefox"
smplayerStart :: X ()
smplayerStart = S.spawnOn "media" "smplayer"
ktorrentStart :: X ()
ktorrentStart = S.spawnOn "utils" "ktorrent"
steamStart :: X ()
steamStart = S.spawnOn "games" "steam"
keepassxStart :: X ()
keepassxStart = S.spawnOn "null" "keepassx"
myBorderWidth :: Dimension
myBorderWidth = 0
