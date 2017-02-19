import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks

import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Run(spawnPipe)

main = do
    myStatusBar <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
    xmonad $ ewmh defaultConfig
        { borderWidth        = 2
        , focusedBorderColor = "#ededed"
        , normalBorderColor  = "#262626"
        , focusFollowsMouse  = True
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , layoutHook         = avoidStruts $ (spacing 4 $ ResizableTall 1 (3 / 100) (1 / 2) [])
        , logHook            = dynamicLogWithPP $ xmobarPP
            { ppOrder           = \(workspace:layout:title:_) -> [workspace, title]
            , ppOutput          = hPutStrLn myStatusBar
            , ppCurrent         = xmobarColor "#ff005f" "black" . \s -> "●"
            , ppUrgent          = xmobarColor "#666666" "black" . \s -> "●"
            , ppVisible         = xmobarColor "#ff005f" "black" . \s -> "⦿"
            , ppHidden          = xmobarColor "#666666" "black" . \s -> "●"
            , ppHiddenNoWindows = xmobarColor "#666666" "black" . \s -> "○"
            , ppTitle           = shorten 30
            , ppWsSep           = " "
            , ppSep             = "　"
            }
        , manageHook         = manageDocks <+> manageHook defaultConfig
        , modMask            = mod4Mask
        , startupHook        = myStartupHook
        , terminal           = "terminator"
        }
        `additionalKeysP`
        [
            ("M-p", shellPrompt defaultXPConfig
                { font              = "xft:SourceHanSansJP:size=16:antialias=true"
                , bgColor           = "black"
                , fgColor           = "#646464"
                , promptBorderWidth = 0
                , position          = Top
                , alwaysHighlight   = True
                , height            = 30
                }
            )
        ]

myStartupHook = do
    spawn "fcitx"
    spawn "feh --bg-fill $HOME/Pictures/wallpaper-linux.jpg"
