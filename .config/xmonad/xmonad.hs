import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main =
  xmonad . docks . ewmhFullscreen . ewmh $
    def
      { borderWidth = 2,
        -- ウインドウのフルスクリーントグルを可能にしておく
        layoutHook = avoidStruts $ mkToggle (single FULL) $ spacingWithEdge 4 $ layoutHook def,
        -- 新しいウインドウはスタックの末尾に追加する
        manageHook = insertPosition Below Newer <+> manageHook def,
        modMask = mod4Mask,
        startupHook = myStartupHook,
        terminal = "alacritty"
      }
      `additionalKeysP` myAdditionalKeysP

myAdditionalKeysP :: [(String, X ())]
myAdditionalKeysP =
  [
    ("M-<Return>", spawn "alacritty"),
    ("M-S-<Return>", spawn "i3lock -i $HOME/Pictures/wallpaper-catppuccin.png"),
    -- xmonad の終了時は rofi のダイアログで確認する
    ("M-S-q", spawn "echo -e 'yes\\nno' | rofi -dmenu -p 'quit xmonad?' | grep -q 'yes' && pkill -x xmonad-x86_64-linux"),
    -- ウインドウ間の移動
    ("M-n", windows W.focusDown),
    ("M-p", windows W.focusUp),
    -- フルスクリーンのトグル
    ("M-S-f", sendMessage $ Toggle FULL),
    -- フォーカスウインドウをマスターにスワップ
    ("M-S-m", windows W.swapMaster),
    -- rofi
    ("M-r", spawn "rofi -show drun"),
    ("M-S-r", spawn "rofi -show window"),
    -- スクリーンショット
    ("<Print>", spawn "import -window root $HOME/Pictures/screenshot$(date \"+%Y%m%d%H%M%S\").jpg"),
    -- 画面のバックライト
    ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5"),
    ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5"),
    -- ボリューム
    ("<XF86AudioMute>", spawn "amixer sset Master mute"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 1%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+")
  ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --no-fehbg --bg-scale $HOME/Pictures/wallpaper-catppuccin.png"
  spawnOnce "picom --config $HOME/.config/picom/picom.conf"
  spawnOnce "$HOME/.config/polybar/launch.sh"
  spawnOnce "nm-applet"
  spawnOnce "fcitx5"
  spawnOnce "dunst"
  spawnOnce "parcellite"
