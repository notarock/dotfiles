import XMonad

main :: IO ()
main = do
  xmonad $ def
        { modMask = mod4Mask -- Use Super instead of Alt
        , terminal = "kitty"
        -- more changes
        }
