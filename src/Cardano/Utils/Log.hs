module Cardano.Utils.Log where

warnMsg :: [String] -> IO ()
warnMsg = genericMsg "Warning: "

errorMsg :: [String] -> IO ()
errorMsg = genericMsg "Error: "

-- FIXME: make more configurable/?
genericMsg :: String -> [String] -> IO ()
genericMsg _ [] = return ()
genericMsg tg (s : ss) = mapM_ putStrLn $ (tg ++ s) : map ("  " ++) ss
