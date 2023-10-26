module Cardano.Utils.Log where

warnMsg :: [String] -> IO ()
warnMsg = genericMsg "Warning: "

errorMsg :: [String] -> IO ()
errorMsg = genericMsg "Error: "

-- FIXME: make more configurable/?
genericMsg :: String -> [String] -> IO ()
genericMsg _ [] = return ()
genericMsg tg (s : ss) = mapM_ putStrLn $ (tg ++ s) : map ("  " ++) ss

---- Possibly ------------------------------------------------------

type Possibly a = Either [String] a  -- FIXME: an existing synonym for?

failLeft :: MonadFail m => Possibly a -> m a
failLeft = possiblyToM

possiblyToM :: MonadFail m => Possibly a -> m a
possiblyToM a = case a of
                  Left s  -> fail (unlines s)
                  Right x -> return x
