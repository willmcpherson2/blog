module Main where

import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import MyLib (someFunc)

main :: IO ()
main =
  getArgs >>= \case
    [dir] -> do
      someFunc
      putStrLn $ "serving " <> dir
      putStrLn "at http://localhost:8000/"
      run 8000 $ staticApp $ defaultFileServerSettings dir
    _ -> putStrLn "no dir to serve"
