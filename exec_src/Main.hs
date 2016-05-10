{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import qualified Network.Socket as S
import System.Environment

import Blockchain.Output
import Executable.EthereumDiscovery

main :: IO ()
main = do
  args <- getArgs
  S.withSocketsDo $ flip runLoggingT printLogMsg $ ethereumDiscovery args
