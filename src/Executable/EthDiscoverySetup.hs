{-# LANGUAGE OverloadedStrings #-}

module Executable.EthDiscoverySetup (
  setup
  ) where

import Control.Monad.Logger
    

setup::LoggingT IO ()
setup = do
  return ()
