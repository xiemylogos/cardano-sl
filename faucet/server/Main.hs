{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import           Control.Lens
import           Data.Aeson (eitherDecode)
import           Data.ByteString.Lazy as BSL
import           Network.Wai.Handler.Warp (run)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Servant
import           System.Environment (getArgs)
import           System.Remote.Monitoring (forkServer, serverMetricStore)
import           System.Remote.Monitoring.Statsd (forkStatsd)

import           Cardano.Faucet
import           Cardano.Faucet.Swagger

main :: IO ()
main = withCompileInfo $ do
  ekg <- forkServer "localhost" 8001
  args <- getArgs
  config <- case args of
    [ "--config", cfgFile ] -> do
      ecfg <- eitherDecode <$> BSL.readFile cfgFile
      either (error . ("Error decoding: " ++)) return ecfg
    _ -> error "Need a --config argument pointing to a json file"
  fEnv <- initEnv config (serverMetricStore ekg)
  let server = faucetHandler fEnv
  _statsd <- forkStatsd (config ^. fcStatsdOpts . _Wrapped') (fEnv ^. feStore)
  run (config ^. fcPort) (serve faucetDocAPI server)
