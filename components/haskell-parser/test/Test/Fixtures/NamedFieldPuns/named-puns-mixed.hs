{-# LANGUAGE NamedFieldPuns #-}

module NamedFieldPunsMixed where

data Config = Config {host :: String, port :: Int, secure :: Bool}

normalize :: Config -> Config
normalize Config {host, port, secure = _} = Config {host, port, secure = True}
