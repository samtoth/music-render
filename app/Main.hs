{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import GHC.Float
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Graphics.Svg.Core

import qualified Network.HTTP.Media as M

import Servant
--import Lucid.Servant
--import Lucid

import qualified Data.Text as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS

import qualified Network.Wai.Handler.Warp as Warp

import Lib (generate, lineSpacing)
import AltRep

data SVGMime

instance Accept SVGMime where
    contentType _ = "image" M.// "svg+xml"


instance MimeRender SVGMime Element where
    mimeRender _ = encodeUtf8.renderText

type SimpleApi = "test" :> Get '[SVGMime] Element

renderElem :: Diagram B -> Element
renderElem = renderDia SVG (SVGOptions (mkWidth 600) Nothing "" [] True)

server :: Server SimpleApi
server = return . renderElem $ generate def{lineSpacing=0.009} [AltRep.Prim 1 :+: AltRep.Prim 2] # frame 0.1

serverApi :: Proxy SimpleApi
serverApi = Proxy

app = serve serverApi server

main :: IO ()
--main = Data.Text.IO.putStrLn.LT.toStrict.renderText.renderElem $ diagram 2 2
main = do
        port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
        putStrLn $ "http://localhost:" ++ show port ++ "/"
        Warp.run port app


