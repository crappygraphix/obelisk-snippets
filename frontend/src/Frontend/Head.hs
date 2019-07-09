{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Head (header) where

import qualified Data.Text as T
import Reflex.Dom.Core

import Obelisk.Generated.Static

header :: DomBuilder t m => m ()
header = do
  el "title" $ text "Obelisk Examples"
  meta
  cssFile $ static @"css/bootstrap.min.css"

meta :: DomBuilder t m => m ()
meta = elAttr "meta" attrs blank
  where 
    attrs = "name" =: "viewport" 
         <> "content" =: "width=device-width,initial-scale=1" 
         <> "charset" =: "UTF-8"

cssFile :: DomBuilder t m => T.Text -> m ()
cssFile r = elAttr "link" attrs blank
  where
    attrs = "rel" =: "stylesheet" 
         <> "type" =: "text/css" 
         <> "href" =: r
