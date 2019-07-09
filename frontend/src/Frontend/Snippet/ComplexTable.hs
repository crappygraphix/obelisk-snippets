{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.ComplexTable (main) where

import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  el "p" $ text "Some nuance or useful information."
  key "someFunction" "Why it's important."

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import qualified Data.Text as T"
    , "import Reflex.Dom"
    ]

main :: DomBuilder t m => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: Complex Table"
  el "p" $ text "Tables where the rows can trigger actions and/or be updated independently."
  keyTools
  imports
