{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Snippet.Loaders (main) where

import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ 
  el "h5" $ text "Key Tools"

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import Reflex.Dom"
    ]

example :: DomBuilder t m => m ()
example =
  card $
    el "h5" $ text "Example: I'm an example!."

main :: DomBuilder t m => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: Dynamic Widgets"
  el "p" $ text "Instead of using static widgets with dynamic data, you can use dynamic widgets. This methodology make it easier to work with complex dynamic data by moving the dynamic nature one layer up."
  keyTools
  imports
  example
