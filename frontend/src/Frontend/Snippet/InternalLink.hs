{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.InternalLink (main) where

import Reflex.Dom

main :: DomBuilder t m => m ()
main = el "h3" $ text "Internal Link"
