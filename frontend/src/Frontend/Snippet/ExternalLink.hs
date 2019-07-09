{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.ExternalLink (main) where

import Reflex.Dom

main :: DomBuilder t m => m ()
main = el "h3" $ text "External Links"
