{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.WebsocketConnection (main) where

import Reflex.Dom

main :: DomBuilder t m => m ()
main = el "h3" $ text "Websocket Connection"
