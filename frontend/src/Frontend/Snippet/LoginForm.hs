{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.LoginForm (main) where

import Reflex.Dom

main :: DomBuilder t m => m ()
main = el "h3" $ text "Login Form"
