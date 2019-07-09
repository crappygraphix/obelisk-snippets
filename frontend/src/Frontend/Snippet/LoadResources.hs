{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.LoadResources (main) where

import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  el "p" $ do
    text "Resource loading happens in the "
    inlineCode "<head>"
    text " tag. As such, an example change to the generated "
    inlineCode "Frontend.hs"
    text " shown in the following snippet is necessary, where the function "
    inlineCode "header"
    text " will be defined in the example snippets."
  code
    [ "frontend :: Frontend (R FrontendRoute)"
    , "frontend = Frontend"
    , "  { _frontend_head = header"
    , "  , _frontend_body = do"
    , "      text \"Welcome to Obelisk!\""
    , "      el \"p\" $ text $ T.pack commonStuff"
    , "      elAttr \"img\" (\"src\" =: static @\"obelisk.jpg\") blank"
    , "  }"
    ]
  key "elAttr" "Takes a tag name and an attribute map to generate a tag in the DOM."
  key "static" "Allows us to access a resource placed in the project's static directory."

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import qualified Data.Text as T"
    , "import Reflex.Dom.Core"
    , "import Obelisk.Generated.Static"
    ]

fromStatic :: DomBuilder t m => m ()
fromStatic = card $ do
  el "h5" $ text "Example: Import CSS from a Static Resource"
  code
    [ "header :: DomBuilder t m => m ()"
    , "header = do"
    , "  cssFile $ static @\"css/custom.css\""
    , ""
    , "cssFile :: DomBuilder t m => T.Text -> m ()"
    , "cssFile r = elAttr \"link\" attrs blank"
    , "  where"
    , "    attrs = \"rel\" =: \"stylesheet\" "
    , "         <> \"type\" =: \"text/css\" "
    , "         <> \"href\" =: r"
    ]

fromCDN :: DomBuilder t m => m ()
fromCDN = card $ do
  el "h5" $ text "Example: Import CSS from a CDN"
  code
    [ "header :: DomBuilder t m => m ()"
    , "header = do"
    , "  cssFile \"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\""
    , ""
    , "cssFile :: DomBuilder t m => T.Text -> m ()"
    , "cssFile r = elAttr \"link\" attrs blank"
    , "  where"
    , "    attrs = \"rel\" =: \"stylesheet\" "
    , "         <> \"type\" =: \"text/css\" "
    , "         <> \"href\" =: r"
    ]

main :: DomBuilder t m => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: Load Resources"
  el "p" $ text "You can load external resources from CDN's or the static folder of your Obelisk project."
  keyTools
  imports
  fromStatic
  fromCDN
