{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Snippet.HTTPApiCall (main) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Reflex.Dom hiding (comment)

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  key "http://httpbin.org/" "We'll be using this service to make calls to. It is an open API for testing purposes."
  key "elAttr" "Takes a tag name and an attribute map to generate a tag in the DOM."
  key "performRequestAsync" "TODO"
  key "postJson" "TODO"
  
imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import Data.Aeson (FromJSON, ToJSON)"
    , "import qualified Data.Text as T"
    , "import GHC.Generics (Generic)"
    , "import Reflex.Dom"
    , "import Reflex.Dom.Xhr"
    ]

btn :: DomBuilder t m => T.Text -> m (Event t ())
btn t = do
  (b, _) <- elAttr' "button" ("type" =: "button" <> "class" =: "btn btn-primary") $ text t
  return $ domEvent Click b

data GetResponse = GetResponse 
  { args :: GetResponseArgs 
  } deriving (Generic, Show)

data GetResponseArgs = GetResponseArgs
  { value1 :: T.Text
  , value2 :: T.Text
  } deriving (Generic, Show)

instance FromJSON GetResponse
instance ToJSON GetResponse
instance FromJSON GetResponseArgs
instance ToJSON GetResponseArgs

getRequest :: (Prerender js t m, DomBuilder t m) => m ()
getRequest = do
  card $ do
    el "h5" $ text "Example: GET"
    divClass "row" $ do
      evBtn <- divClass "col" $ btn "GET"
      divClass "col" $ do
        _ <- performGetRequest evBtn
        return ()

performGetRequest :: (DomBuilder t m, Prerender js t m) => Event t () -> m (Dynamic t ())
performGetRequest evStarted = prerender blank $ do
  evResponse <- performRequestAsync $ XhrRequest "GET" "http://httpbin.org/get?value1=bar1&value2=bar2" def <$ evStarted
  let evFinished = decoder <$> evResponse
  _ <- widgetHold blank $ leftmost [renderResponse <$> evFinished, text "Loading..." <$ evStarted]
  return ()
  where
    decoder :: XhrResponse -> Maybe GetResponse
    decoder r = decodeXhrResponse r

    renderResponse :: DomBuilder t m => Maybe GetResponse -> m ()
    renderResponse (Just v) = do
      let rArgs = args v
      el "ul" $ do
        el "li" $ do
          text "value1: "
          text (value1 rArgs)
        el "li" $ do
          text "value2: "
          text (value2 rArgs)
    renderResponse Nothing = text "Something went wrong..."

getRequestCode :: DomBuilder t m => m ()
getRequestCode = card $ do
  el "h5" $ text "Code"

postRequestCode :: DomBuilder t m => m ()
postRequestCode = card $ do
  el "h5" $ text "Code"

data PostPayload = PostPayload
  { name :: T.Text
  , comment :: T.Text
  } deriving (Generic, Show)
data PostResponse = PostResponse
  { json :: PostPayload
  } deriving (Generic, Show)
instance FromJSON PostPayload
instance ToJSON PostPayload
instance FromJSON PostResponse
instance ToJSON PostResponse

postRequest :: (Prerender js t m, PostBuild t m, DomBuilder t m) => m ()
postRequest = do
  card $ do
    el "h5" $ text "Example: POST JSON"
    divClass "row" $ do
      evBtn <- divClass "col" $ do
        dyN <- divClass "input-group" $ do
          divClass "input-group-prepend" $
            elAttr "span" ("class" =: "input-group-text") $ text "Name"
          nIn <- inputElement $ def
            & inputElementConfig_elementConfig.elementConfig_initialAttributes .~
              (  "id" =: "name" 
              <> "name" =: "name"
              <> "placeholder" =: "Your name"
              <> "class" =: "form-control"
              )
          return $ _inputElement_value nIn
        dyC <- divClass "input-group" $ do
          divClass "input-group-prepend" $
            elAttr "span" ("class" =: "input-group-text") $ text "Comment"
          cIn <- inputElement $ def
            & inputElementConfig_elementConfig.elementConfig_initialAttributes .~
              (  "id" =: "comment"
              <> "name" =: "comment"
              <> "placeholder" =: "Your comment"
              <> "class" =: "form-control"
              )
          return $ _inputElement_value cIn
        evClick <- btn "POST"
        return $ tagPromptlyDyn ((,) <$> dyN <*> dyC) evClick
      divClass "col" $ do
        void $ performPostRequest evBtn
  
performPostRequest :: (DomBuilder t m, Prerender js t m) => Event t (T.Text, T.Text) -> m (Dynamic t ())
performPostRequest evStarted = prerender blank $ do
  evResponse <- performRequestAsync $ fmap (\(n, c) -> postJson "http://httpbin.org/post" (PostPayload n c)) evStarted
  let evFinished = decoder <$> evResponse
  _ <- widgetHold blank $ leftmost [renderResponse <$> evFinished, text "Loading..." <$ evStarted]
  return ()
  where
    decoder :: XhrResponse -> Maybe PostResponse
    decoder r = decodeXhrResponse r

    renderResponse :: DomBuilder t m => Maybe PostResponse -> m ()
    renderResponse (Just v) = do
      let j = json v
      el "ul" $ do
        el "li" $ do
          text "name: "
          text (name j)
        el "li" $ do
          text "comment: "
          text (comment j)
    renderResponse Nothing = text "Something went wrong..."

main :: (PostBuild t m, Prerender js t m, DomBuilder t m) => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: External HTTP API Call"
  el "p" $ text "You may not always have domain control over the API's you'll need to consume. Here are examples of calling a 3rd party HTTP Api."
  keyTools
  imports
  getRequest
  getRequestCode
  postRequest
  postRequestCode
