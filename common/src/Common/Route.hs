{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
  FrontendRoute_Snippets :: FrontendRoute (Maybe (R Snippet))

data Snippet :: * -> * where
  Snippet_LoadResources :: Snippet ()
  Snippet_SPARouting :: Snippet ()
  Snippet_LoginForm :: Snippet ()
  Snippet_CustomButton :: Snippet ()
  Snippet_InternalLink :: Snippet ()
  Snippet_ExternalLink :: Snippet ()
  Snippet_CollapsibleContent :: Snippet ()
  Snippet_HTTPApiCall :: Snippet ()
  Snippet_WebsocketConnection :: Snippet ()
  Snippet_SimpleTable :: Snippet ()
  Snippet_ComplexTable :: Snippet ()
  Snippet_Loaders :: Snippet ()

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Snippets -> PathSegment "snippets" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
        Snippet_LoadResources -> PathSegment "load_resources" $ unitEncoder mempty
        Snippet_SPARouting -> PathSegment "spa_routing" $ unitEncoder mempty
        Snippet_LoginForm -> PathSegment "login_form" $ unitEncoder mempty
        Snippet_CustomButton -> PathSegment "custom_button" $ unitEncoder mempty
        Snippet_InternalLink -> PathSegment "internal_link" $ unitEncoder mempty
        Snippet_ExternalLink -> PathSegment "external_link" $ unitEncoder mempty
        Snippet_CollapsibleContent -> PathSegment "collapsible" $ unitEncoder mempty
        Snippet_HTTPApiCall -> PathSegment "http_api" $ unitEncoder mempty
        Snippet_WebsocketConnection -> PathSegment "websockets" $ unitEncoder mempty
        Snippet_SimpleTable -> PathSegment "simple_table" $ unitEncoder mempty
        Snippet_ComplexTable -> PathSegment "complex_table" $ unitEncoder mempty
        Snippet_Loaders -> PathSegment "loaders" $ unitEncoder mempty

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''Snippet
  ]
