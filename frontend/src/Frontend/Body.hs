{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Body (body) where

import Reflex.Dom.Core
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Route
import qualified Frontend.Snippet.CollapsibleContent as CollapsibleContent
import qualified Frontend.Snippet.CustomButton as CustomButton
import qualified Frontend.Snippet.ExternalLink as ExternalLink
import qualified Frontend.Snippet.HTTPApiCall as HTTPApiCall
import qualified Frontend.Snippet.InternalLink as InternalLink
import qualified Frontend.Snippet.LoadResources as LoadResources
import qualified Frontend.Snippet.LoginForm as LoginForm
import qualified Frontend.Snippet.SPARouting as SPARouting
import qualified Frontend.Snippet.WebsocketConnection as WebsocketConnection
import qualified Frontend.Snippet.ComplexTable as ComplexTable
import qualified Frontend.Snippet.SimpleTable as SimpleTable
import qualified Frontend.Snippet.Loaders as Loaders
import Frontend.Nav

body :: ObeliskWidget js t (R FrontendRoute) m => RoutedT t (R FrontendRoute) m ()
body = do
  divClass "container-fluid d-flex" $ do
    divClass "col-9" $ snippet
    elAttr "div" ("class" =: "col" <> "style" =: "flex: 0 0 250px;") nav
  where
    snippet = subRoute_ $ \case
      FrontendRoute_Main -> home
      FrontendRoute_Snippets -> maybeRoute_ home $ snippets

home :: DomBuilder t m => m ()
home = do
  el "h3" $ text "Obelisk/Reflex Snippets"
  el "p" $ do
    text "Obelisk provides an easy way to develop and deploy your Reflex project for web and mobile."
  el "p" $ do
    text "To the right are snippets implementing common usecases for apps implemented using Reflex along with "
    let obSrc = "https://github.com/obsidiansystems/obelisk"
    elAttr "a" ("href" =: obSrc <> "target" =: "_blank") $ text "Obelisk."
  el "p" $ do
    text "These snippets assume you're using Obelisk and have a project structure matching that created with the 'ob init' command."

snippets :: ObeliskWidget js t (R FrontendRoute) m => RoutedT t (R Snippet) m ()
snippets = subRoute_ $ \case
  Snippet_CollapsibleContent -> CollapsibleContent.main
  Snippet_CustomButton -> CustomButton.main
  Snippet_ExternalLink -> ExternalLink.main
  Snippet_HTTPApiCall -> HTTPApiCall.main
  Snippet_InternalLink -> InternalLink.main
  Snippet_LoadResources -> LoadResources.main
  Snippet_LoginForm -> LoginForm.main
  Snippet_SPARouting -> SPARouting.main
  Snippet_WebsocketConnection -> WebsocketConnection.main
  Snippet_SimpleTable -> SimpleTable.main
  Snippet_ComplexTable -> ComplexTable.main
  Snippet_Loaders -> Loaders.main
