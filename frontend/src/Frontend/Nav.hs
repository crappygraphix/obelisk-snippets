{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend.Nav (nav) where

import Control.Monad (void)
import qualified Data.Text as T
import Reflex.Dom.Core
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend

import Common.Route

nav :: ObeliskWidget js t (R FrontendRoute) m => m ()
nav = do
  collapsibleMenu "Basic" basicMenu
  collapsibleMenu "Medium" mediumMenu
  
collapsibleMenu :: ObeliskWidget js t r m => T.Text -> m () -> m ()
collapsibleMenu t w = do
  rec
    dyC <- toggle False evB 
    (b, _) <- el' "div" $ 
      elAttr "a" ("href" =: "javascript:void(0);") $ do
        text t
        dynText $ chevron <$> dyC
    let evB = domEvent Click b
  void $ widgetHold blank (menu <$> updated dyC)
  where
    menu True = w
    menu False = blank
    chevron True = " ⇧"
    chevron False = " ⇩"

basicMenu :: ObeliskWidget js t (R FrontendRoute) m => m ()
basicMenu = el "ul" $ do
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_LoadResources :/ ())) $ text "Load Resources"  
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_SPARouting :/ ())) $ text "SPA Routing"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_CustomButton :/ ())) $ text "Buttons"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_HTTPApiCall :/ ())) $ text "HTTP API Call"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_LoginForm :/ ())) $ text "Login Form"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_ExternalLink :/ ())) $ text "External Link"
    
mediumMenu :: ObeliskWidget js t (R FrontendRoute) m => m ()
mediumMenu = el "ul" $ do
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_CollapsibleContent :/ ())) $ text "Collapsible Content"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_Loaders :/ ())) $ text "Loaders"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_SimpleTable :/ ())) $ text "Simple Table"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_ComplexTable :/ ())) $ text "Complex Table"
  el "li" $ routeLink (FrontendRoute_Snippets :/ (Just $ Snippet_WebsocketConnection :/ ())) $ text "Websocket Connection"

-- clickable :: DomBuilder t m => T.Text -> m a -> m (Event t ())
-- clickable t w = do
--   (e, _) <- elAttr' t ("href" =: "javascript:void();") w
--   return $ domEvent Click e
