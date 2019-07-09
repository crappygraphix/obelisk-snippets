{-# LANGUAGE OverloadedStrings #-}
module Frontend.Snippet.SPARouting (main) where

import Reflex.Dom

import Frontend.Util

keyTools :: DomBuilder t m => m ()
keyTools = card $ do
  el "h5" $ text "Key Tools"
  key "someFunction" "TODO"

imports :: DomBuilder t m => m ()
imports = card $ do
  el "h5" $ text "Imports of Note"
  code
    [ "import Control.Monad.Fix (MonadFix)"
    , "import Obelisk.Route.Frontend"
    ]

topLevel :: DomBuilder t m => m ()
topLevel = card $ do
  el "h5" $ text "Example: Top level routes."
  el "h6" $ text "Changes to the Common.Route (Common/Route.hs) module."
  el "hr" blank
  el "p" $ do
    text "First define the routes by adding them to the "
    inlineCode "FrontendRoute"
    text " type." 
  code
    [ "data FrontendRoute :: * -> * where"
    , "  FrontendRoute_Main :: FrontendRoute ()"
    , "  FrontendRoute_MyPage :: FrontendRoute ()"
    , "  FrontendRoute_MyOtherPage :: FrontendRoute ()"
    ]
  el "hr" blank
  el "p" $ do
    text "Second we need to map URL's to our type via additions to "
    inlineCode "backendRouteEncoder"
    text "." 
  code
    [ "backendRouteEncoder"
    , "  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName"
    , "backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $"
    , "  pathComponentEncoder $ \\case"
    , "    InL backendRoute -> case backendRoute of"
    , "      BackendRoute_Missing -> PathSegment \"missing\" $ unitEncoder mempty"
    , "    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \\case"
    , "      FrontendRoute_Main -> PathEnd $ unitEncoder mempty"
    , "      FrontendRoute_MyPage -> PathSegment \"my_page\" $ unitEncoder mempty"
    , "      FrontendRoute_MyOtherPage -> PathSegment \"my_other_page\" $ unitEncoder mempty"
    ]
  el "hr" blank
  el "h6" $ text "Changes to the Frontend (Frontend.hs) module."
  el "p" $ do
    text "First we'll leverage our previous code changes to "
    inlineCode "Common.Route"
    text " to create a navigation menu by using "
    inlineCode "routeLink"
    text " from within a widget. Note the constraints "
    inlineCode "RouteToUrl"
    text " and "
    inlineCode "SetRoute"
    text " are necessary on the widget."
  code 
    [ "navigationMenu :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m) => m ()"
    , "navigationMenu = el \"ul\" $ do"
    , "  el \"li\" $ routeLink (FrontendRoute_MyPage :/ ()) $ text \"My Page\""
    , "  el \"li\" $ routeLink (FrontendRoute_MyOtherPage :/ ()) $ text \"My Other Page\""      
    ]
  el "hr" blank
  el "p" $ do
    text "Finally we render the correct widget based on the route by using the "
    inlineCode "subRoute_"
    text " function."
  code 
    [ "frontend :: Frontend (R FrontendRoute)"
    , "frontend = Frontend"
    , "  { _frontend_head = el \"title\" $ text \"Obelisk Minimal Example\""
    , "  , _frontend_body = do"
    , "      navigationMenu"
    , "      content"
    , "  }"
    , ""
    , "content :: (DomBuilder t m, MonadHold t m, MonadFix m) => RoutedT t (R FrontendRoute) m ()"
    , "content = subRoute_ $ \\case"
    , "  FrontendRoute_Main -> el \"h1\" $ text \"Main\""
    , "  FrontendRoute_MyPage -> el \"h1\" $ text \"My Page\""
    , "  FrontendRoute_MyOtherPage -> el \"h1\" $ text \"My Other Page\""
    ]

subLevel :: DomBuilder t m => m ()
subLevel = card $ do
  el "h5" $ text "Example: Sub routes."
  el "h6" $ text "Changes to the Common.Route (Common/Route.hs) module."
  el "hr" blank
  el "p" $ text "First we define our new route type."
  code
    [ "data ChildRoute :: * -> * where"
    , "  ChildRoute_MySubPageA :: ChildRoute ()"
    , "  ChildRoute_MySubPageB :: ChildRoute ()"
    ]
  el "hr" blank
  el "p" $ do
    text "Now we're able to use our route as a child of a primary route. By using "
    inlineCode "Maybe"
    text " we've allowed ourselves to support a URL that may or may not have children. We're now capable of handling http://parent_page and http://parent_page/child_page through the "
    inlineCode "maybeRoute"
    text " function."
  code
    [ "data FrontendRoute :: * -> * where"
    , "  FrontendRoute_Main :: FrontendRoute ()"
    , "  FrontendRoute_MyPage :: FrontendRoute (Maybe (R ChildRoute))"
    ]
  el "hr" blank
  el "p" $ do
    text "Next we need to map URL's to our type via additions to "
    inlineCode "backendRouteEncoder"
    text ". Note the use of "
    inlineCode "maybeEncoder"
    text " and "
    inlineCode "pathComponentEncoder"
    text " for handling the child route encoding." 
  code
    [ "backendRouteEncoder"
    , "  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName"
    , "backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $"
    , "  pathComponentEncoder $ \\case"
    , "    InL backendRoute -> case backendRoute of"
    , "      BackendRoute_Missing -> PathSegment \"missing\" $ unitEncoder mempty"
    , "    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \\case"
    , "      FrontendRoute_Main -> PathEnd $ unitEncoder mempty"
    , "      FrontendRoute_MyPage -> PathSegment \"my_page\" $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \\case"
    , "        ChildRoute_MySubPageA -> PathSegment \"my_sub_page_a\" $ unitEncoder mempty"
    , "        ChildRoute_MySubPageB -> PathSegment \"my_sub_page_b\" $ unitEncoder mempty"
    ]
  el "hr" blank  
  el "p" $ do
    text "Finally we need to make Obelisk aware of our new route component via "
    inlineCode "deriveRouteComponent"
    text " at the bottom of "
    inlineCode "Common.Route"
  code
    [ "concat <$> mapM deriveRouteComponent"
    , "[ ''BackendRoute"
    , ", ''FrontendRoute"
    , ", ''ChildRoute"
    , "]"
    ]
  el "hr" blank
  el "h6" $ text "Changes to the Frontend (Frontend.hs) module."
  el "p" $ do
    text "First we'll leverage our previous code changes to "
    inlineCode "Common.Route"
    text " to create a navigation menu by using "
    inlineCode "routeLink"
    text " from within a widget. Note the constraints "
    inlineCode "RouteToUrl"
    text " and "
    inlineCode "SetRoute"
    text " are necessary on the widget."
  code 
    [ "navigationMenu :: (DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m) => m ()"
    , "navigationMenu = el \"ul\" $ do"
    , "  el \"li\" $ routeLink (FrontendRoute_Main :/ ()) $ text \"Main\""
    , "  el \"li\" $ routeLink (FrontendRoute_MyPage :/ Nothing) $ text \"My Page\""
    , "  el \"li\" $ routeLink (FrontendRoute_MyPage :/ (Just $ ChildRoute_MySubPageA :/ ())) $ text \"Sub Page A\""
    , "  el \"li\" $ routeLink (FrontendRoute_MyPage :/ (Just $ ChildRoute_MySubPageB :/ ())) $ text \"Sub Page B\""
    ]
  el "hr" blank
  el "p" $ do
    text "Finally we render the correct widget based on the route by using the "
    inlineCode "subRoute_"
    text " and "
    inlineCode "maybeRoute_"
    text " functions."
  code 
    [ "frontend :: Frontend (R FrontendRoute)"
    , "frontend = Frontend"
    , "  { _frontend_head = el \"title\" $ text \"Obelisk Minimal Example\""
    , "  , _frontend_body = do"
    , "      navigationMenu"
    , "      content"
    , "  }"
    , ""
    , "content :: (DomBuilder t m, MonadHold t m, MonadFix m) => RoutedT t (R FrontendRoute) m ()"
    , "content = subRoute_ $ \\case"
    , "  FrontendRoute_Main -> el \"h1\" $ text \"Main\""
    , "  FrontendRoute_MyPage -> maybeRoute_ (el \"h1\" $ text \"My Page\") $ subRoute_ $ \\case"
    , "    ChildRoute_MySubPageA -> el \"h1\" $ text \"Sub Page A\""
    , "    ChildRoute_MySubPageB -> el \"h1\" $ text \"Sub Page B\""
    ]

complex :: DomBuilder t m => m ()
complex = card $ do
  el "h5" $ text "Example: Complex routes."
  el "p" $ text "Often times a URL will contain routing information that is useful or necessary for widgets. In this example we'll demonstrate support for the following urls:"
  el "ul" $ do
    el "li" $ text "users/:username/profile"
    el "li" $ text "users/:username/transactions"
    el "li" $ text "users/:username/transactions/:transaction_number"
  el "p" $ do
    text "It's important to note that Obelisk earmarks top level urls so dynamic top level routes are not covered in these examples as they are not advised. For example "
    inlineCode "https://mysite.com/:something/leaf"
    text " should be avoided."
  el "h6" $ text "Changes to the Common.Route (Common/Route.hs) module."
  el "hr" blank

main :: DomBuilder t m => m ()
main = do
  el "h3" $ text "Obelisk/Reflex Snippets: SPA Routing"
  el "p" $ text "Obelisk comes pre-packaged with SPA Routing tools to support navigation with deep linking support."
  keyTools
  imports
  topLevel
  subLevel
  complex
