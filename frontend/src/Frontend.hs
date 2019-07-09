{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend (frontend) where

import Obelisk.Frontend
import Obelisk.Route

import Common.Route

import Frontend.Body
import Frontend.Head

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = header
  , _frontend_body = body
  }
