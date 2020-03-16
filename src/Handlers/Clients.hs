module Handlers.Clients where

import Servant

type NotificationToken = String
-- type ClientsAPI = "clients" :> (
--          "add" :> ReqBody '[PlainText] NotificationToken :> Post '[JSON] NoContent
--     :<|> "remove" :> ReqBody '[PlainText] Int :> Post '[JSON] NoContent) 