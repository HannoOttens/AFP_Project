module Handlers where

type ClientsAPI = "clients" :> (
        Post '[JSON] NoContent
    ) 