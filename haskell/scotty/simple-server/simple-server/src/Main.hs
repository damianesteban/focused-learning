{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty

data User = User { userId::Int, userName::String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 2, userName = "bob" }

sally :: User
sally = User { userId = 1, userName = "Sally" }

allUsers :: [User]
allUsers = [bob, sally]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

main = do
  putStrLn("Starting server.....")
  scotty 3000 $ do
    get "/api/hello/:name" $ do
      name <- param "name"
      text ("hello" <> name <> "!")
    
    get "/api/users" $ do
      json allUsers
    
    get "/api/users/:id" $ do
      id <- param "id"
      json (filter (matchesId id) allUsers)
