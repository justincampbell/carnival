{-# LANGUAGE OverloadedStrings #-}
module TestHelpers.DB
    ( runDB
    , clearTables
    , createUser
    , createComment
    ) where

import Model
import Foundation
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Yesod.Test
import Yesod.Markdown
import Database.Persist
import Database.Persist.Sql (SqlPersistM, runSqlPersistMPool)

type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

clearTables :: Example ()
clearTables = runDB $ do
    deleteWhere ([] :: [Filter Comment])
    deleteWhere ([] :: [Filter User])

createUser :: Text -> Example (Entity User)
createUser ident = do
    let u = User
            { userFirstName = "John"
            , userLastName  = "Smith"
            , userEmail     = "john@gmail.com"
            , userIdent     = ident
            }

    uid <- runDB $ insert u

    return $ Entity uid u

createComment :: UserId -> Text -> Text -> Example (Entity Comment)
createComment uid article body = do
    now <- liftIO getCurrentTime
    let c = Comment
            { commentUser    = uid
            , commentThread  = "thread"
            , commentArticle = article
            , commentBody    = Markdown body
            , commentCreated = now
            }

    cid <- runDB $ insert c

    return $ Entity cid c
