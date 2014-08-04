module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Network.Gravatar

import Text.Blaze.Html(toMarkup)
import Text.Blaze.Renderer.String
import Text.Markdown
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Helper.Markdown ()

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

userName :: User -> Text
userName u = T.concat [userFirstName u , " " , userLastName u]

userGravatar :: User -> Text
userGravatar = T.pack . gravatar def . userEmail

instance ToJSON (Entity User) where
    toJSON (Entity uid u) = object
        [ "id"           .= (String $ toPathPiece uid)
        , "first_name"   .= userFirstName u
        , "last_name"    .= userLastName u
        , "email"        .= userEmail u
        , "gravatar_url" .= userGravatar u
        ]

data UserComment = UserComment (Entity Comment) User

instance ToJSON UserComment where
    toJSON (UserComment (Entity cid c) u) = object
        [ "id"           .= (String $ toPathPiece cid)
        , "user_id"      .= (String $ toPathPiece $ commentUser c)
        , "user_name"    .= userName u
        , "gravatar_url" .= userGravatar u
        , "article"      .= commentArticle c
        , "thread"       .= commentThread c
        , "body"         .= (unMarkdown $ commentBody c)
        , "body_html"    .= (String $ renderMarkdown c)
        ]

unMarkdown :: Markdown -> TL.Text
unMarkdown (Markdown t) = t

renderMarkdown :: Comment -> Text
renderMarkdown = T.pack . renderMarkup . toMarkup . commentBody
