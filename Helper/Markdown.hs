{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper.Markdown where

import Prelude

import Data.Text.Lazy (toStrict, fromStrict)
import Database.Persist.Sql
import Text.Markdown
import qualified Data.Text.Lazy as TL

instance Show Markdown where
    show (Markdown t) = show t

instance Eq Markdown where
    Markdown a == Markdown b = a == b

instance PersistField Markdown where
    toPersistValue (Markdown t) = PersistText $ toStrict t

    fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
    fromPersistValue _ = Left "Not a PersistText value"

instance PersistFieldSql Markdown where
    sqlType _ = SqlString

unMarkdown :: Markdown -> TL.Text
unMarkdown (Markdown t) = t
