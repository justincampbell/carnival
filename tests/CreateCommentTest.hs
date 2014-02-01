{-# LANGUAGE OverloadedStrings #-}
module CreateCommentTest
    ( createCommentSpecs
    ) where

import TestImport

createCommentSpecs :: Spec
createCommentSpecs =
    ydescribe "POST /api/v1/comments" $ do
        yit "responds with 201 created" $ do
            postBody CommentsR (encode $ Comment "" "")

            statusIs 201

        yit "creates a comment in the database" $ do
            clearComments
            let (thread, body) = ("A thread", "A body")

            postBody CommentsR (encode $ Comment thread body)

            ((Entity _ c):_) <- runDB $ selectList [] []
            assertEqual' thread $ commentThread c
            assertEqual' body $ commentBody c
