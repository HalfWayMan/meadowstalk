{-# LANGUAGE RecordWildCards #-}

module Meadowstalk.Controllers.Article
  ( getPublishedArticles
  , ArticleFilter (..)
  , getArticleListViews
  ) where

import Control.Monad.IO.Class

import Data.Default
import Data.Text (Text)

import Database.Esqueleto

import Meadowstalk.Model
import Meadowstalk.Views

-------------------------------------------------------------------------------

getPublishedArticles :: MonadIO m => SqlPersistT m [Entity Article]
getPublishedArticles = select . from $ \article -> do
  where_ (article ^. ArticlePublished !=. val Nothing)
  return article

-------------------------------------------------------------------------------

data ArticleFilter =
  ArticleFilter { afMatching :: Maybe Text
                , afOffset   :: Maybe Int
                , afLimit    :: Maybe Int
                }

instance Default ArticleFilter where
  def = ArticleFilter Nothing Nothing Nothing

getArticleListViews :: MonadIO m =>
                       ArticleFilter -> SqlPersistT m [Entity ArticleListView]
getArticleListViews ArticleFilter{..} = select . from $ \alv -> do
  mapM_ (offset . fromIntegral) afOffset
  mapM_ (limit . fromIntegral) afLimit
  return alv
