module Meadowstalk.Controllers.Tag
  ( getTags
  , getTagsMap
  , getArticleTags
  ) where

import           Control.Arrow
import           Control.Monad.IO.Class

import           Data.Map (Map)
import qualified Data.Map as Map

import           Database.Esqueleto

import           Meadowstalk.Model

-------------------------------------------------------------------------------

getTags :: MonadIO m => SqlPersistT m [Entity Tag]
getTags = select . from $ \tag -> do
  orderBy [ asc (tag ^. TagName) ]
  return tag

getTagsMap :: MonadIO m => SqlPersistT m (Map TagId (Entity Tag))
getTagsMap = go <$> getTags where
  go = Map.fromList . map (entityKey &&& id)

getArticleTags :: MonadIO m => ArticleId -> SqlPersistT m [Entity Tag]
getArticleTags aid = select . from $ \(atag `LeftOuterJoin` tag) -> do
  on (tag ^. TagId ==. atag ^. ArticleTagTag)
  where_ (atag ^. ArticleTagArticle ==. val aid)
  return tag
