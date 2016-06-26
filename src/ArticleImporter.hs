{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger

import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as C8
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Data.Yaml

import           Database.Persist.Sql
import           Database.Persist.Postgresql

import           System.Directory
import           System.Environment
import           System.FilePath

import           Meadowstalk.Model

-------------------------------------------------------------------------------

data YamlArticle =
  YamlArticle { yamlArticleSlug     :: Text
              , yamlArticleTitle    :: Text
              , yamlArticleImage    :: Maybe Text
              , yamlArticleAuthor   :: Text
              , yamlArticleSynopsis :: Text
              , yamlArticleTags     :: [Text]
              }
  deriving (Show)

instance FromJSON YamlArticle where
  parseJSON = withObject "Article" $ \obj ->
    YamlArticle <$> obj .:  "slug"
                <*> obj .:  "title"
                <*> obj .:? "image"
                <*> obj .:  "author"
                <*> obj .:  "synopsis"
                <*> obj .:  "tags"

-------------------------------------------------------------------------------

parseArticleDescription :: FilePath -> IO YamlArticle
parseArticleDescription path = do
  either (fail . prettyPrintParseException) return =<< decodeFileEither path

findArticleContent :: FilePath -> IO Text
findArticleContent path = do
  let path' = replaceExtension path "md"
  exists <- doesFileExist path
  when (not exists) $
    fail $ "Could not find article content: " ++ path'
  TIO.readFile path'

-------------------------------------------------------------------------------

findTag :: MonadIO m => Text -> SqlPersistT m TagId
findTag name = do
  mtag <- getBy (UniqueTag name)
  case mtag of
    Nothing  -> insert (Tag name "#000000")
    Just tag -> return (entityKey tag)

findAuthor :: MonadIO m => Text -> SqlPersistT m UserId
findAuthor name = do
  muser <- getBy (UniqueUser name)
  case muser of
    Nothing   -> fail $ "Could not find user with username: " ++ show name
    Just user -> return (entityKey user)

insertArticle :: MonadIO m => YamlArticle -> Text -> SqlPersistT m ()
insertArticle YamlArticle{..} content = do
  tags   <- mapM findTag yamlArticleTags
  author <- findAuthor yamlArticleAuthor
  let article = Article { articleSlug       = yamlArticleSlug
                        , articleTitle      = yamlArticleTitle
                        , articleSynopsis   = yamlArticleSynopsis
                        , articleContent    = content
                        , articleImage      = yamlArticleImage
                        , articlePublished  = Nothing
                        , articleAuthor     = author
                        }
  aid <- insert article
  mapM_ (insert . ArticleTag aid) tags

updateArticle :: MonadIO m =>
                 ArticleId -> YamlArticle -> Text -> SqlPersistT m ()
updateArticle aid YamlArticle{..} content = do
  tags   <- mapM findTag yamlArticleTags
  author <- findAuthor yamlArticleAuthor
  update aid [ ArticleTitle    =. yamlArticleTitle
             , ArticleImage    =. yamlArticleImage
             , ArticleSynopsis =. yamlArticleSynopsis
             , ArticleContent  =. content
             , ArticleAuthor   =. author
             ]
  deleteWhere [ ArticleTagArticle ==. aid ]
  mapM_ (insert . ArticleTag aid) tags

-------------------------------------------------------------------------------

displayUsage :: IO ()
displayUsage =
  putStrLn "usage: article-importer <connection-string> <path-to-yaml>"

withYamlPath :: (String -> FilePath -> IO a) -> IO ()
withYamlPath action = do
  args <- getArgs
  case args of
    [ connstr, path ] -> void (action connstr path)
    _ -> displayUsage

main :: IO ()
main = withYamlPath $ \connstr path -> do
  article <- parseArticleDescription path
  content <- findArticleContent path
  runStdoutLoggingT $
    withPostgresqlPool (C8.pack connstr) 1 $ \pool -> flip runSqlPool pool $ do
      runMigration migrateModel
      mfound <- getBy (UniqueArticle (yamlArticleSlug article))
      case mfound of
        Nothing    -> insertArticle article content
        Just found -> updateArticle (entityKey found) article content

  
