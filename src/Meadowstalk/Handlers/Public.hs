{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE QuasiQuotes       #-}

module Meadowstalk.Handlers.Public
  where

import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Format.Human

import           Cheapskate (markdown)
import           Text.Blaze.Html (toMarkup)
import           Text.Julius

import           Yesod.Core
import           Yesod.Feed
import           Yesod.Form
import           Yesod.Persist
import           Yesod.Sitemap

import           Meadowstalk.Controllers.Article
import           Meadowstalk.Controllers.Tag
import           Meadowstalk.Foundation
import           Meadowstalk.Model
import           Meadowstalk.Static
import           Meadowstalk.Views

-------------------------------------------------------------------------------

renderTags :: [Entity Tag] -> Widget
renderTags tags = [whamlet|$newline never
  <div .tags>
    $forall tag <- tags
      <a href=@{SearchTagR (entityKey tag)}>
        <span .label .label-default>
          #{tagName (entityVal tag)}
|]

renderTagsMap :: Map TagId (Entity Tag) -> [TagId] -> Widget
renderTagsMap tmap =
  renderTags . catMaybes . map (flip Map.lookup tmap)

-------------------------------------------------------------------------------

getHomeR :: Handler Html
getHomeR = do
  now  <- liftIO getCurrentTime
  page <- runInputGet (iopt intField "page")
  let afilter = def { afOffset = Just (fromMaybe 0 page * pageSize)
                    , afLimit  = Just pageSize
                    }
  (articles, tags) <- runDB $
    (,) <$> getArticleListViews afilter <*> getTagsMap
  defaultLayout $ do
    setTitle "Home"
    $(whamletFile "templates/pages/home.hamlet")
  where
    pageSize = 30

getFavIconR :: Handler ()
getFavIconR =
  sendFile "image/x-icon" "static/media/favicon.ico"

getRobotsR :: Handler ()
getRobotsR =
  sendFile "text/plain" "static/robots.txt"

getHumansR :: Handler ()
getHumansR =
  sendFile "text/plain" "static/humans.txt"

getSitemapXslR :: Handler ()
getSitemapXslR =
  sendFile "text/xsl" "static/sitemap.xsl"

getSitemapXmlR :: Handler TypedContent
getSitemapXmlR = do
  articles <- runDB (getArticleListViews def)
  let first = case articles of
        []      -> Nothing
        (x : _) -> Just (entityVal x)
      firstUrl = SitemapUrl { sitemapLoc        = HomeR
                            , sitemapLastMod    =
                                articleListViewPublished <$> first
                            , sitemapChangeFreq = Just Weekly
                            , sitemapPriority   = Nothing
                            }
  sitemapList (firstUrl : map articleToUrl articles)
  where
    articleToUrl (Entity _ article) =
      SitemapUrl { sitemapLoc        = ArticleR (articleListViewSlug article)
                 , sitemapLastMod    = Just (articleListViewPublished article)
                 , sitemapChangeFreq = Just Monthly
                 , sitemapPriority   = Nothing
                 }

getFeedR :: Handler TypedContent
getFeedR = do
  now      <- liftIO getCurrentTime
  articles <- runDB (getArticleListViews def)
  let upd  = articleListViewPublished . entityVal <$> listToMaybe articles
      feed =
        Feed { feedTitle       = "Meadowstalk"
             , feedAuthor      = "Blake Rain"
             , feedLinkSelf    = FeedR
             , feedLinkHome    = HomeR
             , feedDescription = "Blake Rain's Blog"
             , feedLanguage    = "en"
             , feedUpdated     = fromMaybe now upd
             , feedLogo        = Nothing
             , feedEntries     = map toFeedEntry articles
             }
  newsFeed feed
  where
    toFeedEntry (Entity _ article) =
      FeedEntry { feedEntryLink      = ArticleR (articleListViewSlug article)
                , feedEntryUpdated   = articleListViewPublished article
                , feedEntryTitle     = articleListViewTitle article
                , feedEntryContent   =
                    toMarkup $ markdown def (articleListViewSynopsis article)
                , feedEntryEnclosure = Nothing
                }

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
  setTitle "About"
  $(whamletFile "templates/pages/about.hamlet")

getArticleR :: Text -> Handler Html
getArticleR slug = do
  (Entity aid article, tags, author) <- runDB $ do
    article <- getBy404 (UniqueArticle slug)
    (article,, ) <$> getArticleTags (entityKey article)
                 <*> get404 (articleAuthor (entityVal article))
  now <- liftIO getCurrentTime
  defaultLayout $ do
    setTitleI (articleTitle article)
    addScript (StaticR lightbox_lightbox_min_js)
    addStylesheet (StaticR lightbox_lightbox_min_css)
    toWidget [julius|
    var lightbox = new Lightbox ();
    lightbox.load ();
    |]
    toWidget $(juliusFile "templates/pages/article.julius")
    $(whamletFile "templates/pages/article.hamlet")

getSearchR :: Handler Html
getSearchR = do
  defaultLayout $ do
    setTitle "Search"
    $(whamletFile "templates/pages/search.hamlet")

getSearchTagR :: TagId -> Handler Html
getSearchTagR tid = do
  tag <- runDB (get404 tid)
  defaultLayout $ do
    setTitleI (tagName tag)
    $(whamletFile "templates/pages/search.hamlet")

getContactR :: Handler Html
getContactR = defaultLayout $ do
  setTitle "Contact"
  $(whamletFile "templates/pages/contact.hamlet")
