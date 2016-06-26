module Meadowstalk.Handlers.Admin
  where

import Yesod.Core

import Meadowstalk.Foundation
import Meadowstalk.Model

-------------------------------------------------------------------------------

getAdminR :: Handler Html
getAdminR = undefined

-------------------------------------------------------------------------------

getAdminArticlesR :: Handler Html
getAdminArticlesR = undefined

postAdminArticlesR :: Handler ()
postAdminArticlesR = undefined

getAdminArticleR :: ArticleId -> Handler Html
getAdminArticleR _ = undefined

postAdminArticleR :: ArticleId -> Handler ()
postAdminArticleR _ = undefined

deleteAdminArticleR :: ArticleId -> Handler ()
deleteAdminArticleR _ = undefined

-------------------------------------------------------------------------------

getAdminTagsR :: Handler Html
getAdminTagsR = undefined

postAdminTagsR :: Handler ()
postAdminTagsR = undefined

getAdminTagR :: TagId -> Handler Html
getAdminTagR _ = undefined

postAdminTagR :: TagId -> Handler ()
postAdminTagR _ = undefined

deleteAdminTagR :: TagId -> Handler ()
deleteAdminTagR _ = undefined

-------------------------------------------------------------------------------

getAdminUsersR :: Handler Html
getAdminUsersR = undefined

postAdminUsersR :: Handler ()
postAdminUsersR = undefined

getAdminUserR :: UserId -> Handler Html
getAdminUserR _ = undefined

postAdminUserR :: UserId -> Handler ()
postAdminUserR _ = undefined

deleteAdminUserR :: UserId -> Handler ()
deleteAdminUserR _ = undefined

-------------------------------------------------------------------------------

getAdminUserSignInR :: Handler Html
getAdminUserSignInR = undefined

postAdminUserSignInR :: Handler Html
postAdminUserSignInR = undefined

getAdminUserSignOutR :: Handler ()
getAdminUserSignOutR = undefined
