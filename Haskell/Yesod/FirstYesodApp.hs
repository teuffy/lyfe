{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module FirstYesodApp where

import           Control.Applicative     ((<$>), (<*>))
import           Data.List               (sort)
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Yesod
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings,  mkMigrate "migrateAll"]
    [persistLowerCase|
           AdPosting
            title Text
            description Text
            contactEmail Text Maybe
            price Double Maybe
            deriving Show
    |]

data FirstYesodApp = FirstYesodApp ConnectionPool

mkYesod "FirstYesodApp" [parseRoutes|
/ HomeR GET
/addposting NewPostingR GET POST
/listadds ListAdsR GET
/posting/#AdPostingId AdPostingR GET
|]

instance Yesod FirstYesodApp

instance YesodPersist FirstYesodApp where
    type YesodPersistBackend FirstYesodApp = SqlBackend
    runDB action = do
        FirstYesodApp pool <- getYesod
        runSqlPool action pool


instance RenderMessage FirstYesodApp FormMessage where
    renderMessage _ _ = defaultFormMessage

data Creators = Creators { courseName :: String, peopleCount :: Int }
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                            <head>
                                <title>Functional Ads!
                            <div #navbar> <a href=@{NewPostingR}>Create new account</a>
                            <h1> Welcome to our first Yesod application!
                            <footer>
                                <p>This site was created by #{courseName creators}
                                \ Why not sort our name? #{sort (courseName creators)}
                                \ We are #{peopleCount creators} strong #
                                Next time we will be #{(*) 2 (peopleCount creators)} strong!
                        |]
                        where
                            creators = Creators "Haskell 101 - course" 5

adPostingAForm :: AForm Handler AdPosting
adPostingAForm = AdPosting
    <$> areq textField "Title" Nothing
    <*> areq textField "Description" Nothing
    <*> aopt emailField "Contact Email" Nothing
    <*> aopt doubleField "Price" Nothing

adPostingForm :: Html -> MForm Handler (FormResult AdPosting, Widget)
adPostingForm = renderTable adPostingAForm

getNewPostingR :: Handler Html
getNewPostingR = do
    (widget, enctype) <- generateFormPost adPostingForm
    defaultLayout
        [whamlet|
        <form method=post action="@{NewPostingR}" enctype="">
            ^{widget}
            <button>Submit me!
        |]

postNewPostingR :: Handler Html
postNewPostingR = do
    ((result, _), _) <- runFormPost adPostingForm
    case result of
        FormSuccess adPosting -> do
            adPostingId <- runDB $ insert adPosting
            redirect $ AdPostingR adPostingId
        _ -> defaultLayout
         [whamlet|
         <p> Something went rong m8
         |]

getListAdsR :: Handler Html
getListAdsR = undefined

getAdPostingR :: Handler Html
getAdPostingR = undefined

main :: IO ()
main = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ FirstYesodApp pool