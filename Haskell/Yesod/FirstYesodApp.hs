{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}


import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad.Logger         (runStderrLoggingT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.List                    (sort)
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Yesod

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
navbar :: Widget
navbar = do
    toWidget
        [hamlet|
            <div #navbar>
                <a href=@{HomeR}>Main Page</a> / #
                <a href=@{NewPostingR}>Add new ad</a> / #
                <a href=@{ListAdsR}>List current ads</a> / #
        |]
footer :: Widget
footer = do
    toWidget
        [hamlet|
            <footer>
                <p>This site was created by #{courseName creators}
                \ Why not sort our name? #{sort (courseName creators)}
                \ We are #{peopleCount creators} strong #
                Next time we will be #{(*) 2 (peopleCount creators)} strong!
        |]
        where creators = Creators "Haskell 101 - course" 5
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                            <head>
                                <title>Functional Ads!
                            ^{navbar}
                            <h1> Welcome to our first Yesod application!
                            <div #container>
                                Welcome to our magnificent site in which everything is done in Haskell!
                                Why? Because it causes pleasureable headaches and makes you rethink a lot of stuff!
                            ^{footer}
                        |]

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
    (form, enctype) <- generateFormPost adPostingForm
    defaultLayout
        [whamlet|
        ^{navbar}
        <form method=post action="@{NewPostingR}" enctype="">
            ^{form}
            <button>Submit me!
        ^{footer}
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
         <p> Something went wrong m8
         |]

getListAdsR :: Handler Html
getListAdsR = do
    ads <- runDB $ selectList [] [Desc AdPostingId]
    defaultLayout
        [whamlet|
            ^{navbar}
            <ul>
            $forall ad <- ads
                <li>#{show ad}
            ^{footer}
         |]

getAdPostingR :: AdPostingId -> Handler Html
getAdPostingR adPostingId = do
    [Entity _ (AdPosting title desc email price)] <- runDB $ selectList [AdPostingId ==. adPostingId] []
    defaultLayout
        [whamlet|
            ^{navbar}
            <p>#{title}
            <p>#{desc}
            <p>#{show email}
            <p>#{show price}
            ^{footer}
        |] where
            extractFromEntity :: [(Entity a)] -> a
            extractFromEntity [(Entity _ a)] = a



main :: IO ()
main = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ FirstYesodApp pool
