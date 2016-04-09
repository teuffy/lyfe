{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad.Logger    (runStderrLoggingT)
import           Data.List               (sort)
import           Data.Text               hiding (null)
import           Database.Persist.Sqlite
import           Yesod

data MyFirstYesodApp = MyFirstYesodApp ConnectionPool

share [mkPersist sqlSettings,  mkMigrate "migrateAll"]
    [persistLowerCase|
           AdPosting
            title Text
            description Textarea
            contactEmail Text Maybe
            price Double Maybe
            deriving Show
    |]

mkYesod "MyFirstYesodApp" [parseRoutes|
/            HomeR       GET
/addposting  NewPostingR GET POST
/listads     ListAdsR    GET
/posting/#AdPostingId   AdPostingR      GET
|]

instance YesodPersist MyFirstYesodApp where
    type YesodPersistBackend MyFirstYesodApp = SqlBackend
    runDB action = do
        MyFirstYesodApp pool <- getYesod
        runSqlPool action pool

instance Yesod MyFirstYesodApp

data Creators = Creators { courseName :: String, peopleCount :: Int }

navbar :: Widget
navbar = do
    toWidget
        [hamlet|
            <div #navbar>
                <a href=@{HomeR}>Main Page</a> / #
                <a href=@{NewPostingR}>Add new ad</a> / #
                <a href=@{ListAdsR}>List current ads</a>
        |]

footer :: Widget
footer = do
    toWidget [hamlet|
                 <footer #footer>
                     <p>This site was created by #{courseName creators}
                     \ Why not sort our name? #{sort (courseName creators)}
                     \ We are #{peopleCount creators} strong #
                     Next time we will be #{(*) 2 (peopleCount creators)} strong!
              |]
              where creators = Creators "Haskell 101 Course" 5

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
  ^{navbar}
  ^{footer}
|]


instance RenderMessage MyFirstYesodApp FormMessage where
    renderMessage _ _ = defaultFormMessage

adPostingAForm :: AForm Handler AdPosting
adPostingAForm = AdPosting
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Description" Nothing
    <*> aopt emailField "Contact Email" Nothing
    <*> aopt doubleField "Price" Nothing

adPostingForm :: Html -> MForm Handler (FormResult AdPosting, Widget)
adPostingForm = renderTable adPostingAForm


getNewPostingR :: Handler Html
getNewPostingR = do
    (form, _) <- generateFormPost adPostingForm
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
         <p> Something went wrong!
         |]

adContainerWidget :: AdPosting -> Widget
adContainerWidget (AdPosting title desc maybeEmail maybePrice) = do
    toWidget
        [hamlet|
             <div #adPosting>
                <div #adTitle> #{title}
                <div #adContent> #{desc}
                <div #adFooter>
                $maybe email <- maybeEmail
                    <span #adEmail> #{show email}
                $maybe price <- maybePrice
                     <span #adPrice> #{show price}
        |]

getAdPostingR :: AdPostingId -> Handler Html
getAdPostingR adPostingId = do
    [Entity _ (AdPosting title desc maybeEmail maybePrice)] <- runDB $ selectList [AdPostingId ==. adPostingId] []
    defaultLayout
        [whamlet|
            ^{navbar}
            ^{adContainerWidget (AdPosting title desc maybeEmail maybePrice)}
            ^{footer}
        |]

getListAdsR :: Handler Html
getListAdsR = do
    ads <- runDB $ selectList [] [Desc AdPostingId]
    defaultLayout
        [whamlet|
            ^{navbar}
            $if null ads
                <h1>Sorry, no ads yet!
            $else
                $forall Entity _ (AdPosting title desc maybeEmail maybePrice) <- ads
                    ^{adContainerWidget (AdPosting title desc maybeEmail maybePrice)}
            ^{footer}
         |]

main :: IO ()
main = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ MyFirstYesodApp pool
