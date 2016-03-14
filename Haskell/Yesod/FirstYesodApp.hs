{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module FirstYesodApp where

import           Control.Applicative     ((<$>), (<*>))
import           Data.List               (sort)
import           Data.Text
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Yesod
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkSave "entityDefs"]
    [persistLowerCase|
           AdPosting
            title Text
            description Text
            contactEmail Text Maybe
            price Double Maybe
            deriving Show
    |]

-- createPersistenceModule :: IO ()
-- createPersistenceModule = runSqlite ":memory:" $ do
--    runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe AdPosting)

data FirstYesodApp = FirstYesodApp ConnectionPool

mkYesod "FirstYesodApp" [parseRoutes|
/ HomeR GET
/addposting NewPostingR GET POST
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
            defaultLayout
                [whamlet|
                #{show adPosting}
                |]
        _ -> defaultLayout
         [whamlet|
         <p> sth went wrong
         |]


main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> liftIO $ do
    warp 3000 $ FirstYesodApp pool
