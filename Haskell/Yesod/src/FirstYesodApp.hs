{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Applicative ((<$>), (<*>))
import           Data.List           (sort)
import           Data.Text
import           Yesod
import           Yesod.Form.Jquery
data FirstYesodApp = FirstYesodApp
data MyRoute = Main


mkYesod "FirstYesodApp" [parseRoutes|
/ HomeR GET
/newaccount NewAccountR GET
/login LoginR GET
|]

data Creators = Creators { courseName :: String, peopleCount :: Int }
instance Yesod FirstYesodApp
instance RenderMessage FirstYesodApp FormMessage where
    renderMessage _ _ = defaultFormMessage
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                            <head>
                                <title>Functional Ads!
                            <div #navbar> <a href=@{LoginR}>Log in</a>  / <a href=@{NewAccountR}>Create new account</a>
                            <h1> Welcome to our first Yesod application!
                            <footer>
                                <p>This site was created by #{courseName creators}
                                \ Why not sort our name? #{sort (courseName creators)}
                                \ We are #{peopleCount creators} strong #
                                Next time we will be #{(*) 2 (peopleCount creators)} strong!
                        |]
                        where
                            creators = Creators "Haskell 101 - course" 5

data User = User { username :: Text, password :: Text }
userAForm :: AForm Handler User
userAForm = User
    <$> areq textField "Username" Nothing
    <*> areq textField "Year" Nothing

userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderTable userAForm

getNewAccountR :: Handler Html
getNewAccountR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout
        [whamlet|
        <form method=post action="" enctype="">
            ^{widget}
            <button>Submit me!
        |]

getLoginR :: Handler Html
getLoginR = defaultLayout [whamlet|
|]

main :: IO ()
main = warp 3000 FirstYesodApp
