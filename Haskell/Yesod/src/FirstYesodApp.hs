{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import           Data.List (sort)

data FirstYesodApp = FirstYesodApp
data MyRoute = Main


mkYesod "FirstYesodApp" [parseRoutes|
/ HomeR GET
|]

data Creators = Creators { courseName :: String, peopleCount :: Int }
instance Yesod FirstYesodApp
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                            <head>
                                <title>#{pageTitle}
                            <div #navbar> Log in  / Create account
                            <h1 #> Welcome to our first Yesod application!
                            <div>
                            <footer>
                                <p>This site was created by #{courseName creators}
                                \ Why not sort our name? #{sort (courseName creators)}
                                \ We are #{peopleCount creators} strong #
                                Next time we will be #{(*) 2 (peopleCount creators)} strong!
                        |]
                        where 
                            creators = Creators "Haskell 101 - course" 5
                            pageTitle :: String
                            pageTitle = "Functionall Ads!"

main :: IO ()
main = warp 3000 FirstYesodApp