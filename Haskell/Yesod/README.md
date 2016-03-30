# Notes about MyFirstYesodApp

### Starting point

In order to create simple working Yesod webapp, we do not need too much. First we need to create our own ```Type```. So it would like this:

```haskell
data MyFirstYesodApp = MyFirstYesodApp
```

Then of course we have to create our Yesod application with our type, in order to do se we will use function ```mkYesod``` of type ```String -> [ResourceTree String] -> Q [Dec]```. Do not worry too much about the type signature right now, you'll get to know and understand them (more or less) while writing more code. So lets make Yesod.

```haskell
mkYesod "MyFirstYesodApp" [parseRoutes|
/    HomeR     GET
|]
```

Okay, so in here Yesod and Haskell feature begin to present themselves. First let's state following things:
  - ```mkYesod``` is a [Template Haskell function](https://wiki.haskell.org/Template_Haskell)
  - ```parseRoutes``` as a [QuasiQuoter](https://wiki.haskell.org/Quasiquotation)

Both of those are extensions to Haskell, so in order to use them we have to add following lines at the top of our file:

```haskell
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
```

So right now, when we try to compile we got following error: ```Not in scope: `getHomeR'```. Uh oh, or should I say how cool! Haskell compiler is strict, and does not allow us to simply crete resource (hence the convetion to call resource resourceNameR) without corresponding function. But before we go further let's shortly talk about this line of code:

```haskell
/    HomeR    GET
```

In order to controll request Yesod follows a front controll pattern. That means every request entres the same point and then it is being further dispatched via declared - by us - routes. In this case we have our endpoint at root of our application ```\``` that serves GET requests using HomeR resource. And that's why we've got our compilation error - we did not declare any function to do so! Yesod create some code behind the scenes and that's why compiler already knows that function has to be named ```getHomeR```. Those functions are called Handlers. First type declaration:

```haskell
getHomeR :: Handler Html
```

As this is our first app we will only be doing Html content, but have in mind there is also a way to create Handlers for JSON and others!

Of course we need our acompaning binding - in this case:

```haskell
getHomeR = defaultLayout [whamlet|Hello World!|]
```

defaultLayout function is a handler function, a very common type of function in Yesod. It wraps content that we give it in our site's template. It produces html, head and body tags. Of course there is possibility to override this function using Yesod's typeclasses, but that is quite out of scope of this simple tutorial.

whamlet is quasi-quoter (same as parseRoutes, remember?). It translates Hamlet syntax into Yesod's Widgets (we will use a little bit of them, please be patient). Hamlet is standard HTML templating engine in Yesod.

So... when we try to compile - we get more compile errors!

We have to add those extra Language features:

```haskell
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
```
and instance of our Yesod application (in this case it is not very interesting instance of typeclass, but in here we will declare different settings of our app):

```haskell
instance Yesod MyFirstYesodApp
```

Still we are missing some way of starting our app! For that we will have simple ```IO ()``` function that will allow us to communicate with outisde world.

```haskell
main :: IO ()
main = warp 3000 MyFirstYesodApp
```

So in summary that is what we should have by now:

```haskell
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod

data MyFirstYesodApp = MyFirstYesodApp

mkYesod "MyFirstYesodApp" [parseRoutes|
/    HomeR    GET
|]

instance Yesod MyFirstYesodApp

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 MyFirstYesodApp
```

Please remember that in order to compile this module you need to install Yesod package with cabal.

### Some fun and structure

Woah, that was something! But any web application - even written in cool functoinal way - without any functionality is not something we are aiming for. So let's create simple site on which we will be able to post and list simple advertisements. Let's the journey begin!

#### Links

Okay, so we will definetly need at least two more pages - one for listing ads, second for adding one. Let's get our hands dirty - time to define two more resources.

```haskell
mkYesod "MyFirstYesodApp" [parseRoutes|
/            HomeR       GET
/addposting  NewPostingR GET
/listads     ListAdsR    GET
|]
```
As you now know - in order to compile our code we need to create two more functions - one is ```getNewPostingR``` and the second one is ```getListAdsR```. Both of them will be of type ```Handler Html```. For now please do not care about content of those pages:

```haskell
getNewPostingR :: Handler Html
getNewPostingR = defaultLayout [whamlet||]

getListAdsR :: Handler Html
getListAdsR = defaultLayout [whamlet||]
```

Okay, now we should put links into our Home resource.

```haskell
getHomeR = defaultLayout [whamlet|
 <div>
   <a href=@{NewPostingR}>Add posting
   <a href=@{ListAdsR}>List ads!
 |]
```

This is giving us possibility to talk about two things:
 - Hamlet syntax
 - Links in Yesod

##### Links

The first thinkg we have to note, that ```NewPostingR``` and ```ListAdsR``` are data constructors. That means that Haskell compiler treats those as any other Haskell value, making them type-safe. This gives us a huge amount of flexibility and possibility to compose them as any other Haskell value.

##### Hamlet syntax (and others too)

[Shakespearean Templates](http://www.yesodweb.com/book/shakespearean-templates)

#### Widgets

Of course adding the same links would mean copying. And that is something we do not like in any programming paradigm. That is where Yesod's Widgets come into play. Now we should declare our first Widget that will be used as a navigation bar:
```haskell
navbar :: Widget
navbar = do
    toWidget
        [hamlet|
            <div #navbar>
                <a href=@{HomeR}>Main Page</a> / #
                <a href=@{NewPostingR}>Add new ad</a> / #
                <a href=@{ListAdsR}>List current ads</a>
        |]
```
So we just declared our first widget - as you can see we already came to the Monads, but do not worry - as you can see they are not scary at all, in this case we could use similar - or same - functions to ```toWidget``` to add more content - css, javascript and so on (in type-safe manor of course). Second step is to use it in all of our resources:
```haskell
getHomeR = defaultLayout [whamlet|^{navbar}|]
getNewPostingR = defaultLayout [whamlet|^{navbar}|]
getListAdsR = defaultLayout [whamlet|^{navbar}|]
```
Pretty easy, isn't it? As you can see Widget is normal function, so we can pass arguments into it and operate on them (we will do it later). Okay, now I think we can create another Widget, that will represent the footer for our pages. Also now I'd like to show you possibility to use Haskell function within our widgets (so basically within our HTML/css/js). First let's declare new Type, that will represent creators of this page!
```haskell
data Creators = Creators { courseName :: String, peopleCount :: Int }
```
Now, it's time for our new Widget
```haskell
footer :: Widget
footer = do 
    to Widget [hamlet|
                   <footer #footer>
                     <p>This site was created by #{courseName creators}
                     \ Why not sort our name? #{sort (courseName creators)}
                     \ We are #{peopleCount creators} strong #
                     Next time we will be #{(*) 2 (peopleCount creators)} strong!
              |]
                where creators = Creators "Haskell 101 Course" 5
  
```
That is something, isn't it? So by know we now basics of using ```Widgets``` and Type-safe links. To sum up, right now we should have something like this (please note extra imports):
```haskell
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
module Sandbox where 

import    Yesod
import    Data.List    (sort)

data MyFirstYesodApp = MyFirstYesodApp

mkYesod "MyFirstYesodApp" [parseRoutes|
/            HomeR       GET
/addposting  NewPostingR GET
/listads     ListAdsR    GET
|]

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


getNewPostingR :: Handler Html
getNewPostingR = defaultLayout [whamlet|
  ^{navbar}
  ^{footer}
|]

getListAdsR :: Handler Html
getListAdsR = defaultLayout [whamlet|
  ^{navbar}
  ^{footer}
|]

main :: IO ()
main = warp 3000 MyFirstYesodApp
```

### Forms

In order to add data - in user friendly manor - we need some kind of form, in which we will be able to create our Advertisement entity. Yesod delivers and Yesod devilers with a bang. Package ```yesod-form```. After [Yesod page](http://www.yesodweb.com/book/forms) Yesod's forms allows to:
 - Ensure data is valid
 - Convert data in the form into Haskell datatypes
 - Generate HTML code for displaying the form
 - Generate js for clientside validation
 - Combine simpler forms to create more complex ones
 - Assign names to our fields that are guaranteed to be unique.

We need to declare ```Type``` of our Advertisement:
```haskell
data AdPosting = AdPosting { title :: Text, description :: Textarea, contactEmail :: Maybe Text, price :: Maybe Double } deriving Show
```
We also need to import Text, so our module will compile:
```haskell
import    Data.Text
```

Okay, so first we'll declare form, then I'll try to explain its elements:
```haskell
adPostingAForm :: AForm Handler AdPosting
adPostingAForm = AdPosting
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Description" Nothing
    <*> aopt emailField "Contact Email" Nothing
    <*> aopt doubleField "Price" Nothing
```
At the first glance it may seem scary, but when you'll think about it is very readable - even if you do not know all the operators, but let's try to take it apart and understand:
 - First thing to note is type ```AForm``` it means that our form is ```Applicative```. To understand what does that mean please see [Learn you a Haskell](http://learnyouahaskell.com/functors-applicative-functors-and-monoids). But what we really need to know now is it allow us to create forms in very high-level and declarative way.
 - areq function has a type (simplified): ```Field a -> FieldSettings -> Maybe a -> AForm a```. First argument specifies datatype of field, in our case it is ```textField```, ```textareaField```, ```emailField``` or ```doubleField```. As you can see based on this Yesod will choose how to parse and render it. Next argument ```FieldSettings``` takes ID and label of given field. Last parameter ```Maybe a``` provides place for a default value (Hint: editing existing entities). Aopt is basically the same, but it is optional field whereas req is required field in our form (Remember? Yesod provides validation and this is part of it).

When we do try to compile first get error about missing import from Applicative - <*> and <$>, then something new:
```haskell
No instance for (RenderMessage MyFirstYesodApp FormMessage)
      arising from a use of `areq'
```

```yesod-form``` uses its messages in terms of ```FormMessage``` datatype. So in order to use it we need instance of ```RenderMessage```. If you would like to dig deeper please visit [Yesod site](http://www.yesodweb.com/book/forms#forms_i18n). For now we can be content with default instance:
```haskell
instance RenderMessage MyFirstYesodApp FormMessage where
    renderMessage _ _ = defaultFormMessage
```
As RenderMessage is a typeclass that takes two parameters we have to use additional language extension:
```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
```

We declared our form, now we need to add it to existing resource. First let's ```renderTable```
```haskell
adPostingForm :: Html -> MForm Handler (FormResult AdPosting, Widget)
adPostingForm = renderTable adPostingAForm
```
So we created ```MForm Handler``` with ```FormResult``` and a ```Widget``` in tupled form. Careful reader may notice that we should be using applicative form, but we still converted into monadic one. Why? How come? It is all about code reuse. In our resource we can use monadic forms, so ```renderTable``` get's us there. And finally, having our form as monadic form we can add it in very safe manor.

```haskell
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
```

So as you can see we chained our expressions with ```do``` operator, so basically we bind result of ```generateFormPost``` to content of our ```defaultLayout``` function. 

Of course now our fresh new form does not do much - mainly because of the lack of POST resource that will correspond to ```NewPostingR```. Let's solve that problem.
```haskell
/addposting  NewPostingR GET POST
```
Now compiler will start complaining about lack of ```postNewPostingR```. As we like our compiler friend, we should try to make it happy
```haskell
postNewPostingR :: Handler Html
postNewPostingR = undefined
```
It is not very nice way to go, because now whenever we try to use ```postNewPostingR``` we will receive error! But as I do not want to simply show result of our form we will do following things
 - Create database connection to sqlite database.
 - Result of the form will be inserted into our database.
 - Add new resource that will show result of our single insert action.
 - extend ```getListAdsR``` method, so it will show all ads.

As a first step we need to extends our ```MyFirstYesodApp``` data type by adding a connection pool
```haskell
data FirstYesodApp = FirstYesodApp ConnectionPool
```
We need to add proper sqlite import
```haskell
import Database.Persist.Sqlite
```
Now we should start to listen our complier friend
```haskell
    No instance for (YesodDispatch (ConnectionPool -> MyFirstYesodApp))
      arising from a use of `warp'
```
As our ```main``` method is a ```IO ()``` monad we can chain expressions by using ```do``` operator
```haskell
main = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db3" 10
    warp 3000 $ MyFirstYesodApp pool
```
Yesod also (by default) forces us to use ```runStderrLoggingT```, so we need to add an import
```haskell
import Control.Monad.Logger (runStderrLoggingT)
```
And if you are scared of ```$``` operator, just note that it means nothing more than "first evaluate expression on the right side and then pass it to the expression on the left side""". So we created connection pool for our database that will be named ```test.db3``` with ```ConnectionPool``` of size ```10```.

Now we need to create schema and share entities between database and our code, in order to do use we can use helper function called ```share``` which passes information from persistent block and shares it to each ```TemplateHaskell``` function, finally concatenating the result.
```haskell
share [mkPersist sqlSettings,  mkMigrate "migrateAll"]
    [persistLowerCase|
           AdPosting
            title Text
            description Textarea
            contactEmail Text Maybe
            price Double Maybe
            deriving Show
    |]
```
And now our nagging compiler friend is complaining. We need to add language extension
```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
```
Now in our ```share``` function we have
 - ```mkPresist``` that takes list of entites and creates one Haskell datatype for each and one ```PersistEntity``` instance for each datatype defined.
 - ```mkMigrate``` creates new function that will perform migration on all entities defined in ```persist``` block.

But even after all that we still get error (and interesting one!)
```haskell
Multiple declarations of `AdPosting'
```

As we stated before ```share``` function does all the declarations for us, so we can remove our old datatype declaration ```data AdPosting = ...```. We also need some place to actually perform migration of our database, and ```main``` method looks - at least for me -- as perfect place.
```haskell
main = do
    pool <- runStderrLoggingT $ createSqlitePool "test.db3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ MyFirstYesodApp pool
```

Still we miss one more thing - instance of ```YesodPersist``` for our app
```haskell
instance YesodPersist FirstYesodApp where
    type YesodPersistBackend FirstYesodApp = SqlBackend
    runDB action = do
        FirstYesodApp pool <- getYesod
        runSqlPool action pool
```
Here we also declared general ```runDB``` function that will make easier performing operations on our database. Now we are ready to write our ```postNewPostingR``` method
```haskell
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
```
As you can see all we care about in here is ```result``` from ```runFormPost``` method. If it is instance of ```FormSuccess`` we insert our entity into database and redirect to ```AdPostingR``` with ```adPostingId``` as a parameter. In any other case we just print "Something went wrong!". If you are cautius you should see that we did not declare any ```AdPostingR``` resource. Let's do this now
```haskell
mkYesod "MyFirstYesodApp" [parseRoutes|
/            HomeR       GET
/addposting  NewPostingR GET POST
/listads     ListAdsR    GET
/posting/#AdPostingId   AdPostingR      GET
|]
```
Here you can see that we are using path parameter named ```#AdPostingId```. In order to do so we need language extendsion
```haskell
getAdPostingR :: AdPostingId -> Handler Html
getAdPostingR adPostingId = do
    [Entity _ (AdPosting title desc maybeEmail maybePrice)] <- runDB $ selectList [AdPostingId ==. adPostingId] []
    defaultLayout
        [whamlet|
            ^{navbar}
            ^{footer}
        |] 
```
Before we just put output into our resource let's recognize one thing - we can create reusable ```Widget``` that will be used both in ```getAdPostingR``` and (forgotten by now) ```getListsAdsR```.
```haskell
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
```
Interesting thing here is argument pattern matching, so within parameter of our method we extract fields of our ```AdPosting```. Also we show ```email``` and ```price``` only when they are not empty. Knowing that we can extend ```getAdPostingR```
```haskell
getAdPostingR adPostingId = do
    [Entity _ (AdPosting title desc maybeEmail maybePrice)] <- runDB $ selectList [AdPostingId ==. adPostingId] []
    defaultLayout
        [whamlet|
            ^{navbar}
            ^{adContainerWidget (AdPosting title desc maybeEmail maybePrice)}
            ^{footer}
        |] 
```
and ```getListsAdsR```
```haskell
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
```
Unfortunatelly after that we got compiler error ```Ambiguous occurrence `null'```. We can easly solve it by updating our import
```haskell
import           Data.Text        hiding (null)
```
and - another - language extension
```haskell
{-# LANGUAGE ViewPatterns               #-}
```

So, to sum up - we have fully functioning application (with out any css, which makes it a little bit ugly) written in type-safe, functional and easy to extend manor
```haskell
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
```

