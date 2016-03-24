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
```
