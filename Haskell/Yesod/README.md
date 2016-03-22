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
and instance of our Yesod application:

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

mkYesod "HelloWorld" [parseRoutes|
/    HomeR    GET
|]

instance Yesod MyFirstYesodApp

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 MyFirstYesodApp
```

Please remember that in order to compile this module you need to install Yesod package with cabal.
