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
  - ```mkYesod``` is a Template Haskell function
  - ```parseRoutes``` as a QuasiQuoter

Both of those are extensions to Haskell, so in order to use them we have to add following lines at the top of our file:

```haskell
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
```

[#### Quickly about Template Haskell](https://wiki.haskell.org/Template_Haskell)

[#### Quickly about QuasiQuotes](https://wiki.haskell.org/Quasiquotation)

So right now, when we try to compile we got following error: ```Not in scope: `getHomeR'```. Uh oh, or should I say how cool! Haskell compiler is strict, and does not allow us to simply crete resource (hence the convetion to call resource resourceNameR) without corresponding function. But before we go further let's shortly talk about this line of code:

```/    HomeR    GET```

