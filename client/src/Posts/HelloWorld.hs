module Posts.HelloWorld where

import Markup.M (m)
import Reflex.Dom (Widget)

post :: Widget x ()
post =
  [m|

p[
This post is an overview of how I made this website. The source code can be found here:
]

p[@[[github.com/willmcpherson2/willmcpherson2.com]https://github.com/willmcpherson2/willmcpherson2.com]]

#[The Stack]

p[@[[Reflex]https://github.com/reflex-frp/reflex] is a Haskell library for functional reactive programming.]

p[@[[Reflex Platform]https://github.com/reflex-frp/reflex-platform] is a set of Nix packages and expressions for building cross-platform Reflex applications.]

p[
Reflex Platform provides @[[GHC]https://gitlab.haskell.org/ghc/ghc] and @[[GHCJS]https://github.com/ghcjs/ghcjs] for compiling Haskell to native code and JavaScript respectively.
Using one language for your website is great, but using two compilers is a bit tricky.
Fortunately, @[[GHC now has its own JavaScript backend]https://engineering.iog.io/2022-12-13-ghc-js-backend-merged], so this will improve over time.
]

p[
Reflex Platform provides deterministic Nix builds, but also incremental builds via @[[Cabal]https://github.com/haskell/cabal].
]

p[
To get basic hot-reloading, I just used @[[entr]https://github.com/eradman/entr] to `[cabal build] the frontend and `[cabal run] the server.
]

#[Usage]

``
|# Deterministic, optimised build
|nix-build
|
|# Development environment with GHCJS
|nix-shell -A shells.ghcjs
|# Rebuild the fronted on change
|client-reload 
|
|# Development environment with GHC
|nix-shell -A shells.ghc
|# Restart the server on change
|server-reload

#[Code]

p[
We have server-side routing which just provides resources:
]

p[`[*[server/src/Main.hs]]]

``
|route :: String -> [String] -> Response
|route dir = \case
|  ["style.css"] -> res dir "style.css" "text/css"
|  ["all.js"] -> res dir "all.js" "text/javascript"
|  ["favicon.ico"] -> res dir "favicon.ico" "image/x-icon"
|  _ -> res dir "/index.html" "text/html"

p[
Then we have client-side routing based on `[window.location.pathname]:
]

p[`[*[client/src/Main.hs]]]

``
|route :: String -> Widget x ()
|route pathname = case filter (not . null) $ splitOn "/" pathname of
|  [] -> index
|  ["posts"] -> previews
|  ["posts", name] -> post name
|  ["tulip"] -> tulip
|  ["markup"] -> markup
|  _ -> notFound

p[
We are doing something quite weird which is that we have a custom markup language available as a quasiquoter.
This post was actually written in a Haskell source file!
You can try out the @[[live editor]/markup].
]

#[The Good]

p[
Nix really shines in these situations where you want to coordinate a complicated set of tools.
It goes beyond package management and is really more of a general tool for software configuration.
Also, it provides a means of abstraction, which is how Reflex Platform is able to provide the @[`[project]https://github.com/reflex-frp/reflex-platform/blob/master/project/default.nix] expression that is much more than just a package.
]

p[
I'm not too deep into functional reactive programming yet, but I will say that `[reflex] and `[reflex-dom] are pretty easy to get started with.
And actually that's a point I would like to make: even if you don't quite understand FRP and just want to make a website, Reflex is a good choice.
I will recommend @[[reflex-dom-inbits]https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md] tutorial which is a nice practical introduction to Reflex.
]

#[The Bad]

p[
Nix is a great source of sanity, but it is often poorly documented.
My wish is that Nix had a strong, expressive type system to make it easier to know how a given expression can be used.
]

p[
Reflex Platform doesn't provide a working `[haskell-language-server]!
That's ok since you can just add a haskell-language-server package, but it needs to work with GHC 8.6.5, so some digging is required to find one that works.
It would be nice if Reflex did that work for us.
]

#[Conclusion]

p[
I'm really looking forward to the future of Haskell, GHC, haskell-language-server, FRP and Nix.
They represent a much more structured and "complete" way to write software.
]

|]
