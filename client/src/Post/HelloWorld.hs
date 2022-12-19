module Post.HelloWorld where

import Reflex.Dom (Widget)
import Markup (m)

post :: Widget x ()
post = [m|

p[
I've finally bit the bullet and bought a domain and a real host. I have some
sort of irrational fear of monthly subscriptions that has prevented me from
subscribing to *[anything] until now. "The cost of a cup of coffee each month"
seems to have the opposite effect on me. Marketing targeted towards me should
instead say something like "0.1% of your salary".
]

p[
This site is being hosted on @[[Heroku]https://heroku.com] as a Docker container
containing a small Haskell server. In this post I'll discuss each technology
and make some recommendations.
]

#[Heroku]

p[
I used Heroku throughout my undergrad course and have always enjoyed using it.
For this site I originally tried to use DigitalOcean because it was recommended
and cheap, but found it less intuitive to use. I don't want to definitively say
that it's a worse service, but I will say that I gave up on it even after
spending $5.
]

p[
I also considered using GitHub pages, but I wanted this to be a programming
project rather than just a static blog.
]

p[
Heroku has a pretty generous free-tier which only has one noticeable limitation
for small-scale projects, which is that your container sleeps after 30 minutes
of inactivity and can therefore take a few seconds to warm up. If you need to
demo for your teacher or boss, just visit the site beforehand and it'll be
ready to go. A free-tier like this is really indispensable for those who need
to shop around and consider their options.
]

p[
Heroku also has very decent documentation and interfaces, both of which are
necessary if you're going to spend a day fiddling around with settings and
configs.
]

p[
Interacting with Heroku is pretty KISS. Need to containerise? Just add a
@[[Dockerfile]https://github.com/willmcpherson2/blog/blob/main/Dockerfile] and
@[[3 lines of YAML]https://github.com/willmcpherson2/blog/blob/main/heroku.yml]
and run `[heroku stack:set container]. Want to use a regular shell script
instead? Just link to a git repository containing the script from your
@[[app.json]https://github.com/willmcpherson2/notcord/blob/main/app.json]. Time
to push? Just `[git push heroku].
]

#[Docker]

p[
I wanted to use Docker because there's an @[[official Haskell
image]https://hub.docker.com/_/haskell] which means I can totally skip the
toolchain installation. I ended up being able to copy-and-paste one of their
Dockerfile examples and only had to change some names. Then it's just that bit
of YAML and that `[heroku stack:set container] thing.
]

p[
That's about 1 minute of actual typing, but there were multiple hours trying to
get DigitalOcean to work, switching to Heroku in desperation and then finally
figuring out the right 13 lines of configuration. I had plenty of time to be
jaded and think about "the state of computing". Instead of that pessimistic
rant, I will try to give a more cool-headed state of the union.
]

##[State of the Union]

p[
Platform agnosticism is not really happening. The primary reason I had to spend
hours deploying a simple blog is that I'm using an ARM machine. We really
haven't successfully abstracted over the ISA layer. Docker on my ARM machine
doesn't give the same results as Docker on an x86 cloud machine, and while I
can use an x86 Docker image on my ARM machine, it will just produce a third set
of results. I don't expect Docker to solve this problem, obviously, but it has
become the de facto solution and is clearly more of a bandaid.
]

p[
What's the opposite of platform agnosticism? Platform theism? I think it's
worth considering as an alternative solution. The easiest way to write a cloud
application is to just us an Ubuntu machine, since that's what's running on the
cloud. The easiest (only?) way to write a macOS application is to just use
macOS.
]

#[Haskell]

##[For Servers]

p[
Web servers in Haskell are great. And I don't even have to tell you about some
cool new library that encodes your routes at the type level. I stuck to the
basics:
]

`[
build-depends:
  base
  , directory
  , bytestring
  , text
  , warp
  , wai
  , http-types
]

p[
The whole gist of the server is this case expression:
]

`|[
router :: Method -> Path -> IO (Status, Content)
router method path = case (method, path) of
  ("GET", []) -> index "posts"
  ("GET", ["posts"]) -> index "posts"
  ("GET", ["posts", title]) -> serveFilePath $ "posts/" ++ unpack title
  ("GET", ["www", "style.css"]) -> serveRaw "www/style.css"
  _ -> notFound
]|

p[
You can't write a white paper about it, but it works.
]

p[
I'm definitely one of those "Haskell is the best imperative language" guys.
Laziness and purity interact surprisingly well with side-effects because of the
great membrane which is monadic IO. In Haskell you really can say that a value
is a "computation" and that a monadic value is an "action".
]

##[For Compilers]

p[
Did you really think you could read about Haskell without also reading about
compilers? This server also contains a little markdown compiler. Here's the
language definition ripped right out of the source code:
]

`|[
blockParsers :: [ParseMaybe]
blockParsers =
  [ mkParser Rule "---" "\n"
  , mkParser Title "# " "\n"
  , mkParser Heading "## " "\n"
  , mkParser Subheading "### " "\n"
  , mkParser CodeBlock "``\n" "\n``\n"
  , mkParser Paragraph "" "\n\n"
  ]

elementParsers :: [ParseMaybe]
elementParsers =
  [ mkParser Bold "**" "**"
  , mkParser Italics "*" "*"
  , mkParser CodeInline "`" "`"
  , mkParser Link "[" "]"
  ]
]|

p[
You can see how it's mostly just markdown with some quirks. The code generator
spits out the HTML tags that you'd expect. There are some things I'd like to
add in order to supplant the need for writing HTML completely, since like most
people I hate typing or seeing XML.
]

|]
