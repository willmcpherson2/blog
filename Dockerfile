FROM haskell:8

WORKDIR /opt/blog
RUN cabal update
COPY ./blog.cabal /opt/blog/blog.cabal
RUN cabal build --only-dependencies -j4
COPY . /opt/blog
RUN cabal install

CMD ["blog"]
