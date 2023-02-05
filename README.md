# [willmcpherson2.com](http://willmcpherson2.com/)

```sh
# dev build
nix-shell -A shells.ghc --run server-reload
nix-shell -A shells.ghcjs --run client-reload

# release build
nix-build
./result/bin/server result/static/

# docker image
nix-build docker.nix
docker load < result
docker run -p 8000:8000 registry.heroku.com/willmcpherson2/web:latest

# deploy to heroku
heroku login
heroku container:login
docker push registry.heroku.com/willmcpherson2/web:latest
heroku container:release web -a willmcpherson2
```
