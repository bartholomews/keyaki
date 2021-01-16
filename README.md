# ケヤキ

[![Build Status](https://gitlab.com/bartholomews/keyaki/badges/master/pipeline.svg)](https://gitlab.com/bartholomews/keyaki/pipelines/latest)
[![Docker Pulls](https://img.shields.io/docker/pulls/bartholomews/keyaki)](https://hub.docker.com/r/bartholomews/keyaki)
[![License: MIT](https://img.shields.io/badge/License-MIT-brown.svg)](https://opensource.org/licenses/MIT)


🔧 **This project is still early stage and very much WIP / experimental** 🔧  

## Haskell service

Seed from [servant-persistent](https://github.com/parsonsmatt/servant-persistent)

+ [`Haskell`](https://www.haskell.org/)  
+ [`Servant`](http://haskell-servant.github.io/)  
+ [`Persistent`](https://hackage.haskell.org/package/persistent)

## Elm client

Look at the [client package](https://github.com/bartholomews/keyaki/tree/master/client)

## Run locally

```bash
# Create a custom network if you want to use the app and db in docker without docker-compose
# (https://blog.linuxserver.io/2017/10/17/using-docker-networks-for-better-inter-container-communication/)
docker network create keyaki-network
```

#### Start the db

```bash
docker run -p 5432:5432 \
    -e POSTGRES_USER=test \
    -e POSTGRES_PASSWORD=test \
    -e POSTGRES_DB=keyaki \
    --network=keyaki-network \
    --name=keyaki-postgres \
    postgres
```

*from within the project, you can start the db locally with:*

```bash
docker-compose up
``` 

#### Run the app via docker

```bash
docker pull bartholomews/keyaki
docker run -it -p 8081:8081 \
    -e PG_USER='test' \
    -e PG_PASSWORD='test' \
    -e PG_HOST='keyaki-postgres' \
    --network=keyaki-network \
    --name=keyaki \
    bartholomews/keyaki
```

If you want to change the port you need to add the `PORT` env var as well.

#### Development

- Go to project folder

``` shell
cd keyaki
```

- Build project:

``` shell
stack build
```

Please note that in order to build `persistent-postgresql`, you need to have `pg_config` in your PATH.
Look at https://stackoverflow.com/questions/11618898/pg-config-executable-not-found if you have any issues.

- Execute app  
(make sure you have built the [client](https://github.com/bartholomews/keyaki/tree/master/client) first)
``` shell
stack exec keyaki
```

- Live reloading w/ `--file-watch`:
```shell
./scripts/dev.sh
```

- Live reloading w/ [ghcid](https://github.com/ndmitchell/ghcid)
(see also [this reddit thread](https://www.reddit.com/r/haskell/comments/7e24nx/code_reloading/))

``` shell
ghcid -c stack ghci -W -T main
```

Open http://localhost:8081

#### Run tests

``` shell
stack build --test
```

Please note that you *must* have the following env vars defined:
- `KEYAKI_PG_HOST`
- `KEYAKI_PG_PORT`
- `KEYAKI_PG_USER`
- `KEYAKI_PG_PASSWORD`
- `KEYAKI_PG_DATABASE`

#### Build a docker image locally

`docker build -f docker/local/Dockerfile -t bartholomews/keyaki .`

#### DB queries

by using [httpie](https://github.com/jkbrzt/httpie):

``` shell
# add an entry
http POST localhost:8081/api/entry meaning="keyaki" kana="ケヤキ"

# get an entry
http :8081/api/entry/1

# get all entries
http :8081/api/entries

# update an entry
http PUT :8081/api/entry/1 meaning="keyaki" kana="ケヤキ" active:=true

# delete an entry (TODO)
http DELETE :8081/api/entry/1
```

### Helpful Haskell / Servant stuff

- Haskell + Persistent: [http://www.yesodweb.com/book/persistent](http://www.yesodweb.com/book/persistent)

- School of Haskell "[Querying an existing database](https://www.schoolofhaskell.com/school/advanced-haskell/persistent-in-detail/existing-database)"

- Example Servant + Persistent: [https://github.com/haskell-servant/example-servant-persistent/](https://github.com/haskell-servant/example-servant-persistent/)

- Example Servant + Persistent by [Matt Parsons](https://github.com/parsonsmatt/): [https://github.com/parsonsmatt/servant-persistent](https://github.com/parsonsmatt/servant-persistent)

- Example Servant + Elm: [https://github.com/haskell-servant/example-servant-elm](https://github.com/haskell-servant/example-servant-elm)

- "Todobackend" with Servant: [https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-servant](https://github.com/jhedev/todobackend-haskell/tree/master/todobackend-servant)

- Album app (Haskell + Elm) by [Magnus Rundberget / @rundis](https://github.com/rundis): [https://github.com/rundis/albums](https://github.com/rundis/albums)

- DB example of "[5 Ways to Test Application Code that Accesses a Database in Haskell](https://github.com/cdepillabout/testing-code-that-accesses-db-in-haskell/)" 
