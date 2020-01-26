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

- Execute app

``` shell
stack exec keyaki
```

- Live reloading w/ `--file-watch`:
```shell
./server/scripts/dev.sh
```

- Live reloading w/ [ghcid](https://github.com/ndmitchell/ghcid)
(see also [this reddit thread](https://www.reddit.com/r/haskell/comments/7e24nx/code_reloading/))

``` shell
ghcid -c stack ghci -W -T main
```

- run tests

``` shell
stack build --test
```

#### Build a docker image locally

`docker build -f docker/local/Dockerfile -t bartholomews/keyaki .`

#### DB queries

by using [httpie](https://github.com/jkbrzt/httpie):

``` shell

# add a todo
http POST localhost:8080/todo/ completed:=false description="my todo"

# get a todo
http localhost:8080/todo/1

# delete a todo
http DELETE localhost:8080/todo/1

# update a todo
http PUT localhost:8080/todo/1 description="any other description" completed:=true

# get all todos
http localhost:8080/todos

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