# ケヤキ

Following the [elm guide](https://guide.elm-lang.org)
Webpack 4, Hot Reloading & Babel 7 seed from [elm-webpack-starter](https://github.com/simonh1000/elm-webpack-starter)

## Installation

```sh
$ git clone git@github.com:simonh1000/elm-webpack-starter.git new-project
$ cd new-project
$ yarn
$ yarn dev
 ```

Open http://localhost:3000.
(An example using Routing is provided in the `navigation` branch)

## Production

Build production assets (js and css together) with:

```sh
yarn run prod
```

## Static assets

Just add to `src/assets/` and the production build copies them to `/dist`

## Testing

[Install elm-test globally](https://github.com/elm-community/elm-test#running-tests-locally)

`elm-test init` is run when you install your dependencies. After that all you need to do to run the tests is

```
yarn test
```

Take a look at the examples in `tests/`

If you add dependencies to your main app, then run `elm-test --add-dependencies`

<!-- I have also added [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples) and provided an example in the definition of `add1` in App.elm. -->

<hr />

## ES6

This starter includes [Babel](https://babeljs.io/) so you can directly use ES6 code.

 ## How it works

 `webpack-serve --hot --colors --port 3000`
 `webpack-serve --hot --host=0.0.0.0 --port 3000`

  - hot Enable webpack's Hot Module Replacement feature
  - inline (default) a script will be inserted in your bundle to take care of reloading, and build messages will appear in the browser console.
  - host=0.0.0.0 - enable you to reach your dev environment from another device - e.g  your phone
  - port 3000 - use port 3000 instead of default 8000
