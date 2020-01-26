'use strict';

require("./styles.scss");
// TODO https://www.nodefornerds.com/understanding-the-browser-document-elm-application-pt-1/

const {Elm} = require('./Main.elm');
Elm['Main'].init({node: document.getElementById("elm"), flags: 1});

// app.toJs.subscribe(data => console.log(data));

// Use ES2015 syntax and let Babel compile it for you
// var testFn = (inp) => {
//     let a = inp + 1;
//     return a;
// }
