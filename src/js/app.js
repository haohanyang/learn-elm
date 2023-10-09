import { Elm } from "../elm/Main.elm";

import 'bootstrap/dist/js/bootstrap.bundle.js';
import 'bootstrap/dist/css/bootstrap.css';
import 'bootstrap-icons/font/bootstrap-icons.css';

const storedState = localStorage.getItem("elm-todo");
const startingState = storedState ? JSON.parse(storedState) : null;
const app = Elm.Main.init({ flags: startingState });

app.ports.setStorage.subscribe(function (state) {
    localStorage.setItem("elm-todo", JSON.stringify(state));
});



