import { Elm } from "../elm/Main.elm";
import { faker } from "@faker-js/faker";
import "bootstrap/dist/js/bootstrap.bundle.js";

const storedState = localStorage.getItem("elm-todo");
const startingState = storedState ? JSON.parse(storedState) : null;
const app = Elm.Main.init({ flags: startingState, node: document.getElementById("app") });


const ws = new WebSocket("wss://ws.postman-echo.com/raw");

ws.onopen = function (event) {
    console.log("WebSocket is open now.");
    // Mock messages from server
    setInterval(() => {
        ws.send(JSON.stringify({
            content: faker.lorem.paragraph(),
            fromRemote: true,
        }));
    }, 5000);
};

ws.onmessage = function (event) {
    app.ports.messageReceiver.send(event.data);
    console.log("WebSocket message received:", event);
};

ws.onerror = function (event) {
    console.error("WebSocket error observed:", event);
};

app.ports.setStorage.subscribe(function (state) {
    localStorage.setItem("elm-todo", JSON.stringify(state));
});

app.ports.logError.subscribe(function (error) {
    console.error("Error from Elm:", error);
});


app.ports.sendMessage.subscribe(function (message) {
    ws.send(JSON.stringify({
        content: message,
        fromRemote: false,
    }));
});
