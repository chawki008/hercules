'use strict';

// Require index.html so it gets copied to dist
require('./index.html');
var storedUser = localStorage.getItem('user');
var startingState = storedUser ? JSON.parse(storedUser) : null;

var app = require('./Main.elm').Main.fullscreen({
  backendURL: process.env.BACKEND_URL,
  user: startingState
});

app.ports.setStorage.subscribe(function (user) {
  localStorage.setItem('user', JSON.stringify(user));
});

app.ports.removeStorage.subscribe(function () {
  localStorage.removeItem('user');
});

app.ports.title.subscribe(function(title) {
  document.title = "Hercules - " + title;
});
