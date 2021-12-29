const App = require('./output/Main');

window.__appState = App.main(window.__appState || App.initialState)();
module.hot.accept();
