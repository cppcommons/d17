console.log('abc');
const {app, BrowserWindow} = require('electron')

console.log(process.platform);
console.log(__dirname);

app.quit()
/*
var mainWindows = null

app.on('ready', function() {
  mainWindow = new BrowserWindow({width: 800, height: 600})
  mainWindow.loadURL(`file://${__dirname}/index.html`)
  mainWindow.on('closed', () => {
    mainWindow = null
  })
})

app.on('window-all-closed', function () {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})
*/