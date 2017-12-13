console.log(process.versions);
const {app, BrowserWindow} = require('electron')
console.log(process.platform);
console.log(__dirname);

var ffi = require('ffi');

var funcPtr = ffi.Function('void', ['int']);

var mylib = ffi.Library('MyDll', {
  'twice': ['int', ['int']],
  'doSomething': ['int', [funcPtr, 'int']]
});

var onResult = function(resultVal) {
  console.log('Result is', resultVal);
};

console.log('twice 10 =', mylib.twice(10));
mylib.doSomething(onResult, 10);

/*
var myExtension = require('./build/Release/my_extension');
console.log(myExtension);
console.log(myExtension.hello());
*/

const https = require('https');
const { URL } = require('url');
const options = new URL('https://raw.githubusercontent.com/cyginst/cyginst-v1/master/cyginst.bat');
var img;
const req = https.request(options, (res) => {
  console.log('statusCode:', res.statusCode);
  console.log('headers:', res.headers);

  res.setEncoding('binary');
  var data = [];

  res.on('data', (d) => {
    //process.stdout.write(d);
    data.push( new Buffer( d, 'binary' ) );
  });
  res.on('end', function () {
    console.log('[end]');
    img = Buffer.concat( data );
    process.stdout.write(img);
    app.quit()
  });
});
req.on('error', (e) => {
  console.error(e);
  app.quit()
});
req.end();

