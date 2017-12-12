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

var myExtension = require('./build/Release/my_extension');
console.log(myExtension);
console.log(myExtension.hello());

const https = require('https');
const { URL } = require('url');
const options = new URL('https://www.google.com');
const req = https.request(options, (res) => {
  console.log('statusCode:', res.statusCode);
  console.log('headers:', res.headers);

  res.on('data', (d) => {
    process.stdout.write(d);
  });
});
req.on('error', (e) => {
  console.error(e);
});
req.end();