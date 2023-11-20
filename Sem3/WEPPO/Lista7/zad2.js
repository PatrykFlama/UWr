const https = require('https');
const fs = require('fs');

// const options = {    //! nie działa dla node v18.14 LTS, trzeba się cofnąć do v16.19 LTS
//   pfx: fs.readFileSync('certificate.pfx'),
//   passphrase: ''    // for the certificate
// };

const options = {
  key: fs.readFileSync('key.pem'),
  cert: fs.readFileSync('certificate.pem')
};


https.createServer(options, (req, res) => {
  res.end('Hello world!\n');
}).listen(443);   // standard https port

