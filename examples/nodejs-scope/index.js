const express = require('express');
const bodyParser = require('body-parser');
const edn = require('jsedn');

const app = express();
app.use(bodyParser.raw({type: 'application/edn'}));

app.post('/write-dependencies', function (req, res) {
  const deps = edn.toJS(edn.parse(req.body.toString('utf8')));
  console.log(deps);
  res.status(200).end();
});

app.listen(8888, function () {
  console.log('Nodejs scope listening on 8888');
})
