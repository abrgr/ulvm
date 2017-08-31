function createSession(session, username, password) {
  return Promise.resolve("sessionToken");
}

function createSessionWrapper() {
  httpReceive((httpReceive_headers, httpReceive_body, httpReceive_res) => {
    const session = httpReceive_headers["session"];
    const username = httpReceive_body["username"];
    const password = httpReceive_body["password"];

    const result = createSession(session, username, password);

    httpRespond(httpReceive_res, result);
  });

}

function httpReceive(req, res, cb) {
  cb(req.headers, req.body, res);
}

function httpReceive(req, res) {
  const session = req.headers['x-session-token'];
  const { username, password } = req.body;

  createSession(session, username, password).then((sessionResponse) => {
    res.status(200).json({sessionResponse});
  }).catch((err) => {
    if ( err instanceOf createSession.AuthErr ) {
      res.status(401).end();
    }

    res.status(500).end();
  });
}

function init() {
  const {app} = scope;
  route(app, 'post', '/sessions', httpReceive);
}

function route(app, method, path, handler) {
  app[method](path, handler);
}
