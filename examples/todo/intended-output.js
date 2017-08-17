function createSession(session, username, password) {
  return "sessionToken";
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
