# Developing Flows

[Flows](flows.html) is handled on the [Heroku](https://www.heroku.com/) platform.

## Getting started

First steps to get started.

- Create an account on [Heroku](https://www.heroku.com/).
- Install the [Heroku utilities](https://toolbelt.heroku.com/).
- Ask a Liquidsoap administrator to give you access to the repositories.

The repositories of the main components are organized as follows.

- `savonet-flows` is the python handler to submit metadata:
  - the associated [github repository](https://github.com/savonet/flows-submit)
  - the Heroku repository is `git@heroku.com:savonet-liquidsoap.git`
- `savonet-flows-socket` is the node application to serve the webpage and client stuff.
  - the associated [github repository](https://github.com/savonet/flows-push)
  - the [test Heroku webpage](http://savonet-flows-socket.herokuapp.com/) is updated by pushing on `git@heroku.com:savonet-flows-socket-next.git`
  - the [prod Heroku webpage](http://savonet-flows-socket.herokuapp.com/) is updated by pushing on `git@heroku.com:savonet-flows-socket.git`

Some more experimental repositories include:

- a [command-line client](https://github.com/savonet/flows-client)

## Useful commands

Getting the environment variables:

```
heroku config -s --app savonet-flows
```

Seeing the logs of the socket application:

```
heroku logs -t --app savonet-flows-socket
```
