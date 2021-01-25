# ScalaChess - pps1819

A distributed, heterogeneous system to host chess games.
Developed with functional programming in a Scrum environment.

**Local Client**
To run the local client execute:

`scala local_client.jar`

It allows to play against an AI or against another player on the same prompt.

**Server**

It's possibile to define IP and port, otherwise it will run on the local IP of the machine on a predefined port.

`scala server.jar [ip:port]`

**Remote Client**

To launch the remote client, it's possibile to specify the IP and port of the server. Otherwise, the client will try to connect to its local IP on the predefined port.

`scala server.jar [ip:port]`

Everything can be also executed with java, just need to replace `scala` with `java -jar`.

[![Build Status](https://travis-ci.com/CastFX/PPS-18-SC.svg?branch=master)](https://travis-ci.com/CastFX/PPS-18-SC)
