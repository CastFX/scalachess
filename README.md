# ScalaChess - pps1819
A distributed, heterogeneous system to host chess games.


Sviluppo di un sistema distribuito ed eterogeneo per la gestione di partite di scacchi.
Il processo di sviluppo è stato caratterizzato dall’uso del paradigma funzionale e dall’utilizzo di un approccio agile di tipo Scrum.

**Local Client**
To run the local client execute:

`scala local_client.jar` //Permette di giocare contro un AI o contro un altro player sulla stessa CLI.

**Server**
It's possibile to define IP and port, otherwise it will run on the local IP of the machine on a predefined port.

`scala server.jar [ip:port]` //ip:porta definiti.

**Remote Client**
To launch the remote client, it's possibile to specify the IP and port of the server. Otherwise, the client will try to connect to its local IP on the predefined port.

`scala server.jar [ip:port]` //ip:porta definiti.

Everything can be also executed with java, just need to replace `scala` with `java -jar`.

[![Build Status](https://travis-ci.com/CastFX/PPS-18-SC.svg?branch=master)](https://travis-ci.com/CastFX/PPS-18-SC)
