# ScalaChess - pps1819
Sviluppo di un sistema distribuito ed eterogeneo per la gestione di partite di scacchi.
Il processo di sviluppo è stato caratterizzato dall’uso del paradigma funzionale e dall’utilizzo di un approccio agile di tipo Scrum.

**Client Locale**

Per avviare il client locale: 

`scala local_client.jar` //Permette di giocare contro un AI o contro un altro player sulla stessa CLI.

**Server**

Per avviare il server è possibile definire ip:porta, altrimenti sarà avviato sull'IP locale e su una porta predefinita.

`scala server.jar` //IP locale e porta predefinita 2555.

`scala server.jar ip:port` //ip:porta definiti.

**Client Remoto**

Per avviare il client è possibile definire ip:porta del server a cui connettersi, altrimenti proverà a collegarsi all'IP locale e su una porta predefinita.

`scala client.jar` //IP locale e porta predefinita 2555.

`scala server.jar ip:port` //ip:porta definiti.

L'applicativo può anche essere avviato usando Java al posto di Scala: basterà sostituire `scala` con `java -jar`.

[![Build Status](https://travis-ci.com/CastFX/PPS-18-SC.svg?branch=master)](https://travis-ci.com/CastFX/PPS-18-SC)
