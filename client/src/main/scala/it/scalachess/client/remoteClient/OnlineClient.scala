package it.scalachess.client.remoteClient

import akka.actor.typed.ActorSystem

object OnlineClientApp extends App {

  case
  def apply()

  def main(args: Array[String]): Unit = {
    val system: ActorSystem[HelloWorldMain.SayHello] =
      ActorSystem(HelloWorldMain(), "hello")

    system ! HelloWorldMain.SayHello("World")
    system ! HelloWorldMain.SayHello("Akka")
  }

  OnlineClientApp.main(Array.empty)
}
