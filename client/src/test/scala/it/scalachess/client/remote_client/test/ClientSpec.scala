package it.scalachess.client.remote_client.test

import akka.actor.testkit.typed.scaladsl.{ ActorTestKit, LoggingTestKit }
import it.scalachess.client.remote_client.{ Client, ClientCommands }
import org.scalatest._
import org.slf4j.event.Level

class ClientSpec extends FlatSpec with BeforeAndAfterAll with Matchers with OptionValues with Inspectors {
  val testKit: ActorTestKit = ActorTestKit()
  import testKit._
  override def afterAll(): Unit = shutdownTestKit()

  val address: String = "127.0.0.1:25555"

  "A Client" should "show the helper when started" in {
    LoggingTestKit.empty
      .withLogLevel(Level.INFO)
      .withCustom { event =>
        ClientCommands.helpers.exists(event.message.contains)
      }
      .withOccurrences(Int.MaxValue)
      .expect { spawn(Client(address)) }
  }
}
