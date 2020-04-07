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
    var loggingTest = LoggingTestKit.empty.withLogLevel(Level.INFO)
    ClientCommands.helpers.foreach { helper =>
      loggingTest = loggingTest.withMessageContains(helper)
    }
    loggingTest.expect { spawn(Client(address)) }
  }
}
