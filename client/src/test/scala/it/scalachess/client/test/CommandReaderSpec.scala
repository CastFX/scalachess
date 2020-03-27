package it.scalachess.client.test

import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class CommandReaderSpec extends FlatSpec with Matchers with OptionValues with Inspectors {

  "Commands" should "be parsed correctly" in {
    val commandRegex = "/([a-zA-Z]+)$".r

    assert(commandRegex.pattern.matcher("/start").matches())
  }
}
