package it.scalachess.util

import java.net.NetworkInterface
import scala.collection.JavaConverters._

object NetworkUtils {
  def privateIPAddress: String =
    NetworkInterface.getNetworkInterfaces.asScala
      .flatMap(_.getInetAddresses.asScala)
      .map(_.getHostAddress)
      .find(_.startsWith("192.168."))
      .getOrElse("127.0.0.1")
}
