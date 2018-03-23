package name.nocompany.noproject.model

import java.net.InetAddress
import java.time._


case class LogEntry(user: String, addr: InetAddress, time: Instant) {
  def lift = LogSeries(addr, this :: Nil)
}

object LogEntry {

  def unapply(arg: String): Option[LogEntry] = {
    arg.split(",") match {
      case Array( UserID(name), IPAddress(addr), Moment(time) ) =>
        Some(LogEntry(name, addr, time))
      case _ =>
        None
    }
  }

}







