package name.nocompany.noproject.model

import java.io.PrintWriter
import java.net.InetAddress
import java.time.Duration

import name.nocompany.noproject.util.{Formattable, LogParseFailure}

/**
  * LogContainer is immutable structure that is used
  * to aggregate information about user access entries
  *   see +(e:LogEntry) method to get a new one with entry e.
  *   see complete() method to get clean copy without singular entries
  * @param impl   - actual container for the entries
  * @param window - time window for series extraction
  */

case class LogContainer(impl:Map[InetAddress, List[LogSeries]] = Map())(implicit window:Duration) {

  private class CompleteLogContainer(res:Map[InetAddress, List[LogSeries]]) extends Formattable {
    def format(p:PrintWriter) = { res.values.flatten.foreach(_.format(p)) }
  }

  def + (e:LogEntry):LogContainer = {
    val newMap = impl.get(e.addr) match {
      case Some(series) =>
        val shead     = series.head
        val lonely    = shead.entries.tail.isEmpty
        val lastTime  = shead.entries.head.time
        if (e.time.isBefore(lastTime)) {
          // can't go unsorted this way
          throw new LogParseFailure("Unsorted log entries!")
        } else if (e.time.minus(window).isAfter(lastTime)) {
          // if lonely we don't need and overwrite it
          val tail = if (lonely) series.tail else series
          impl + (e.addr -> (e.lift :: tail))
        } else {
          // append to the head of last entry
          val newEntries = e :: shead.entries
          val newSeries  = LogSeries(e.addr, newEntries) :: series.tail
          impl + (e.addr -> newSeries)
        }
      case _ =>
        impl + (e.addr -> (e.lift :: Nil))
    }
    LogContainer(newMap)
  }

  def complete:Formattable = {
    val compact = impl.foldLeft(Map[InetAddress, List[LogSeries]]()) { case (res, (addr, lseries)) =>
      val newSeries = lseries.filter(_.entries.size > 1)
      if (newSeries.isEmpty) res else res + (addr -> newSeries)
    }
    new CompleteLogContainer(compact)
  }

  /*
   * @Deprecated: expensive, debug only
   */
  @deprecated def size = {
    (0 /: impl){ (r, el) => r + el._2.flatMap(_.entries).size }
  }

}
