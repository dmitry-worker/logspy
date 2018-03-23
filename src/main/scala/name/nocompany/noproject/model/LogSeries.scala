package name.nocompany.noproject.model

import java.io.PrintWriter
import java.net.InetAddress
import java.time.Instant

import name.nocompany.noproject.util.Formattable

case class LogSeries(addr:InetAddress, entries:List[LogEntry]) extends Formattable {

  def + (entry:LogEntry):LogSeries = {
    require(entry.addr == addr)
    copy(addr = this.addr, entries = entry :: this.entries)
  }

  def format(p:PrintWriter) = {
    val last = entries.head.time
    val revs = entries.reverse
    val first = revs.head.time
    implicit def toString(time:Instant):String = Moment.format.format(time)
    val contents = revs.map(r => (s"${r.user}:${r.time:String}")).mkString(",")
    p.println(s""""${addr.getHostAddress}","${first:String}","${last:String}","${contents}"""")
  }

}
