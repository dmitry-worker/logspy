package name.nocompany.noproject.model

import java.net.InetAddress
import java.time.{Instant, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter

import scala.util.Try

/**
  * This is a helper trait for reading quoted csv values
  * With csv primitive extractors below.
  */
sealed trait Extractors {
  def unbox(src:String) = {
    if (src.isEmpty) src
    else if (src.head == '\"' && src.last == '\"') src.substring(1, src.length - 1)
    else src
  }
}


object UserID extends Extractors {
  def unapply(arg: String): Option[String] = {
    Some(unbox(arg))
  }
}


object IPAddress extends Extractors {
  def unapply(arg: String): Option[InetAddress] = {
    Try(InetAddress.getByName(unbox(arg))).toOption
  }
}

object Moment extends Extractors {
  private val PATTERN = "yyyy-MM-dd HH:mm:ss"
  private val zone = ZoneId.of("UTC")
  val format = DateTimeFormatter.ofPattern(PATTERN).withZone(zone)
  def unapply(arg: String): Option[Instant] = {
    val instant = Try {
      val parsed = format.parse(unbox(arg))
      val ldt = LocalDateTime.from(parsed)
      val zdt = ZonedDateTime.of(ldt, zone)
      zdt.toInstant
    }
    instant.toOption
  }
}

