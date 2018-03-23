package name.nocompany.noproject

import java.io.{File, PrintWriter}
import java.time.Duration

import name.nocompany.noproject.model.{LogContainer, LogEntry}

import scala.util.Try

object Application extends App {

  if (args.size != 2) {
    val msg = """Usage: logspy <input.name> <output.name> accepts `csv` with structure  "user","ip","datetime". """
    sys.error(msg)
  }

  implicit val window = Duration.ofHours(1)

  val source = io.Source.fromFile(args(0)).getLines().map(LogEntry.unapply).flatten
  val empty = LogContainer()

  val container = (empty /: source) (_ + _)
  val result = container.complete

  var p:Option[PrintWriter] = None
  val target = Try {
    val f = new File(args(1))
    val w = new PrintWriter(f)
    p = Some(w)
    result.format(w)
  }
  p.foreach(_.close)

}


