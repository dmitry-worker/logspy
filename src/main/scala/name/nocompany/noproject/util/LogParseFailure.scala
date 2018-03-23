package name.nocompany.noproject.util

import scala.util.control.NoStackTrace

class LogParseFailure(reason:String) extends RuntimeException(reason) with NoStackTrace
