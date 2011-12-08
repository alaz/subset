package com.osinka.subset

import java.util.Date
import org.joda.time.DateTime

// TODO: move to "subset-joda" project (?)
object JodaValues extends JodaValues
trait JodaValues {
  implicit val jodaDateTimeReader = ValueReader[DateTime]({
      case d: Date => new DateTime(d)
      case l: Long => new DateTime(l)
      case i: Int => new DateTime(i*1000L)
    })

  implicit val jodaDateTimeWriter = ValueWriter[DateTime](_.toDate)

  // TODO: Duration and/or Period reader and writer
}
