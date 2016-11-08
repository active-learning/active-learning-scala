package novo

import clean.lib.Ds

case class ALive(dataset: String, exp: String, disabled: Boolean = false) {
  val timems = 60000
  var running = false
  lazy val ds = Ds(dataset, readOnly = false)
  lazy val id = ds.id
  lazy val thread = new Thread {
    override def run() = if (disabled) sys.error("ALive disabled!")
    else {
      while (running) {
        beat()
        var c = 0
        while (running && c < 10000) {
          Thread.sleep(timems / 10000)
          c += 1
        }
      }
    }
  }

  def putResults(res: String): Unit = if (disabled) println("ALive disabled!") else ds.write(s"replace into r values ('$exp', '$res')")

  def clear() = if (disabled) sys.error("ALive disabled!")
  else {
    ds.write(s"delete from r where s='$exp'")
    ds.write(s"delete from l where r=0 and f=0")
  }

  def getResults = if (disabled) {
    println("ALive disabled!")
    None
  } else ds.readString(s"select o from r where s='$exp'").headOption match {
    case Some(Vector(str)) => Some(str)
    case _ => None
  }

  def isFree = if (disabled) {
    println("ALive disabled!")
    true
  } else {
    if (ds.isClosed) ds.open()
    val now = ds.readTime(s"select now()").head.head
    //    val idPast = ds.readString(s"select u from l where r=0 and f=0").headOption match {
    //      case Some(Vector(res)) => res
    //      case _ => "no id"
    //    }
    ds.readTime(s"select t from l where r=0 and f=0").headOption match {
      case Some(Vector(res)) =>
        val past = toDate(res)
        val elapsedMiliSeconds = now.getTime - past.getTime
        //        idPast != id &&
        elapsedMiliSeconds > 2 * timems
      case _ => true
    }
  }

  def toDate(timestamp: java.sql.Timestamp) = if (disabled) sys.error("ALive disabled!")
  else {
    val milliseconds = timestamp.getTime + (timestamp.getNanos / 1000000)
    new java.util.Date(milliseconds)
  }

  def start() = if (disabled) println("ALive disabled!")
  else {
    if (ds.isClosed) ds.open()
    running = true
    thread.start()
  }

  def stop() = if (disabled) println("ALive disabled!")
  else {
    running = false
    Thread.sleep(timems / 5000)
    ds.write(s"delete from l where u='$id'")
    ds.close()
  }

  def beat() = if (disabled) sys.error("ALive disabled!")
  else {
    //val now = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(java.util.Calendar.getInstance().getTime)
    val query = s"replace into l values (0, 0, '$id', now())"
    ds.write(query)
  }
}