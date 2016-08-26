package novo

import clean.lib.Ds

case class ALive(dataset: String, exp: String) {
  def putResults(res: String): Unit = {
    ds.write(s"replace into r values ('$exp', '$res')")
  }

  def clear() {
    ds.write(s"delete from r where s='$exp'")
    ds.write(s"delete from l where r=0 and f=0")
  }

  def getResults = ds.readString(s"select o from r where s='$exp'").headOption match {
    case Some(Vector(str)) => Some(str)
    case _ => None
  }

  val ds = Ds(dataset, readOnly = false)
  val timems = 60000
  val id = ds.id
  var running = false
  val thread = new Thread {
    override def run() {
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

  def isFree = {
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

  def toDate(timestamp: java.sql.Timestamp) = {
    val milliseconds = timestamp.getTime + (timestamp.getNanos / 1000000)
    new java.util.Date(milliseconds)
  }

  def start() {
    if (ds.isClosed) ds.open()
    running = true
    thread.start()
  }

  def stop() {
    running = false
    Thread.sleep(timems / 5000)
    ds.write(s"delete from l where u='$id'")
    ds.close()
  }

  def beat() {
    val query = s"replace into l values (0, 0, '$id', now())"
    ds.write(query)
  }
}