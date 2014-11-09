/*
 active-learning-scala: Active Learning library for Scala
 Copyright (c) 2014 Davi Pereira dos Santos

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package clean

import java.sql.{Timestamp, Connection, DriverManager, Statement}
import java.util.{UUID, Calendar}


/**
 * Cada instancia desta classe representa uma conexao.
 */
class Db(val database: String) extends Log with Lock {
  override lazy val toString = database
  private var connection: Connection = null
  val context = database
  val connectionWait_ms = 30000
  var alive = false
  val id = System.currentTimeMillis() + UUID.randomUUID().toString

  def open() {
    try {
      val url = s"jdbc:mysql://127.0.0.1:${Global.mysqlPort}/" + database
      //      val url = "jdbc:sqlite:////" + database
      //      connection = DriverManager.getConnection(url)
      connection = DriverManager.getConnection(url, "davi", Global.mysqlPass)
      //      connection.asInstanceOf[SQLiteConnection].setBusyTimeout(20 * 60 * 1000) //20min. timeout
      log(s"Connection to $database opened.")

      alive = true
      new Thread(new Runnable() {
        def run() {
          while (Global.running && alive) {
            //20s
            1 to 500 takeWhile { _ =>
              Thread.sleep(40)
              Global.running && alive
            }
            heartbeat()
          }
        }
      }).start()

      log("created alive beeper")
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"Problems opening db connection: ${e.getMessage} ! Trying again in 30s...", 0)
        Thread.sleep(connectionWait_ms)
        if (connection == null || connection.isClosed) {
          log("Reopening database...")
          open()
        }
    }
  }

  def heartbeat() {
    val now = Calendar.getInstance().getTime
    write(s"update t set v=`$now`, uuid=`$id`")
  }

  def toDate(timestamp: java.sql.Timestamp) = {
    val milliseconds = timestamp.getTime + (timestamp.getNanos / 1000000)
    new java.util.Date(milliseconds)
  }

  def isAliveByOtherJob(lifetimeSeconds: Double = 120): Boolean = {
    val now = Calendar.getInstance().getTime
    val sql = "select * from t"
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(sql)
      val rsmd = resultSet.getMetaData
      val numColumns = rsmd.getColumnCount
      val columnsType = new Array[Int](numColumns + 1)
      columnsType(0) = 0
      1 to numColumns foreach (i => columnsType(i) = rsmd.getColumnType(i))
      val queue = collection.mutable.Queue[(Timestamp, String)]()
      while (resultSet.next()) {
        val tup = resultSet.getTimestamp(1) -> resultSet.getString(2)
        queue.enqueue(tup)
      }
      resultSet.close()
      statement.close()
      val past = toDate(queue.head._1)
      val idPast = queue.head._2
      val elapsedSeconds = (now.getTime - past.getTime) / 1000d
      idPast != id && elapsedSeconds < lifetimeSeconds
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems executing SQL query '$sql': ${e.getMessage} .\nTrying againg in 60s.\n", 0)
        Thread.sleep(60000) //waiting time is longer than normal to allow for other alive connections to update the table
        if (connection.isClosed) {
          log("Reopening database...")
          open()
        }
        isAliveByOtherJob(lifetimeSeconds + 60) //each time we recover, the elapsed time should be higher
    }
  }

  override def error(msg: String) = {
    if (connection != null && !connection.isClosed) close()
    super.error(database + ": " + msg)
  }

  def quit(msg: String) = {
    close()
    justQuit(msg)
  }

  def read(sql: String): List[Vector[Double]] = {
    test(sql)
    log(s"[$sql]", 10)
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(sql)
      val rsmd = resultSet.getMetaData
      val numColumns = rsmd.getColumnCount
      val columnsType = new Array[Int](numColumns + 1)
      columnsType(0) = 0
      1 to numColumns foreach (i => columnsType(i) = rsmd.getColumnType(i))
      val queue = collection.mutable.Queue[Seq[Double]]()
      while (resultSet.next()) {
        val seq = 1 to numColumns map { i => resultSet.getDouble(i)}
        queue.enqueue(seq)
      }
      resultSet.close()
      statement.close()
      queue.toList.map(_.toVector)
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems executing SQL query '$sql': ${e.getMessage} .\nTrying againg in 30s.\n", 0)
        Thread.sleep(connectionWait_ms)
        if (connection.isClosed) {
          log("Reopening database...")
          open()
        }
        read(sql)
    }
  }

  def test(sql: String) = if (connection == null || connection.isClosed) error(s"Not applying sql query [$sql]. Database $database is closed or null.")

  def write(sql: String) {
    test(sql)
    log(s"[$sql]", 10)
    try {
      acquire()
      val statement = connection.createStatement()
      statement.executeUpdate(sql)
      statement.close()
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems executing SQL query '$sql' in: ${e.getMessage} .\nTrying againg in 30s", 0)
        release()
        Thread.sleep(connectionWait_ms)
        if (connection.isClosed) {
          log("Reopening database...")
          open()
        }
        write(sql)
    } finally release()
  }

  def readBlobs(sql: String): List[(Array[Byte], Int)] = {
    test(sql)
    log(s"[$sql]", 10)
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(sql)
      val queue = collection.mutable.Queue[(Array[Byte], Int)]()
      while (resultSet.next()) {
        val bytes = resultSet.getBytes(1)
        queue.enqueue(bytes -> resultSet.getInt(2))
      }
      resultSet.close()
      statement.close()
      queue.toList
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems executing SQL read blobs query '$sql': ${e.getMessage} .\nTrying againg in 30s.\n", 0)
        Thread.sleep(connectionWait_ms)
        if (connection.isClosed) {
          log("Reopening database...")
          open()
        }
        readBlobs(sql)
    }
  }

  def readBlobs4(sql: String): List[(Array[Byte], Int, Int, Int)] = {
    test(sql)
    log(s"[$sql]", 10)
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(sql)
      val queue = collection.mutable.Queue[(Array[Byte], Int, Int, Int)]()
      while (resultSet.next()) {
        val bytes = resultSet.getBytes(1)
        queue.enqueue((bytes, resultSet.getInt(2), resultSet.getInt(3), resultSet.getInt(4)))
      }
      resultSet.close()
      statement.close()
      queue.toList
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems executing read blobs4 SQL query '$sql' in: ${e.getMessage} .\nTrying againg in 30s.\n", 0)
        Thread.sleep(connectionWait_ms)
        if (connection.isClosed) {
          log("Reopening database...")
          open()
        }
        readBlobs4(sql)
    }
  }

  def writeBlob(sql: String, data: Array[Byte]) {
    test(sql)
    log(s"[$sql]", 10)
    try {
      acquire()
      val statement = connection.prepareStatement(sql)
      statement.setBytes(1, data)
      statement.execute()
      statement.close()
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems executing SQL blob query '$sql' in: ${e.getMessage} .\nTrying againg in 30s.\n", 0)
        release()
        Thread.sleep(connectionWait_ms)
        if (connection.isClosed) {
          log("Reopening database...")
          open()
        }
        writeBlob(sql, data)
    } finally release()
  }

  /**
   * Several blob writings inside a transaction.
   * @param sqls
   */
  def batchWriteBlob(sqls: List[String], blobs: List[Array[Byte]]) {
    if (connection.isClosed) error(s"Not applying sql queries $sqls. Database $database is closed.")
    log("batch write blob ...", 10)
    log(sqls.mkString("\n"), 10)
    var stats: List[Statement] = null
    try {
      acquire()
      connection.setAutoCommit(false)
      stats = sqls.zip(blobs) map { case (sql, blob) =>
        val statement = connection.prepareStatement(sql)
        if (blob != null) statement.setBytes(1, blob)
        statement.execute()
        statement
      }
      connection.commit()
      stats foreach (_.close())
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems writing blobs with SQL query '$sqls' in: ${e.getMessage} .\nTrying againg in 30s\n", 0)
        if (connection != null) {
          try {
            System.err.print("Transaction is being rolled back")
            connection.rollback()
            connection.setAutoCommit(true)
          } catch {
            case e2: Throwable => log(s"\nProblems 'rolling back'/'setting auto commit' SQL queries '$sqls': ${e2.getMessage} .\n" +
              s"Probably it wasn't needed anyway.", 0)
          }
          release()
          Thread.sleep(connectionWait_ms)
          if (connection.isClosed) {
            log("Reopening database...")
            open()
          }
        }
        batchWriteBlob(sqls, blobs)
    } finally {
      //      if (stats != null && stats.forall(_ != null)) stats foreach (_.close())
      connection.setAutoCommit(true)
      release()
    }
    log("batch write blob finished.", 10)
  }

  /**
   * Several queries inside a transaction.
   * @param sqls
   */
  def batchWrite(sqls: List[String]) {
    if (connection.isClosed) error(s"Not applying sql queries $sqls. Database $database is closed.")
    sqls foreach (m => log(m, 10))
    var statement: Statement = null
    try {
      acquire()
      connection.setAutoCommit(false)
      statement = connection.createStatement()
      sqls foreach statement.executeUpdate
      connection.commit()
      statement.close()
    } catch {
      case e: Throwable => //e.printStackTrace()
        log(s"\nProblems writing blobs with SQL query '$sqls': ${e.getMessage} .\nTrying againg in 30s\n", 0)
        if (connection != null) {
          try {
            log("Transaction is being rolled back...", 0)
            connection.rollback()
            connection.setAutoCommit(true)
          } catch {
            case e2: Throwable => log(s"\nProblems 'rolling back'/'setting auto commit' SQL queries '$sqls': ${e2.getMessage}.\n" +
              s"Probably it wasn't needed anyway.", 0)
          }
          release()
          Thread.sleep(connectionWait_ms)
          if (connection.isClosed) {
            log("Reopening database...")
            open()
          }
        }
        log("Recursive call...", 0)
        batchWrite(sqls)
        println(s"depois")
    } finally {
      if (statement != null) statement.close()
      connection.setAutoCommit(true)
      release()
    }
  }

  def close() {
    alive = false
    log(s"Connection to $database closed.")
    if (!connection.isClosed) connection.close()
  }
}

