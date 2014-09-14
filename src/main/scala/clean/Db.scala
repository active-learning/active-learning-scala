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

import java.io.{File, FileInputStream}
import java.sql.{Connection, DriverManager}

import org.sqlite.SQLiteConnection

/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db.
 * Uma vez aberta, a conexao aceita consultas simultaneas de leitura
 * que sao resolvidas pelo SQLite.
 * A escrita depende do mutex aqui implementado, mas
 * pode ser resolvida pelo SQLite via tentativas durante BUSY_WAITING.
 */
class Db(val database: String, debug: Boolean = true) extends Log {
  override lazy val toString = database
  private var available = true
  private var connection: Connection = null

  def open() {
    if (!fileExists(database)) error(s" $database not found!")
    try {
      val url = "jdbc:sqlite:////" + database
      connection = DriverManager.getConnection(url)
      connection.asInstanceOf[SQLiteConnection].setBusyTimeout(20 * 60 * 1000) //20min. timeout
      if (debug) log(s"Connection to $database opened.")()
    } catch {
      case e: Throwable => e.printStackTrace()
        log(e.getMessage)()
        error(s"Problems opening db connection: $database !")
    }
  }

  def acquire() = {
    synchronized {
      while (!available) wait()
      available = false
    }
  }

  def release() = {
    synchronized {
      available = true
      notify()
    }
  }

  /**
   * A more reliable test for file existence.
   * @param filePath
   * @return
   */
  def fileExists(filePath: String) = {
    val f = new File(filePath)
    try {
      val buffer = new Array[Byte](4)
      val is = new FileInputStream(f)
      if (is.read(buffer) != buffer.length) {
      }
      is.close()
      true
    } catch {
      case _: Throwable => false
    }
  }

  def error(msg: String) = {
    throw new Error(s"Error: $msg")
  }

  def justQuit(msg: String) = {
    log(s"Quiting: $msg")()
    sys.exit(1)
  }

  def quit(msg: String) = {
    close()
    justQuit(msg)
  }

  def read(sql: String) = {
    test(sql)
    if (debug) log(s"[$sql]")()
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
        val seq = 1 to numColumns map {
          i =>
            //            val s = columnsType(i) match {
            //              case java.sql.Types.BOOLEAN | java.sql.Types.DATE | java.sql.Types.TIMESTAMP | java.sql.Types.TINYINT | java.sql.Types.SMALLINT | java.sql.Types.INTEGER | java.sql.Types.BIGINT | java.sql.Types.CHAR | java.sql.Types.VARCHAR => resultSet.getString(i)
            //              case java.sql.Types.NVARCHAR => resultSet.getNString(i)
            //              case java.sql.Types.FLOAT | java.sql.Types.NUMERIC | java.sql.Types.DOUBLE => "%2.2f".format(resultSet.getDouble(i))
            //              case _ => resultSet.getString(i)
            //            }
            resultSet.getDouble(i)
        }
        queue.enqueue(seq)
      }
      resultSet.close()
      statement.close()
      queue.toList.map(_.toVector)
    } catch {
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL query '$sql' in: $database .\n" + e.getMessage)
    }
  }

  def test(sql: String) = if (connection == null || connection.isClosed) error(s"Not applying sql query [$sql]. Database $database is closed.")

  def write(sql: String) {
    test(sql)
    if (debug) log(s"[$sql]")()
    try {
      acquire()
      val statement = connection.createStatement()
      val r = statement.executeUpdate(sql)
      statement.close()
      if (debug && r > 0) log(s"statement.execute returned $r for $sql")()
    } catch {
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL query '$sql' in: $database .\n" + e.getMessage)
    } finally release()
  }

  def readBlobs(sql: String) = {
    test(sql)
    if (debug) log(s"[$sql]")()
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
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL blob query '$sql' in: $database .\n" + e.getMessage)
    } finally release()
  }

  def writeBlob(sql: String, data: Array[Byte]) {
    test(sql)
    if (debug) log(s"[$sql]")()
    try {
      acquire()
      val statement = connection.prepareStatement(sql)
      statement.setBytes(1, data)
      val r = statement.execute()
      statement.close()
      if (debug) log(s"statement.execute returned $r for $sql")()
    } catch {
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL blob query '$sql' in: $database .\n" + e.getMessage)
    } finally release()
  }

  /**
   * Several queries inside a transaction.
   * @param sql
   */
  def batchWrite(sqls: List[String]) {
    if (connection.isClosed) error(s"Not applying sql queries $sqls. Database $database is closed.")
    if (debug) sqls foreach log

    try {
      acquire()
      val statement = connection.createStatement()
      statement.execute("begin")
      val rs = sqls map statement.executeUpdate
      statement.execute("end")
      statement.close()
      if (debug && rs.exists(_ > 0)) log(s"statement.execute returned $rs for $sqls")()
    } catch {
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL queries '$sqls' in: $database .\n" + e.getMessage)
    } finally release()
  }

  def close() {
    if (debug) log(s"Connection to $database closed.")()
    connection.close()
  }
}

