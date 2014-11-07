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

import java.sql.{Statement, SQLException, Connection, DriverManager}


/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db.
 * Uma vez aberta, a conexao aceita consultas simultaneas de leitura
 * que sao resolvidas pelo SQLite.
 * A escrita depende do mutex aqui implementado, mas
 * pode ser resolvida pelo SQLite via tentativas durante BUSY_WAITING.
 */
class Db(val database: String) extends Log with Lock {
  override lazy val toString = database
  private var connection: Connection = null
  val context = database

  def open() {
    //    if (!fileExists(database)) error(s" $database not found!")
    try {
      val url = s"jdbc:mysql://127.0.0.1:${Global.mysqlPort}/" + database
      //      val url = "jdbc:sqlite:////" + database
      //      connection = DriverManager.getConnection(url)
      connection = DriverManager.getConnection(url, "davi", Global.mysqlPass)
      //      connection.asInstanceOf[SQLiteConnection].setBusyTimeout(20 * 60 * 1000) //20min. timeout
      log(s"Connection to $database opened.")
    } catch {
      case e: Throwable => e.printStackTrace()
        log(e.getMessage)
        error(s"Problems opening db connection: $database !")
    }
  }

  //  /**
  //   * A more reliable test for file existence.
  //   * @param filePath
  //   * @return
  //   */
  //  def fileExists(filePath: String) = {
  //    val f = new File(filePath)
  //    try {
  //      val buffer = new Array[Byte](4)
  //      val is = new FileInputStream(f)
  //      if (is.read(buffer) != buffer.length) {
  //      }
  //      is.close()
  //      true
  //    } catch {
  //      case _: Throwable => false
  //    }
  //  }

  override def error(msg: String) = {
    if (connection != null && !connection.isClosed) close()
    super.error(database + ": " + msg)
  }

  def quit(msg: String) = {
    close()
    justQuit(msg)
  }

  def read(sql: String) = {
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
    log(s"[$sql]", 10)
    try {
      acquire()
      val statement = connection.createStatement()
      statement.executeUpdate(sql)
      statement.close()
    } catch {
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL query '$sql' in: $database .\n" + e.getMessage)
    } finally release()
  }

  def readBlobs(sql: String) = {
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
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL blob query '$sql' in: $database .\n" + e.getMessage)
    } finally release()
  }

  def readBlobs4(sql: String) = {
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
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL blob query '$sql' in: $database .\n" + e.getMessage)
    } finally release()
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
      case e: Throwable => e.printStackTrace()
        error(s"\nProblems executing SQL blob query '$sql' in: $database .\n" + e.getMessage)
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
    } catch {
      case e: Throwable => e.printStackTrace()
        if (connection != null) {
          try {
            System.err.print("Transaction is being rolled back")
            connection.rollback()
            error(s"\nProblems executing SQL queries '$sqls' in: $database .\n" + e.getMessage)
          } catch {
            case e2: Throwable => error(s"\nProblems rolling back SQL queries '$sqls' in: $database .\n" + e.getMessage)
          }
        }
    } finally {
      if (stats != null && stats.forall(_ != null)) stats foreach (_.close())
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
    } catch {
      case e: Throwable => e.printStackTrace()
        if (connection != null) {
          try {
            System.err.print("Transaction is being rolled back")
            connection.rollback()
            error(s"\nProblems executing SQL queries '$sqls' in: $database .\n" + e.getMessage)
          } catch {
            case e2: Throwable => error(s"\nProblems rolling back SQL queries '$sqls' in: $database .\n" + e.getMessage)
          }
        }
    } finally {
      if (statement != null) statement.close()
      connection.setAutoCommit(true)
      release()
    }
  }

  def close() {
    log(s"Connection to $database closed.")
    if (!connection.isClosed) connection.close()
  }
}

