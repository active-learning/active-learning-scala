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

package app.db

import java.io.{File, FileWriter}
import java.sql.{Connection, DriverManager}

import app.ArgParser
import org.apache.commons.io.FileUtils
import util.Lock

import scala.collection.mutable

/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db, ou seja, um dataset ou o appfile.
 * Uma vez aberta, a conexao aceita consultas simultaneas
 * que sao resolvidas pelo SQLite.
 * Ao fechar a conexão com close() o arquivo temporário onde
 * ocorrem as operações é apagado.
 * Ele é copiado de volta para seu local de origem a cada chamada a save().
 * O programa é interrompido (espera-se) caso mais de
 * uma conexão seja tentada no mesmo arquivo.
 */
trait Database extends Lock {
  override lazy val toString = database
  lazy val dbOriginal = new File(path + database + ".db")
  lazy val dbLock = new File(path + "locked/" + database + ".db")
  //  lazy val dbCopy = if (!readOnly) new File("/tmp/" + database + ".db") else dbOriginal
  lazy val dbCopy = if (!readOnly) new File("/run/shm/" + database + ".db") else dbOriginal
  val path: String
  val database: String
  val createOnAbsence: Boolean
  val readOnly: Boolean
  var connection: Connection = null

  /**
   * Opens connection to database.
   * @param debug true, if the dataset had to be created (create parameter should be also true)
   */
  def open(debug: Boolean = false) = {
    if (isOpen) safeQuit(s"Database $dbOriginal already opened as $dbCopy!")
    //check file existence and if it is in use
    if (dbLock.exists()) {
      if (dbOriginal.exists()) safeQuit(s"Inconsistency: $dbOriginal and $dbLock exist at the same time!")
      else safeQuit(s"$dbOriginal is locked as $dbLock! Cannot open it.")
      //todo: maybe a locked database should be readable
    }
    if (dbCopy.exists() && !readOnly) safeQuit(dbCopy + " já existe! Talvez outro processo esteja usando " + dbOriginal + ". Entretanto, não há lock.")

    var created = false
    if (createOnAbsence) {
      if (!dbOriginal.exists()) {
        createDatabase()
        created = true
      }
    } else {
      if (!dbOriginal.exists()) safeQuit(dbOriginal + " não existe!")
    }

    //open
    try {
      if (!readOnly) {
        lockFile()
        Thread.sleep(200)
        FileUtils.copyFile(dbLock, dbCopy)
        Thread.sleep(400)
      }
      Class.forName("org.sqlite.JDBC") //todo: put forName at a global place to avoid repeated calling
      val url = "jdbc:sqlite:////" + dbCopy
      connection = DriverManager.getConnection(url)
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems opening db connection: " + dbCopy + ":")
        safeQuit(e.getMessage)
    }
    if (debug) println("Connection to " + dbCopy + " opened.")

    if (database != "app") {
      if (debug) println("Attaching App dataset...")
      val appPath = ArgParser.appPath
      try {
        val statement = connection.createStatement()
        statement.executeUpdate("attach '" + appPath + "app.db' as app")
      } catch {
        case e: Throwable => e.printStackTrace
          safeQuit("\nProblems Attaching " + appPath + s" to $dbCopy.")
      }
      if (debug) println(" Dataset " + appPath + "app.db attached!")
    }
    created
  }

  def createDatabase() = {
    if (readOnly) safeQuit("Cannot init a readOnly database!")
    val fw = new FileWriter(dbOriginal)
    fw.close()
  }

  /**
   * rename file to dataset.db.locked.
   * All this shit is needed because of SQLite relying on NFS locks.
   */
  def lockFile() {
    if (readOnly) safeQuit("readOnly databases don't accept lockFile(), and there is no reason to accept.")
    if (dbLock.exists()) safeQuit(s"$dbLock should not exist; $dbOriginal needs to take its place.")
    dbOriginal.renameTo(dbLock)
  }

  def exec(sql: String) = {
    if (!isOpen) safeQuit("Impossible to get connection to apply sql query " + sql + ". Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")

    try {
      val statement = connection.createStatement()
      if (sql.toLowerCase.startsWith("select ")) {
        val resultSet = statement.executeQuery(sql)
        val rsmd = resultSet.getMetaData
        val numColumns = rsmd.getColumnCount
        val columnsType = new Array[Int](numColumns + 1)
        columnsType(0) = 0
        1 to numColumns foreach (i => columnsType(i) = rsmd.getColumnType(i))

        val queue = mutable.Queue[Seq[Double]]()
        while (resultSet.next()) {
          val seq = 1 to numColumns map { i =>
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
        if (sql.toLowerCase.startsWith("select count(*) from ")) Left(queue.head.head.toInt)
        else {
          if (sql.toLowerCase.startsWith("select rowid from ")) Left(queue.head.head.toInt) else Right(queue)
        }
      } else {
        if (readOnly) safeQuit("readOnly databases only accept select SQL command!")

        statement.execute(sql)
        Left(0)
      }
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems executing SQL query '" + sql + "' in: " + dbCopy + ".\n" + e.getMessage)
    }
  }

  def isOpen = connection != null

  def batchWrite(results: Array[String]) {
    try {
      val statement = connection.createStatement()
      if (readOnly) safeQuit("readOnly databases does not accept batchExec!")
      acquire()
      statement.execute("begin")
      var i = 0
      val n = results.size
      while (i < n) {
        statement.execute(results(i))
        i += 1
      }
      statement.execute("end")
      save()
      release()
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems executing batch SQL query in: " + dbCopy + ".\n" + e.getMessage)
    }
  }

  /**
   * Copies file from temp to the original (locked)
   * which does not occur at close().
   */
  def save() {
    if (readOnly) safeQuit("readOnly databases don't accept save(), and there is no reason to accept.")

    Thread.sleep(100)
    //Just in case writting to db were not a blocking operation. Or something else happened to put db in inconsistent state.
    if (new File(dbCopy + "-journal").exists()) safeQuit(s"$dbCopy-journal file found! Run 'sqlite3 $dbCopy' before continuing.")

    FileUtils.copyFile(dbCopy, dbLock)
    Thread.sleep(400)
  }

  def runStr(sql: String) = {
    if (!isOpen) safeQuit("Impossible to get connection to apply sql query " + sql + ". Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")

    try {
      val statement = connection.createStatement()
      if (sql.toLowerCase.startsWith("select ")) {
        val resultSet = statement.executeQuery(sql)
        val rsmd = resultSet.getMetaData
        val numColumns = rsmd.getColumnCount
        val columnsType = new Array[Int](numColumns + 1)
        columnsType(0) = 0
        1 to numColumns foreach (i => columnsType(i) = rsmd.getColumnType(i))

        val queue = mutable.Queue[Seq[String]]()
        while (resultSet.next()) {
          val seq = 1 to numColumns map { i =>
            //            val s = columnsType(i) match {
            //              case java.sql.Types.BOOLEAN | java.sql.Types.DATE | java.sql.Types.TIMESTAMP | java.sql.Types.TINYINT | java.sql.Types.SMALLINT | java.sql.Types.INTEGER | java.sql.Types.BIGINT | java.sql.Types.CHAR | java.sql.Types.VARCHAR => resultSet.getString(i)
            //              case java.sql.Types.NVARCHAR => resultSet.getNString(i)
            //              case java.sql.Types.FLOAT | java.sql.Types.NUMERIC | java.sql.Types.DOUBLE => "%2.2f".format(resultSet.getDouble(i))
            //              case _ => resultSet.getString(i)
            //            }
            resultSet.getString(i)
          }
          queue.enqueue(seq)
        }
        if (sql.toLowerCase.startsWith("select count(*) from ")) Left(queue.head.head.toInt)
        else {
          if (sql.toLowerCase.startsWith("select rowid from ")) Left(queue.head.head.toInt) else Right(queue)
        }
      } else {
        if (readOnly) safeQuit("readOnly databases only accept select SQL command!")
        acquire()
        statement.execute(sql)
        release()
        Left(0)
      }
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems executing SQL query '" + sql + "' in: " + dbCopy + ".\n" + e.getMessage)
    }
  }

  def close() {
    Thread.sleep(100)
    //    println(" " + dbCopy + " deleted!")
    connection.close()
    Thread.sleep(100)
    connection = null
    if (!readOnly) {
      //Checks if something happened to put db in inconsistent state.
      if (new File(dbCopy + "-journal").exists()) safeQuit(s"$dbCopy-journal file found! Run 'sqlite3 $dbCopy' before continuing.")

      dbCopy.delete()
      unlockFile()
    }
  }

  /**
   * rename dataset.db.locked to original name.
   */
  def unlockFile() {
    if (readOnly) safeQuit("readOnly databases don't accept unlockFile(), and there is no reason to accept.")
    if (dbOriginal.exists()) safeQuit(s"$dbOriginal should not exist; $dbLock needs to take its place.")
    dbLock.renameTo(dbOriginal)
  }
}

