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

package app.db.entities

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
  lazy val dbCopy = if (!readOnly) new File(folderToCopyDb + database + ".db") else dbOriginal
  //  lazy val dbCopy = if (!readOnly) new File("/run/shm/" + database + ".db") else dbOriginal
  val path: String
  val database: String
  val createOnAbsence: Boolean
  var connection: Connection = null
  var fileLocked = false // to avoid relying only in the file (which other process can have locked)

  /**
   * Opens connection to database.
   * @param debug true, if the dataset had to be created (create parameter should be also true)
   */
  def open(): Boolean = {
    //random waiting to avoid simultaneous opening
    if (!readOnly) Thread.sleep((rnd.nextDouble() * 300).toInt)

    if (isOpen()) justQuit(s"Database $dbOriginal already opened as $dbCopy!")
    //check file existence and if it is in use
    if (isLocked()) {
      if (checkExistsForNFS(dbOriginal)) justQuit(s"Inconsistency: $dbOriginal and $dbLock exist at the same time!")
      else println(s"$dbOriginal is locked as $dbLock! Cannot open it. Ignoring open request...")
      false
    } else {
      var created = false
      if (checkExistsForNFS(dbCopy) && !readOnly) justQuit(dbCopy + " já existe! Talvez outro processo esteja usando " + dbOriginal + ". Entretanto, não há lock.")

      if (createOnAbsence) {
        if (!checkExistsForNFS(dbOriginal)) {
          createDatabase()
          created = true
        }
      } else {
        if (!checkExistsForNFS(dbOriginal)) justQuit(dbOriginal + " não existe!")
      }

      //open
      if (isLocked(0)) {
        //recheck due to delays above
        if (checkExistsForNFS(dbOriginal, 0)) justQuit(s"Inconsistency: $dbOriginal and $dbLock exist at the same time!")
        else println(s"$dbOriginal is locked as $dbLock! Cannot open it. Ignoring open request...")
        false
      } else {
        try {
          if (!readOnly) {
            if (!lockFile()) {
              println("Could not open due to problems with moving dataset file.")
              return false
            }
            Thread.sleep(10)
            if (!FileUtils.contentEquals(dbLock, dbCopy)) {
              if (debug) println(s"copiando $dbLock (${dbLock.length()}) para $dbCopy (${dbCopy.length()})")
              FileUtils.copyFile(dbLock, dbCopy)
              if (debug) println(s"$dbLock para $dbCopy copiado!")
              Thread.sleep(100)
            }
          }
          Class.forName("org.sqlite.JDBC") //todo: put forName at a global place to avoid repeated calling
          val url = "jdbc:sqlite:////" + dbCopy
          connection = DriverManager.getConnection(url)
        } catch {
          case e: Throwable => e.printStackTrace
            println("\nProblems opening db connection: " + dbCopy + " :")
            println(e.getMessage)
            unsafeQuit("\nProblems opening db connection: " + dbCopy + " .")
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
              unsafeQuit("\nProblems Attaching " + appPath + s" to $dbCopy.")
          }
          if (debug) println(" Dataset " + appPath + "app.db attached!")
        }
        created
      }
    }
  }

  def createDatabase() = {
    if (readOnly) justQuit("Cannot init a readOnly database!")
    val fw = new FileWriter(dbOriginal)
    fw.close()
  }

  /**
   * rename file to dataset.db.locked.
   * All this shit is needed because of SQLite relying on NFS locks.
   */
  def lockFile() = {
    if (!new File(path + "locked/").exists()) justQuit(s"$path/locked/ does not exist.")
    if (readOnly) justQuit("readOnly databases don't accept lockFile(), and there is no reason to accept.")
    if (fileLocked || isLocked(0)) justQuit(s"$dbLock should not exist; $dbOriginal needs to take its place.")
    fileLocked = true
    if (debug) println(s"Renaming $dbOriginal to $dbLock")
    dbOriginal.renameTo(dbLock)
  }

  def isLocked(delay: Int = 300) = checkExistsForNFS(dbLock, delay)

  def exec(sql: String) = {
    if (debug) println(s"[$sql]")
    if (!isOpen) {
      justQuit("Not applying sql query " + sql + ". Database is closed.")
    }

    try {
      val statement = connection.createStatement()
      if (sql.toLowerCase.startsWith("select ") || sql.toLowerCase.startsWith("pragma ")) {
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
        //        println(s"queue: $queue   sql: $sql")
        Some(queue)
      } else {
        if (readOnly) justQuit("readOnly databases only accept select and pragma SQL commands!")
        statement.execute(sql)
        None
      }
    } catch {
      case e: Throwable => e.printStackTrace
        safeQuit("\nProblems executing SQL query '" + sql + "' in: " + dbCopy + ".\n" + e.getMessage)
    }
  }

  def batchWrite(results: Array[String]) {
    try {
      val statement = connection.createStatement()
      if (readOnly) justQuit("readOnly databases does not accept batchExec!")
      acquireOp()
      statement.execute("begin")
      var i = 0
      val n = results.size
      while (i < n) {
        statement.execute(results(i))
        i += 1
      }
      statement.execute("end")
      weakSave()
      //      save()
      releaseOp()
    } catch {
      case e: Throwable => e.printStackTrace
        releaseOp()
        safeQuit("\nProblems executing batch SQL query in: " + dbCopy + ".\n" + e.getMessage)
    }
  }

  /**
   * Copies file from temp to the original (locked)
   * which does not occur at close().
   * It verifies if the last save() was too recent to avoid overloading NFS.
   * It assumes the locking mechanism is properly used.
   */
  var lastSave = System.currentTimeMillis()

  def weakSave() {
    val now = System.currentTimeMillis()
    if (now > lastSave + 60000) {
      lastSave = now
      save()
    }
  }

  def save() {
    if (readOnly) justQuit("readOnly databases don't accept save(), and there is no reason to accept.")

    //Just in case writting to db were not a blocking operation. Or something else happened to put db in inconsistent state.
    Thread.sleep(100)

    if (checkExistsForNFS(new File(dbCopy + "-journal"))) safeQuit(s"save: $dbCopy-journal file found! Run 'sqlite3 $dbCopy' before continuing.")

    if (!FileUtils.contentEquals(dbCopy, dbLock)) {
      println("Backing up tmpFile...")
      //      println(s"copiando $dbCopy (${dbCopy.length()}) para $dbLock (${dbLock.length()})")
      FileUtils.copyFile(dbCopy, dbLock)
      println("Backing up tmpFile ok!")
      //      println(s"$dbCopy para $dbLock copiado!")
      Thread.sleep(100)
    }
  }

  def runStr(sql: String) = {
    if (!isOpen) justQuit("Not applying sql query " + sql + ". Database is closed.")

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
        Some(queue)
      } else {
        if (readOnly) justQuit("readOnly databases only accept select SQL command!")
        incCounter()
        acquireOp()
        statement.execute(sql)
        releaseOp()
        None
      }
    } catch {
      case e: Throwable => e.printStackTrace
        releaseOp()
        safeQuit("\nProblems executing SQL query '" + sql + "' in: " + dbCopy + ".\n" + e.getMessage)
    }
  }

  def isOpen() = connection != null

  def close() = if (isOpen()) {
    Thread.sleep(100)
    connection.close()
    connection = null
    if (!readOnly) {

      if (debug) println("Checking if something happened to put db in inconsistent state...")
      if (checkExistsForNFS(new File(dbCopy + "-journal"))) justQuit(s"close: $dbCopy-journal file found! Run 'sqlite3 $dbCopy' before continuing.")

      if (debug) println("Deleting dbCopy...")
      if (fileLocked) dbCopy.delete()

      if (debug) println("Unlocking...")
      unlockFile()
    }
  }

  /**
   * rename dataset.db.locked to original name.
   */
  def unlockFile() {
    if (readOnly) justQuit("readOnly databases don't accept unlockFile(), and there is no reason to accept.")
    if (checkExistsForNFS(dbOriginal)) justQuit(s"$dbOriginal should not exist; $dbLock needs to take its place.")
    if (!fileLocked) justQuit(s"Trying to unlock $dbLock, but this connection is not responsible for that lock.")
    else {
      if (debug) println(s"Renaming $dbLock to $dbOriginal")
      dbLock.renameTo(dbOriginal)
      fileLocked = false
    }
  }
}

