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

import java.io.{FileWriter, File}
import java.sql.{Connection, DriverManager}

import al.strategies.{RandomSampling, Strategy}
import app.ArgParser
import org.apache.commons.io.FileUtils
import util.{ALDatasets, Datasets}

import scala.collection.mutable
import scala.util.Random

/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db, ou seja, um dataset ou o appfile.
 * Uma vez aberta, a conexao aceita consultas simultaneas
 * que sao resolvidas pelo SQLite.
 * Ao fechar a conexão com close() (ou se algum método antecipar) o arquivo é
 * copiado de volta para seu local de origem.
 * O programa é interrompido (espera-se) caso mais de
 * uma conexão seja tentada no mesmo arquivo.
 */
trait Database extends Lock {
  var connection: Connection = null
  val path: String
  val database: String
  val create: Boolean

  lazy val dbOriginal = new File(path + database + ".db")
  lazy val dbCopy = new File("/tmp/" + database + ".db")

  def createDatabase() = {
    val fw = new FileWriter(dbOriginal)
    fw.close()
  }

  /**
   * Opens connection to database.
   * @param debug true, if the dataset had to be created (create parameter should be also true)
   */
  def open(debug: Boolean = false) = {
    //check file existence and if it is in usen
    if (dbCopy.exists()) {
      println(dbCopy + " já existe! Talvez outro processo esteja usando " + dbOriginal + ".")
      sys.exit(0)
    }
    var created = false
    if (create) {
      if (!dbOriginal.exists()) {
        createDatabase()
        created = true
      }
    } else {
      if (!dbOriginal.exists()) {
        println(dbOriginal + " não existe!")
        sys.exit(0)
      }
    }

    //open
    try {
      FileUtils.copyFile(dbOriginal, dbCopy)
      Class.forName("org.sqlite.JDBC")
      val url = "jdbc:sqlite:////" + dbCopy
      connection = DriverManager.getConnection(url)
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems opening db connection: " + dbCopy + ":")
        println(e.getMessage)
        sys.exit(0)
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
          println("\nProblems Attaching " + appPath + ".")
          sys.exit(0)
      }
      if (debug) println(" Dataset " + appPath + "app.db attached!")
    }
    created
  }

  def run(sql: String) = {
    if (connection == null) {
      println("Impossible to get connection to apply sql query " + sql + ". Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(0)
    }

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
        statement.execute(sql)
        Left(0)
      }
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems executing SQL query '" + sql + "' in: " + dbCopy + ".\n" + e.getMessage)
        sys.exit(0)
    }
  }

  /**
   * Antecipates cpying of file from /tmp to the original
   * which would normally occur at close().
   */
  def save() {
    //    println("Copying " + dbCopy + " to " + dbOriginal + "...")
    FileUtils.copyFile(dbCopy, dbOriginal)
    //    println(" " + dbCopy + " to " + dbOriginal + " copied!")
  }

  def close() {
    save
    //    println("Deleting " + dbCopy + "...")
    dbCopy.delete()
    //    println(" " + dbCopy + " deleted!")
    connection.close()
    connection = null
  }
}

