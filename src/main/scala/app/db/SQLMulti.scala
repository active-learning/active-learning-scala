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

import app.ArgParser
import java.sql.DriverManager

/**
 * Created by davi on 09/06/14.
 */
object SQLMulti extends App {
  val desc = "Version 0.1 \n Apply SQL queries to all provided databases, unifying results in an volatile table, " +
    "allowing a second query to be applied in it." +
    "This program is needed because SQLite has a limited of only 20 simultaneous attached datasets."
  val (path, datasetNames, sqls) = ArgParser.testArgsWithText(getClass.getSimpleName.dropRight(1), args, desc)
  val parallel = args(2) == "y"
  val dest = Dataset(path) _
  val arr = sqls.split(';')
  val sql = arr(0)
  val sql2 = arr(1)

  Class.forName("org.sqlite.JDBC")
  val url = "jdbc:sqlite::memory:"
  val connection = DriverManager.getConnection(url)
  val statement = connection.createStatement()
  ???
  //todo:terminar
  statement.executeUpdate("create table out (ds VARCHAR, att VARCHAR, val VARCHAR)")

  (if (parallel) datasetNames.par else datasetNames) foreach { datasetName =>
    val db = dest(datasetName)
    db.open()

    db.run(sql) match {
      case Right(queue) =>
        queue.foreach { seq =>
          seq.zipWithIndex.foreach { case (v, i) =>
            val tempStatem = connection.createStatement()
            tempStatem.executeUpdate("begin")
            val str = "insert into out values ('" + datasetName + "','a" + i + "'," + v + ")"
            tempStatem.executeUpdate(str)
            tempStatem.executeUpdate("end")
          }
        }
      case Left(rowCount) => println("SQLMulti is intended to execute SELECT over all datasets."); sys.exit(0)
    }

    val tempStatem2 = connection.createStatement()
    val rs = tempStatem2.execute(sql2)
    println(rs)

    db.close
  }
}
