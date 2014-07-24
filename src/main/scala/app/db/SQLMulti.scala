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

import java.sql.DriverManager

import app.ArgParser

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

  //todo:useDatabase class
  Class.forName("org.sqlite.JDBC")
  val url = "jdbc:sqlite::memory:"
  val connection = DriverManager.getConnection(url)
  val statement = connection.createStatement()
  ???
  //todo:terminar


}
