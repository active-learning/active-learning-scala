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

/**
 * Created by davi on 09/06/14.
 */
object SQLBatch extends App {
  val desc = s"Version ${ArgParser.version} \n Apply SQL queries to all provided databases independently." +
    "This program is needed because SQLite has a limit of only 20 simultaneous attached datasets. Parallel:y|n"
  val (path, datasetNames, sql) = ArgParser.testArgsWithText(getClass.getSimpleName.dropRight(1), args, desc)
  val parallel = args(2) == "y"
  val dest = Dataset(path) _

  (if (parallel) datasetNames.par else datasetNames) foreach { datasetName =>
    val db = dest(datasetName)
    db.open()
    print(datasetName + " ")
    db.run(sql) match {
      case Right(queue) =>
        println(queue.mkString("\n" + datasetName.map(_ => ' ') + " "))
      case Left(rowCount) => println(rowCount)
    }
    db.save()
    db.close()
  }
}
