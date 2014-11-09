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

import java.io.File
import weka.core.Instances

import scala.util.{Failure, Success, Try}
import app.ArgParser
import weka.core.converters.DatabaseSaverCustomized

object ARFF2SQLite extends App {
  //  Class.forName("org.sqlite.JDBC")
  val desc = """Version 1
It preprocess the ARFF files before sendind to SQLite.
Steps:
  RemoveUseless attributes
  deduplicate (keeps only one instance with the mode of the conflicting labels).
  apply RandomProjection (numeric) to keep at most 1000 attributes            """


  val (path, names) = ArgParser.testArgs(getClass.getSimpleName.dropRight(1), args, 3, desc)

  def f(name: String, instancesOp: => Option[Instances]) {
    val file = new File(path + name + ".db")
    if (file.exists) {
      //TODO: Test beyond the emptyness of db file.
      if (file.length == 0) {
        file.delete
        println("Deleted empty database for '" + name + "'.")
      }
    } else if (instancesOp.isDefined) {
      val instances = instancesOp.get
      val save = new DatabaseSaverCustomized
      save.setUrl("jdbc:sqlite:////" + path + name + ".db")
      save.setInstances(instances)
      save.setRelationForTableName(false)
      save.setTableName("i")
      save.setAutoKeyGeneration(true)
      save.connectToDatabase()
      Try(save.writeBatchExcep()) match {
        case Success(_) =>
          //The weights were used as clipboard, but weight info is not written to SQLite. That's good.
          // instances variable cannot be used after this unless weights are properly recovered.
          println(" Finished: '" + name + "'.")
        case Failure(ex) =>
          val file = new File(path + name + ".db")
          file.delete
          println(ex + "\nSkipping dataset '" + name + "'.")
      }
    }
  }

  ArgParser.applyArgs(args)(f)
}
