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
import weka.filters.unsupervised.attribute.RandomProjection
import weka.filters.Filter
import app.ArgParser
import weka.core.converters.DatabaseSaverForSQLite

object ARFF2SQLite extends App {
  Class.forName("org.sqlite.JDBC")
  val desc = """Version 0.9
It preprocess the ARFF files before sendind to SQLite.
It does not preserve the order of the instances.
Steps:
  RemoveUseless attributes
  Nominal -> Binary (numeric)
  deduplicate (keeps only one instance with the mode of the conflicting labels).
  apply RandomProjection to keep at most 1999 attributes            """


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
      val instancesProj = if (instances.numAttributes > 1998) {
        val filter = new RandomProjection
        filter.setNumberOfAttributes(1998)
        filter.setInputFormat(instances)
        filter.setSeed(0)

        //        Some(Filter.useFilter(instances, filter))
        Try(Filter.useFilter(instances, filter)) match {
          case Success(x) => println("Reduced attributes from " + instances.numAttributes + " to " + x.numAttributes + " in '" + name + "'.")
            Some(x)
          case Failure(ex) => println("\nSkipping dataset '" + name + "' due to " + ex + "\n" + Thread.currentThread().getStackTrace.mkString("\n"))
            None
        }
      } else Some(instances)

      if (instancesProj.isDefined) {
        //ignores completly id attributed by ARFF loader in Datasets object
        val instances = instancesProj.get
        val save = new DatabaseSaverForSQLite
        save.setUrl("jdbc:sqlite:////" + path + name + ".db")
        save.setInstances(instances)
        save.setRelationForTableName(false)
        save.setAutoKeyGeneration(false)
        save.setTableName("inst")
        save.connectToDatabase()
        Try(save.writeBatchExcep()) match {
          case Success(_) => println(" Finished: '" + name + "'.")
          case Failure(ex) =>
            val file = new File(path + name + ".db")
            file.delete
            println(ex + "\nSkipping dataset '" + name + "'.")
        }
      }
    }
  }

  ArgParser.applyArgs(args)(f)
}
