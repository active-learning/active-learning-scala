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

import ml.{Pattern, PatternParent}
import weka.experiment.InstanceQuerySQLite

import scala.collection.JavaConversions._

/**
 * Cada instancia desta classe representa um ML dataset.
 */
case class Ds(path: String, debug: Boolean = true)(dataset: String) extends Db(s"$path/$dataset.db", debug) {
  override lazy val toString = dataset
  lazy val patterns = {
    val ids = read("select id from i order by id asc").map(_.head.toInt)
    if (debug) println(s"Fetching patterns from $database ...")
    try {
      val query = new InstanceQuerySQLite()
      query.setDatabaseURL("jdbc:sqlite:////" + database)
      query.setQuery("select * from i order by id asc")
      query.setDebug(false)
      val instances = query.retrieveInstances()
      instances.setClassIndex(instances.numAttributes() - 1)
      instances.setRelationName(dataset)
      val parent = PatternParent(instances)
      val res = instances.zip(ids).map { case (instance, idx) => Pattern(idx, instance, missed = false, parent)}
      query.close()
      res
    } catch {
      case ex: Exception => justQuit(s"${ex.getStackTraceString} \n Problems reading file $database: ${ex.getMessage}")
    }
  }
}
