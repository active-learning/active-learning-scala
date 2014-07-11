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

package util

import java.io.{File, IOException}

import al.strategies.Strategy
import app.db.Dataset
import ml.classifiers.Learner
import ml.{Pattern, PatternParent}
import weka.experiment.{InstanceQuerySQLite, InstanceQuery}

object ALDatasets {

  import scala.collection.JavaConversions._

  /**
   * Reads SQLite patterns in the querying order.
   */
  def queriesFromSQLite(path: String)(db: Dataset)(strategy: Strategy, run: Int, fold: Int) = {
    val learner = strategy.learner
    val arq = db.dbCopy
    val qids = db.run(s"select q.rowid from query as q, app.strategy as s, app.learner as l where run = $run and fold = $fold and q.strategyid=s.rowid and s.name='$strategy' and q.learnerid=l.rowid and l.name='$learner' order by position") match {
      case Right(x) => x.map(_.head.toInt)
      case Left(str) => println(s"Problems fetching query ids from $db : $str")
        sys.exit(0)
    }
    val queriedInstanceIds = db.run(s"select q.instid from query as q, app.strategy as s, app.learner as l where run = $run and fold = $fold and q.strategyid=s.rowid and s.name='$strategy' and q.learnerid=l.rowid and l.name='$learner' order by position") match {
      case Right(x) => x.map(_.head.toInt)
      case Left(str) => println(s"Problems fetching queries from $db : $str")
        sys.exit(0)
    }
    try {
      val query = new InstanceQuerySQLite()
      query.setDatabaseURL("jdbc:sqlite:////" + arq)
      query.setQuery(s"select i.* from query as q, inst as i, app.strategy as s, app.learner as l where q.instid = i.rowid and run = $run and fold = $fold and q.strategyid=s.rowid and s.name='$strategy' and q.learnerid=l.rowid and l.name='$learner' order by position")
      query.setDebug(false)
      val instances = query.retrieveInstances()
      instances.setClassIndex(instances.numAttributes() - 1)
      instances.setRelationName(db.database)
      val parent = PatternParent(instances)
      val patterns = instances.zip(queriedInstanceIds).map { case (instance, idx) => Pattern(idx + 1, instance, false, parent)}
      Right(patterns.zip(qids).toStream)
    } catch {
      case ex: Exception => Left("Problems reading file " + arq + ": " + ex.getMessage)
    }
  }
}
