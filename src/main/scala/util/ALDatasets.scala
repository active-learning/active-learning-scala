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
import weka.experiment.InstanceQuery

object ALDatasets {

  import scala.collection.JavaConversions._

  /**
   * Reads SQLite patterns in the querying order.
   */
  def queriesFromSQLite(path: String)(dataset: String)(strategy: Strategy, run: Int, fold: Int) = {
    val learner = strategy.learner
    val arq = path + dataset + ".db"
    val dbCopy = new File("/tmp/" + dataset + ".db")
    if (dbCopy.exists()) {
      println(dbCopy + " was found! Talvez outro processo esteja atualizando a base de dados " + arq + ".")
      sys.exit(0)
    }
    val dbOriginal = new File(path + dataset + ".db")
    if (!dbOriginal.exists()) {
      println(dbOriginal + " não existe! Talvez seja preciso rodar o conversor ARFF -> SQLite.")
      sys.exit(0)
    }
    val db = Dataset(path, createOnAbsence = false, readOnly = true)(dataset)
    val queriedInstanceIds = db.run(s"select instid from query, strategy as s, learner as l where run = $run and fold = $fold and strategyid=s.id and s.name='$strategy' and learnerid=l.id and l.name='$learner' order by position") match {
      case Right(x) => Seq(0) //x.map(_.head)
      case Left(str) => println(s"Problems fetching queries from $dataset !")
        sys.exit(0)
    }
    try {
      val query = new InstanceQuery()
      query.setDatabaseURL("jdbc:sqlite:////" + arq)
      query.setQuery(s"select inst.* from query as q, inst as i, strategy as s, learner as l where q.instid = i.rowid and run = $run and fold = $fold and strategyid=s.id and s.name='$strategy' and learnerid=l.id and l.name='$learner' order by position")
      query.setDebug(false)
      val instances = query.retrieveInstances()
      instances.setClassIndex(instances.numAttributes() - 1)
      instances.setRelationName(dataset)
      val parent = PatternParent(instances)
      val patterns = instances.zip(queriedInstanceIds).map { case (instance, idx) => Pattern(idx + 1, instance, false, parent)}
      Right(patterns.toStream)
    } catch {
      case ex: IOException => Left("Problems reading file " + arq + ": " + ex.getMessage)
    }
  }
}
