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

import al.strategies.Strategy
import app.db.entities.Dataset
import ml.classifiers.Learner
import ml.{Pattern, PatternParent}
import weka.experiment.InstanceQuerySQLite

import scala.collection.JavaConversions._
import scala.util.Left

/**
 * Cada instancia desta classe representa um ML dataset.
 */
case class Ds(path: String, debug: Boolean = false)(dataset: String) extends Db(s"$path/$dataset.db", debug) {
  override lazy val toString = dataset
  lazy val n = read("select count(1) from i").head.head.toInt
  lazy val patterns = fetchPatterns("i order by id asc")
  lazy val Q = read(s"select v from r where m=0 AND p=-1")

  /**
   * Reads SQLite patterns in the querying order.
   * It opens a new connection, so it will not be able to open a connection under writing ops.
   */
  def fetchPatterns(sqlTail: String) = try {
    val ids = read("select i.id from " + sqlTail).map(_.head.toInt)
    if (debug) println(s"Fetching patterns from $database ...")
    val query = new InstanceQuerySQLite()
    query.setDatabaseURL("jdbc:sqlite:////" + database)
    query.setQuery("select i.* from " + sqlTail)
    query.setDebug(false)
    val instances = query.retrieveInstances()
    instances.setClassIndex(instances.numAttributes() - 1)
    instances.setRelationName(dataset)
    val parent = PatternParent(instances)
    val res = instances.zip(ids).map { case (instance, idx) => Pattern(idx, instance, missed = false, parent)}
    query.close()
    res.toVector
  } catch {
    case ex: Exception => error(s"${ex.getStackTraceString} \n Problems reading file $database: ${ex.getMessage}")
  }

  def queries(strat: Strategy, run: Int, fold: Int) = fetchPatterns(s"i, q where i.id=q.i and s=${strat.id} and l=${strat.learner.id} and r=$run and f=$fold order by t asc")

  def poolId(strat: Strategy, learner: Learner, run: Int, fold: Int) = read(s"SELECT id FROM p WHERE s=${strat.id} and l=${learner.id} and r=$run and f=$fold").head.head.toInt

  def queriesFinished(poolId: Int, pool: Seq[Pattern]) = read(s"SELECT COUNT(1) FROM q WHERE p=$poolId").head.head match {
    case 0 => false
    case qs => if (qs != pool.size) quit(s"$qs previous queries should be ${pool.size}") else true
  }

  def hitsFinished(poolId: Int, pool: Seq[Pattern]) = read(s"SELECT COUNT(1) FROM h WHERE p=$poolId").head.head match {
    case 0 => false
    case hs => if (hs != pool.size) quit(s"$hs previous hits should be ${pool.size}") else true
  }

  def writeQueries(pool: Seq[Pattern], strat: Strategy, run: Int, fold: Int, q: Int) {
    val queryPoolId = poolId(strat, strat.learner, run, fold)
    if (queriesFinished(queryPoolId, pool)) log(s"Queries do pool $run.$fold já estavam gravadas para $strat.$learner.", this)
    else {
      val sqls = strat.queries.take(q).zipWithIndex map { case (q, t) => s"INSERT INTO q VALUES ($queryPoolId, $t, ${q.id})"}
      batchWrite(sqls.toList)
    }
  }

  def writeHits(pool: Seq[Pattern], queries: Stream[Pattern], strat: Strategy, run: Int, fold: Int)(learner: Learner) =
    if (learner.id != strat.learner.id && strat.id != 0 && strat.id != 1) quit(s"Provided learner $learner is different from gnostic strategy's learner $strat.${strat.learner}")
    else {
      val hitPoolId = poolId(strat, learner, run, fold)
      if (hitsFinished(hitPoolId, pool)) log(s"Hits do pool $run.$fold já estavam gravados para $strat.$learner.", this)
      else {
        val qs = queries
        val sqls = queries.zipWithIndex map { case (q, t) => s"INSERT INTO q VALUES ($hitPoolId, $t, ${q.id})"}
        batchWrite(sqls.toList)
      }
    }
}
