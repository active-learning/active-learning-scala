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
import ml.classifiers.Learner
import ml.models.Model
import ml.{Pattern, PatternParent}
import weka.experiment.InstanceQuerySQLite

import scala.collection.JavaConversions._

/**
 * Cada instancia desta classe representa um ML dataset.
 */
case class Ds(path: String, debug: Boolean = false)(dataset: String) extends Db(s"$path/$dataset.db", debug) with Blob {
  override lazy val toString = dataset
  lazy val n = read("select count(1) from i").head.head.toInt
  lazy val patterns = fetchPatterns("i order by id asc")
  lazy val calculaQ = {
    val poolIds = read("SELECT id FROM p WHERE s=0 AND l IN (1,2,3)").map(_.head)
    val hit_t = readBlobs(s"select mat,t from h WHERE p IN (${poolIds.mkString(",")}) ORDER BY t").map { case (b, t) => hits(stretchFromBytes(b).grouped(nclasses).map(_.toArray).toArray) -> t}
    val maxAcc = hit_t.map(_._1).max
    val firstTAtMaxAcc = hit_t.filter(_._1 == maxAcc).sortBy(_._2).head._2
    write(s"INSERT INTO r values (0, -1, $firstTAtMaxAcc)")
    firstTAtMaxAcc
  }
  lazy val Q = {
    val r = read(s"select v from r where m=0 AND p=-1").head
    if (r.isEmpty) None else Some(r)
  }
  lazy val nclasses = patterns.head.nclasses

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
    if (queriesFinished(queryPoolId, pool)) log(s"Queries do pool $run.$fold já estavam gravadas para $strat.${strat.learner}.")(dataset)
    else {
      val sqls = strat.queries.take(q).zipWithIndex map { case (patt, t) => s"INSERT INTO q VALUES ($queryPoolId, $t, ${patt.id})"}
      batchWrite(sqls.toList)
    }
  }

  /**
   * Grava tantas matrizes de confusão quantas {queries - |Y| + 1} houver.
   * Time step das tabelas q e h é o mesmo.
   * +1 é referente à mat. de conf. do primeiro build, i.e. t = |Y| - 1.
   * Outra forma de ver é que as primeiras |Y| queries geram a primeira mat. de conf.
   * Strat.learner deve ser igual a learner, em strats gnósticas.
   * @param pool
   * @param testSet
   * @param queries
   * @param strat
   * @param run
   * @param fold
   * @param learner
   * @return
   */
  def writeHits(pool: Seq[Pattern], testSet: Seq[Pattern], queries: Vector[Pattern], strat: Strategy, run: Int, fold: Int)(learner: Learner) =
    if (learner.id != strat.learner.id && strat.id != 0 && strat.id != 1) quit(s"Provided learner $learner is different from gnostic strategy's learner $strat.${strat.learner}")
    else {
      val hitPoolId = poolId(strat, learner, run, fold)
      if (hitsFinished(hitPoolId, pool)) log(s"Hits do pool $run.$fold já estavam gravados para $strat.$learner.")(dataset)
      else {
        val initialPatterns = pool.take(nclasses)
        val rest = pool.drop(nclasses)
        var m: Model = null
        val sqls = null +: rest.zipWithIndex map { case (patt, idx) =>
          val t = idx + nclasses - 1
          m = if (patt == null) learner.build(initialPatterns)
          else learner.update(m, fast_mutable = true)(patt)
          val cm = m.confusion(testSet)
          val blob = shrinkToBytes(cm.flatten)
          s"INSERT INTO h VALUES ($hitPoolId, $t, $blob)"
        }
        batchWrite(sqls.toList)
      }
    }
}
