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
import ml.{Pattern, PatternParent}
import util.Datasets
import weka.experiment.InstanceQuerySQLite
import weka.filters.Filter

import scala.collection.JavaConversions._

/**
 * Cada instancia desta classe representa um ML dataset.
 */
case class Ds(path: String, dataset: String) extends Db(s"$path/$dataset.db") with Blob {
  override lazy val toString = dataset
  override val context = dataset
  lazy val n = read("select count(1) from i").head.head.toInt
  lazy val nclasses = patterns.head.nclasses
  lazy val patterns = fetchPatterns("i order by id asc")
  lazy val Q = {
    val r = fetchQ()
    if (r.isEmpty) error("Q not found.") else r.head.toInt
  }
  lazy val singlePoolSizeForTests = nclasses * 3 //less than 3 makes impossible to generate enough hits

  private def fetchQ() = read(s"select v from r where m=0 AND p=-1").map(_.head)

  def reset() {
    //query [pool timeStep instance]
    //hit [pool timeStep blobMatrix(realClass X guessedClass values)] (confusion matrix blob)
    //pool [strat learner run fold]
    //result [app.measure pool value] (Q, ...)
    //time [pool value] in seconds
    write("drop table if exists q")
    write("drop table if exists h")
    write("drop table if exists p")
    write("drop table if exists r")
    write("drop table if exists t")
    write("CREATE TABLE q ( p INT, t INT, i INT, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, UNIQUE (p, i) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id), FOREIGN KEY (i) REFERENCES i (id) ); ")
    write("CREATE TABLE h ( p INT, t INT, mat BLOB, PRIMARY KEY (p, t) ON CONFLICT ROLLBACK, FOREIGN KEY (p) REFERENCES p (id) );")
    write("CREATE TABLE p ( id INTEGER PRIMARY KEY ON CONFLICT ROLLBACK, s INT, l INT, r INT, f INT, UNIQUE (s, l, r, f) ON CONFLICT ROLLBACK ); ")
    write("CREATE TABLE r ( m INT, p INT, v FLOAT, PRIMARY KEY (m, p) ON CONFLICT ROLLBACK, FOREIGN KEY (m) REFERENCES measure (id), FOREIGN KEY (p) REFERENCES p (id) ); ")
    write("CREATE TABLE t ( p INTEGER PRIMARY KEY ON CONFLICT ROLLBACK, v INT, FOREIGN KEY (p) REFERENCES p (id) ); ")
  }

  /**
   * Reads all SQLite patterns according to given SQL conditions.
   * Note that nominal attributes must have all possible values present in the resulting sample!
   * It opens a new connection, so it will have to wait to open a connection under writing ops.
   */
  private def fetchPatterns(sqlTail: String) = try {
    val ids = read("select i.id from " + sqlTail).map(_.head.toInt)
    println(s"Fetching patterns from $database ...")
    val query = new InstanceQuerySQLite()
    query.setDatabaseURL("jdbc:sqlite:////" + database)
    query.setQuery("select i.* from " + sqlTail)
    query.setDebug(false)
    val instances = query.retrieveInstances()
    instances.setClassIndex(instances.numAttributes() - 1)
    instances.setRelationName(dataset)
    val parent = PatternParent(instances)
    val res = instances.zip(ids).map {
      case (instance, idx) => Pattern(idx, instance, missed = false, parent)
    }
    query.close()
    res.toVector
  } catch {
    case ex: Exception => error(s"${
      ex.getStackTraceString
    } \n Problems reading file $database: ${
      ex.getMessage
    }")
  }

  private def poolId(strat: Strategy, learner: Learner, run: Int, fold: Int) = {
    read(s"SELECT id FROM p WHERE s=${strat.id} and l=${learner.id} and r=$run and f=$fold") match {
      case List() => None
      case List(seq) => Some(seq.head.toInt)
    }
  }

  def isQCalculated = fetchQ().nonEmpty

  def areQueriesFinished(pool: Seq[Pattern], strat: Strategy, run: Int, fold: Int) =
    poolId(strat, strat.learner, run, fold) match {
      case None => false
      case Some(pid) =>
        val PoolSize = pool.size
        val (qs, lastT) = read(s"SELECT COUNT(1),max(t+0) FROM q WHERE p=$pid") match {
          case List(Vector(c, m)) => c -> m
          case List() => error(s"Inconsistency: there is a pool $pid for no queries!")
        }
        if (qs != lastT + 1) error(s"Inconsistency: $qs queries differs from last timeStep+1 ${lastT + 1}")
        strat.id match {
          case x if x < 2 => qs match {
            case 0 => error(s"Inconsistency: there is a pool $pid for no queries!")
            case PoolSize => true
            case _ => error(s"$qs previous agnostic queries should be $PoolSize")
          }
          case _ => qs match {
            case 0 => error(s"Inconsistency: there is a pool $pid for no queries!")
            case Q => true
            case _ => error(s"$qs previous queries should be $Q")
          }
        }
    }

  def areHitsFinished(pool: Seq[Pattern], strat: Strategy, learner: Learner, run: Int, fold: Int) =
    if (learner.id != strat.learner.id && strat.id > 1) quit(s"areHitsFinished: Provided learner $learner is different from gnostic strategy's learner $strat.${strat.learner}")
    else if (!areQueriesFinished(pool, strat, run, fold)) error(s"Queries must be finished to check hits!")
    else {
      poolId(strat, learner, run, fold) match {
        case None => false
        case Some(pid) =>
          val ExpectedHitsForFullPool = pool.size - nclasses + 1
          val hs = read(s"SELECT COUNT(1),max(t+0) FROM h WHERE p=$pid") match {
            case List(Vector(0)) => 0
            case List(Vector(c, m)) => if (c == m - nclasses + 2) c
            else error(s"Inconsistency: $hs cms differs from last timeStep+1 ${m - nclasses + 2}")
            case _ => error(s"Inconsistency: there is a pool $pid for no hits! s=$strat l=$learner")
          }
          (strat.id, learner.id) match {
            case (s, l) if s < 2 && l < 4 => hs match {
              case 0 => error(s"Inconsistency: there is a pool $pid for no hits!")
              case ExpectedHitsForFullPool => true
              case _ => error(s"$hs previous agnostic hits should be $ExpectedHitsForFullPool")
            }
            case _ => hs match {
              case 0 => error(s"Inconsistency: there is a pool $pid for no hits!")
              case Q => true
              case _ => error(s"$hs previous hits should be $Q")
            }
          }
      }
    }

  def calculaQ(runs: Int, folds: Int) {
    //get pool ids
    val numberOfLearners = 3
    val numberOfPools = runs * folds * numberOfLearners
    val numberOfConfMats = if (runs == 1 && folds == 1) {
      //special case for tests (TopSpec.scala)
      (singlePoolSizeForTests - nclasses + 1) * numberOfLearners
    } else (runs * (folds - 1)) * numberOfLearners - (nclasses - 1) * numberOfPools
    val poolIds = read("SELECT id FROM p WHERE s=0 AND l IN (1,2,3)").map(_.head)
    if (numberOfPools != poolIds.size) error(s"${poolIds.size} found, $numberOfPools expected!")

    //get conf. mats and create map hits->timeStep
    val hits_t = readBlobs(s"select mat,t from h WHERE p IN (${poolIds.mkString(",")}) ORDER BY t").map {
      case (b, t) =>
        hits(blobToConfusion(b, nclasses)) -> t
    }
    if (numberOfConfMats != hits_t.size) error(s"${hits_t.size} found, $numberOfConfMats expected (|Y|=$nclasses)!")

    //get and write smallest time step with maximum accuracy
    val maxAcc = hits_t.map(_._1).max
    val firstTAtMaxAcc = hits_t.filter(_._1 == maxAcc).sortBy(_._2).head._2
    val qToWrite = math.max(firstTAtMaxAcc, nclasses)
    write(s"INSERT INTO r values (0, -1, $qToWrite)")
  }

  def countQueries(strat: Strategy, run: Int, fold: Int) = {
    val pid = poolId(strat, strat.learner, run, fold).getOrElse(quit(s"Pool not found for  s=${strat.id} and l=${strat.learner.id} and r=$run and f=$fold !"))
    read(s"SELECT COUNT(1) FROM q WHERE p=$pid") match {
      case List() => 0
      case List(seq) => seq.head.toInt
    }
  }

  def getCMs(strat: Strategy, learner: Learner, run: Int, fold: Int) = poolId(strat, learner, run, fold) match {
    case None => error("Attempt to get hits without an existent related pid.")
    case Some(pid) =>
      val cms = readBlobs(s"select mat,t from h WHERE p=$pid ORDER BY t").map {
        case (b, t) => blobToConfusion(b, nclasses)
      }
      val numberOfQueries = countQueries(strat, run, fold)
      val expectedCms = numberOfQueries - nclasses + 1
      if (expectedCms != cms.size) error(s"${cms.size} conf mats found, $expectedCms expected!")
      cms
  }

  /**
   * Retrieve instances from disk in querying order.
   * Apply filters previously calibrated with training set.
   * Fetch all instances first to avoid missing nominal attribute values in dataset/"ARFF" header.
   * @param strat
   * @param run
   * @param fold
   * @param binaf
   * @param zscof
   * @return
   */
  def queries(strat: Strategy, run: Int, fold: Int, binaf: Filter, zscof: Filter) = {
    val patts = read(s"SELECT i.id FROM i, q, p where i.id=q.i and p.id=p and p.s=${strat.id} and p.l=${strat.learner.id} and p.r=$run and p.f=$fold order by t asc") match {
      case List() => quit("No queries found!")
      case list: List[Vector[Double]] =>
        val ids = list.map(_.head.toInt)
        val m = (patterns map (p => p.id -> p)).toMap
        ids map m
    }
    if (binaf == null) patts
    else {
      val binaPatts = Datasets.applyFilter(binaf)(patts)
      Datasets.applyFilter(zscof)(binaPatts).toList
    }
  }

  def writeQueries(pool: Seq[Pattern], strat: Strategy, run: Int, fold: Int, q: Int) = {
    poolId(strat, strat.learner, run, fold) match {
      case Some(queryPoolId) => quit(s"Pool $run.$fold já estava gravado para $strat.${strat.learner} referente às queries a gravar.")
      case None =>
        val qs = strat.queries.take(q).toList
        val poolSQL = s"INSERT INTO p VALUES (NULL, ${strat.id}, ${strat.learner.id}, $run, $fold)"
        val sqls = poolSQL +: (qs.zipWithIndex map { case (patt, t) =>
          s"INSERT INTO q select id, $t, ${patt.id} from p where s=${strat.id} and l=${strat.learner.id} and r=$run and f=$fold"
        })
        batchWrite(sqls.toList)
        qs
    }
  }

  /**
   * Valor de Time step das tabelas q e h é o mesmo.
   * Primeira conf mat é referente à mat. de conf. do primeiro build, i.e. t = |Y| - 1.
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
    if (learner.id != strat.learner.id && strat.id > 1)
      quit(s"Provided learner $learner is different from gnostic strategy's learner $strat.${strat.learner}")
    else {
      //agnostic strats gravam um poolId que tem NoLearner, não-reutilizável pra hits.
      val insertIntoP = poolId(strat, learner, run, fold) match {
        case Some(pid) => if (strat.id < 2) quit(s"Pool $run.$fold já estava gravado para $strat.$learner referente aos hits de $strat.") else "SELECT 1"
        case None => if (strat.id < 2) s"INSERT INTO p VALUES (NULL, ${strat.id}, ${learner.id}, $run, $fold)" else quit(s"Missing gnostic queries pid for hits.")
      }

      //para rnd e os 3 learners especiais, Q = |U|.
      val expectedQ = if (strat.id == 0 && learner.id < 4) pool.size else Q
      if (expectedQ != queries.size) quit(s"Number of ${queries.size} provided queries for hits is different from $expectedQ expected!")
      val (initialPatterns, rest) = queries.splitAt(nclasses)
      var m = learner.build(initialPatterns)
      val tuples = (insertIntoP, null) +: ((null +: rest).zipWithIndex map { case (patt, idx) =>
        val t = idx + nclasses - 1
        if (patt != null) m = learner.update(m, fast_mutable = true)(patt)
        val cm = m.confusion(testSet)
        val blob = confusionToBlob(cm)
        (s"INSERT INTO h SELECT id, $t, ? FROM p where s=${strat.id} and l=${learner.id} and r=$run and f=$fold", blob)
      }).toList
      val (sqls, blobs) = tuples.unzip
      log(tuples.mkString("\n"))
      batchWriteBlob(sqls, blobs)
    }
}