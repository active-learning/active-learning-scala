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

import al.strategies.{RandomSampling, Strategy}
import ml.Pattern
import ml.classifiers.{NoLearner, Learner}
import util.{ALDatasets, Datasets, Tempo}
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable

/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db que é um dataset.
 */
case class Dataset(path: String, createOnAbsence: Boolean = false, readOnly: Boolean = false)(dataset: String) extends Database {
  val runs = 5
  val folds = 5
  val database = dataset
  /**
   * Assumes Rnd queries will never be partially recorded.
   */
  lazy val rndCompletePools = exec(s"select * from query where strategyid=1 group by run,fold").right.get.length
  val sidmap = mutable.Map[String, Int]()
  val lidmap = mutable.Map[String, Int]()

  /**
   * Returns only the recorded number of tuples.
   * You should add runs*folds*|Y|²*|Y| manually.
   */
  def rndCompleteHits(learner: Learner) = exec(s"select count(*) from hit,${where(RandomSampling(Seq()), learner)}").left.get

  lazy val rndCompletePerformedQueries = exec(s"select count(*) from query,${where(RandomSampling(Seq()), NoLearner())}").left.get

  def where(strategy: Strategy, learner: Learner) = s" app.strategy as s, app.learner as l where strategyid=s.rowid and s.name='$strategy' and learnerid=l.rowid and l.name='$learner'"

  def fetchQueries(strat: Strategy, run: Int, fold: Int, f: Standardize) = {
    acquire()
    val queries = ALDatasets.queriesFromSQLite(this)(strat, run, fold) match {
      case Right(x) => x
      case Left(str) => println(s"Problem loading queries for Rnd: $str")
        sys.exit(0)
    }
    release()
    Datasets.applyFilter(queries, f)
  }

  def saveHits(strat: Strategy, learner: Learner, run: Int, fold: Int, nc: Int, f: Standardize, testSet: Seq[Pattern]) {
    val lid = fetchlid(learner)
    val sid = fetchsid(strat)

    //descobre em que ponto das queries retomar os hits
    Tempo.start
    val nextPos = nextHitPosition(strat, learner, run, fold)
    Tempo.print_stop
    val timeStep = math.max(nc, nextPos)
    //    if (timeStep==3) println(s"next=3: $strat $learner $run $fold")
    val queries = fetchQueries(strat, run, fold, f)
    Tempo.print_stop

    //retoma hits
    val initial = queries.take(timeStep)
    val rest = queries.drop(timeStep)
    if (rest.nonEmpty) {
      var model = learner.build(initial)
      Tempo.print_stop

      //train
      val results = rest.zipWithIndex map { case (trainingPattern, idx) =>
        model = learner.update(model, fast_mutable = true)(trainingPattern)
        val confusion = model.confusion(testSet)
        val position = timeStep + idx
        var i = 0
        var j = 0
        val sqls = mutable.Queue[String]()
        while (i < nc) {
          j = 0
          while (j < nc) {
            val sql = s"insert into hit values ($sid, $lid, $run, $fold, $position, $i, $j, ${confusion(i)(j)})"
            sqls.enqueue(sql)
            j += 1
          }
          i += 1
        }
        sqls
      }
      Tempo.print_stop

      //save
      batchWrite(results.toArray.flatten)
      Tempo.print_stop
    }
  }

  /**
   * Returns only the recorded number of tuples.
   * You should add |Y|²*|Y| manually.
   * @param strategy
   * @param learner
   * @param run
   * @param fold
   * @return
   */
  def countHits(strategy: Strategy, learner: Learner, run: Int, fold: Int) =
    exec(s"select count(*) from hit,${where(strategy, learner)} and run=$run and fold=$fold").left.get

  def completePools(strategy: Strategy) =
    exec(s"select * from query,${where(strategy, strategy.learner)} group by run,fold").right.get.length

  def performedQueries(strategy: Strategy, run: Int, fold: Int) = {
    val n = exec(s"select count(*) from query,${where(strategy, strategy.learner)} and run=$run and fold=$fold").left.get
    if (n == 0) 0
    else exec(s"select max(position) from query,${where(strategy, strategy.learner)} and run=$run and fold=$fold").right.get.head.head.toInt + 1
  }

  def nextHitPosition(strategy: Strategy, learner: Learner, run: Int, fold: Int) = {
    val sql1 = "select count(*) from hit," + where(strategy, learner) + s" and run=$run and fold=$fold"
    val sql2 = "select max(position) from hit," + where(strategy, learner) + s" and run=$run and fold=$fold"
    val n = exec(sql1).left.get
    if (n == 0) 0
    else exec(sql2).right.get.head.head.toInt + 1
  }

  def fetchsid(strat: Strategy) = {
    //Fetch StrategyId by name.
    lazy val sid = try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("select rowid from app.strategy where name='" + strat + "'")
      resultSet.next()
      resultSet.getInt("rowid")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems consulting strategy to insert queries into: " + dbCopy + " with query \"" + "select rowid from app.strategy where name='" + strat + "'" + "\".")
        sys.exit(0)
    }
    sidmap.getOrElseUpdate(strat.toString, sid)
  }


  def fetchlid(learner: Learner) = {
    //Fetch LearnerId by name.
    lazy val lid = try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("select rowid from app.learner where name='" + learner + "'")
      resultSet.next()
      resultSet.getInt("rowid")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems consulting learner to insert queries into: " + dbCopy + ".")
        sys.exit(0)
    }
    lidmap.getOrElseUpdate(learner.toString, lid)
  }

  /**
   * Inserts query-tuples (run, fold, position, instid) into database.
   * All queries for a given pair run-fold should be written at once.
   * The original file is updated at the end.
   * If the given pair run/fold already exists, queries are resumed from that point.
   *
   * Generates queries until Q from the provided strategy,
   * unless Q is not given (in this case all possible queries (the entire pool) will be generated)
   * or a time restriction is specified.
   *
   * filter f is needed to resume queries
   *
   * @param strat
   * @param run
   * @param fold
   * @param seconds time limit, if exceeded, waits for the current query to end and finish querying
   * @param Q
   * @return the total number of queries generated by the strategy along all jobs in this pool
   */
  def saveQueries(strat: Strategy, run: Int, fold: Int, f: Standardize, seconds: Double, Q: Int = Int.MaxValue) = {
    if (readOnly) {
      println("Cannot save queries on a readOnly database!")
      sys.exit(0)
    }
    if (!isOpen) {
      println(s"Impossible to get connection to write queries at the run $run and fold $fold for strategy $strat and learner ${strat.learner}. Isso acontece após uma chamada a close() ou na falta de uma chamada a open().")
      sys.exit(0)
    }
    val stratId = fetchsid(strat)
    val learnerId = fetchlid(strat.learner)

    //Check if there are more queries than the size of the pool (or greater than Q).
    var q = -1
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(s"select count(rowid) as q from query where strategyid=$stratId and learnerid=$learnerId and run=$run and fold=$fold")
      resultSet.next()
      q = resultSet.getInt("q")
      if (q > Q || q > strat.pool.size) {
        println("Excess of queries (" + q + ") fetched from dataset " + dbCopy + " for run=" + run + " and fold=" + fold + ". They should amount to lesser than the limit " + Q + s" and the pool size ${strat.pool.size}")
        sys.exit(0)
      }
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems looking for preexistence of queries in: " + dbCopy + ".")
        sys.exit(0)
    }

    //Get queries from past jobs, if any.
    val queries = fetchQueries(strat, run, fold, f)
    val nextPosition = queries.size
    if (nextPosition < Q && nextPosition < strat.pool.size) {
      val (nextIds, t) = if (nextPosition == 0) Tempo.timev(strat.timeLimitedQueries(seconds).take(Q).map(_.id).toVector)
      else Tempo.timev(strat.timeLimitedResumeQueries(queries, seconds).take(Q - nextPosition).map(_.id).toVector)
      q = nextIds.length
      acquire()
      var str = ""
      try {
        val statement = connection.createStatement()
        statement.executeUpdate("begin")
        nextIds.zipWithIndex.foreach { case (pattId, idx) =>
          val position = nextPosition + idx
          str = s"insert into query values ($stratId,$learnerId,$run,$fold,$position,$pattId)"
          statement.executeUpdate(str)
        }
        str = s"insert or ignore into time values ($stratId,$learnerId,$run,$fold,0)"
        statement.executeUpdate(str)
        str = s"update time set value = value + $t where strategyid=$stratId and learnerid=$learnerId and run=$run and fold=$fold"
        statement.executeUpdate(str)
        statement.executeUpdate("end")
      } catch {
        case e: Throwable => e.printStackTrace
          println(s"\nProblems inserting queries for $strat / ${strat.learner} into: $dbCopy: [ $str ]:")
          println(e.getMessage)
          println("Deleting " + dbCopy + "...")
          dbCopy.delete()
          println(" " + dbCopy + " deleted!")
          sys.exit(0)
      }
      println(s"$q $strat queries written to " + dbCopy + ". Backing up tmpFile...")
      save()
      release()
      nextPosition + q
    } else nextPosition
  }
}

object DatasetTest extends App {
  //load patterns
  //  val patts = Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("iris").right.get

  //reorder patterns as queries
  //  val shuffled = patts.drop(5) ++ patts.take(4)

  //write queries
  val d = Dataset("/home/davi/wcs/ucipp/uci/")("iris")
  d.open(debug = true)
  //  d.saveQueries(RandomSampling(patts), 64, 17, 0.2)

  //load queries as patterns
  val qpatts = ALDatasets.queriesFromSQLite(d)(RandomSampling(Seq()), 0, 0) match {
    case Right(x) => x
    case Left(str) => println(s"Problema: $str"); sys.exit(0)
  }
  d.close()
  qpatts foreach println

}