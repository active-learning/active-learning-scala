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
import exp.raw.RandomHits._
import ml.Pattern
import ml.classifiers.Learner
import util.{ALDatasets, Datasets, Tempo}
import weka.filters.unsupervised.attribute.Standardize

/**
 * Cada instancia desta classe representa uma conexao a
 * um arquivo db que é um dataset.
 */
case class Dataset(path: String, createOnAbsence: Boolean = false, readOnly: Boolean = false)(dataset: String) extends Database {
  def saveHits(strat: Strategy, learner: Learner, run: Int, fold: Int, nc: Int, f: Standardize, testSet: Seq[Pattern]) = {
    val lid = fetchlid(learner)
    val sid = fetchsid(strat)
    val fetchQueries = ALDatasets.queriesFromSQLite(path) _

    //descobre em que ponto das queries retomar
    val performed = performedQueries(RandomSampling(Seq()))

    //retoma
    fetchQueries(this)(RandomSampling(Seq()), run, fold) match {
      case Right(queries) =>
        val zscoredQueries = Datasets.applyFilter(queries, f)
        val initial = zscoredQueries.take(performed + 1)
        val rest = zscoredQueries.drop(performed + 1)
        var model = learner.build(initial)

        acquire()
        exec("begin")
        rest.zipWithIndex map { case (trainingPattern, idx) =>
          model = learner.update(model, fast_mutable = true)(trainingPattern)
          val confusion = model.confusion(testSet)
          val position = performed + idx
          var i = 0
          var j = 0
          while (i < nc) {
            while (j < nc) {
              val sql = s"insert into hit values ($sid, $lid, $run, $fold, $position, $i, $j, ${confusion(i)(j)}})"
              //                    println(sql)
              exec(sql)
              j += 1
            }
            i += 1
          }
        }
        exec("end")
        save()
        release()

      case Left(str) => println(s"Problem loading queries for Rnd: $str")
    }
  }

  val database = dataset
  lazy val rndCompletePools = exec(s"select * from query where strategyid=1 group by run,fold").right.get.length

  def rndCompleteHits(strategy: Strategy, learner: Learner, run: Int, fold: Int) = {
    exec(s"select count(*) from hit where strategyid=${fetchsid(strategy)} and learnerid=${fetchlid(learner)} and run=$run and fold=$fold").left.get
  }

  def completePools(strategy: Strategy) =
    exec(s"select * from query as q, app.strategy as s, app.learner as l where strategyid=s.rowid and s.name='$strategy' and learnerid=l.rowid and l.name='${strategy.learner}' group by run,fold").right.get.length

  def performedQueries(strategy: Strategy) =
    exec(s"select max(position) from query as q, app.strategy as s, app.learner as l where strategyid=s.rowid and s.name='$strategy' and learnerid=l.rowid and l.name='${strategy.learner}'").right.get.head.head.toInt + 1

  def fetchsid(strat: Strategy) = {
    //Fetch StrategyId by name.
    var stratId = -1
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("select rowid from app.strategy where name='" + strat + "'")
      resultSet.next()
      resultSet.getInt("rowid")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems consulting strategy to insert queries into: " + dbCopy + " with query \"" + "select rowid from app.strategy where name='" + strat + "'" + "\".")
        sys.exit(0)
    }
  }

  def fetchlid(learner: Learner) = {
    //Fetch LearnerId by name.
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("select rowid from app.learner where name='" + learner + "'")
      resultSet.next()
      resultSet.getInt("rowid")
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems consulting learner to insert queries into: " + dbCopy + ".")
        sys.exit(0)
    }
  }

  /**
   * Inserts query-tuples (run, fold, position, instid) into database.
   * All queries for a given pair run-fold should be written at once.
   * The original file is updated at the end.
   * If the given pair run/fold already exists, nothing is done.
   *
   * Generates only Q queries from the provided strategy,
   * unless Q is not given (in this case all possible queries (the entire pool) will be generated).
   *
   * @param strat
   * @param run
   * @param fold
   * @param seconds time limit, if exceeded, waits for the current query to end and finish querying
   * @param Q
   * @return the number of queries generated by the strategy
   */
  def saveQueries(strat: Strategy, run: Int, fold: Int, seconds: Double, Q: Int = Int.MinValue) = {
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
    //    println("Strategy " + strat + " has id " + stratId + ".")
    //Check if the job was already done before.
    //It assumes that there is no inconsistency like a partial job/half transactions (e.g. half of the queries) registered in dataset.
    //The consistency is guaranted by transactions.
    var alreadyDone = false
    var q = -1
    try {
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery(s"select count(rowid) as q from query where strategyid=$stratId and learnerid=$learnerId and run=$run and fold=$fold")
      resultSet.next()
      q = resultSet.getInt("q")
      if (q > 0) {
        alreadyDone = true
        if (q < Q) {
          println("Not enough queries (" + q + ") fetched from dataset " + dbCopy + " for run=" + run + " and fold=" + fold + ". Required: " + Q)
          sys.exit(0)
        }
      }
    } catch {
      case e: Throwable => e.printStackTrace
        println("\nProblems looking for preexistence of queries in: " + dbCopy + ".")
        sys.exit(0)
    }

    //Insert Q (or all) queries.
    if (!alreadyDone) {
      val Qtaken = if (Q == Int.MinValue) Int.MaxValue else Q
      val (seq, t) = Tempo.timev(strat.timeLimitedQueries(seconds).take(Qtaken).map(_.id).toVector)
      q = seq.length
      acquire()
      var str = ""
      try {
        val statement = connection.createStatement()
        statement.executeUpdate("begin")
        seq.zipWithIndex.foreach { case (pattId, idx) =>
          str = s"insert into query values ($stratId,$learnerId,$run,$fold,$idx,$pattId)"
          statement.executeUpdate(str)
        }
        statement.executeUpdate(s"insert into time values ($stratId,$learnerId,$run,$fold,$t)")
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
    }
    q
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
  val qpatts = ALDatasets.queriesFromSQLite("/home/davi/wcs/ucipp/uci/")(d)(RandomSampling(Seq()), 0, 0) match {
    case Right(x) => x
    case Left(str) => println(s"Problema: $str"); sys.exit(0)
  }
  d.close()
  qpatts foreach println

}