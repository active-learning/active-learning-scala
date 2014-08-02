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

package exp.raw

import java.util.Calendar

import al.strategies.RandomSampling
import app.db.Dataset
import ml.Pattern
import ml.classifiers.Learner
import util.{Lock, Datasets, Lazy}
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable
import scala.util.Random

/**
 * Created by davi on 05/06/14.
 */
trait CrossValidation extends Lock with ClassName {
  lazy val parallelDatasets = args1(2).contains("d")
  lazy val parallelRuns = args1(2).contains("r")
  lazy val parallelFolds = args1(2).contains("f")
  lazy val parallelStrats = args1(2).contains("s")
  lazy val source = Datasets.patternsFromSQLiteFullPath _
  val args1: Array[String]
  val dest: (String) => Dataset
  val samplingSize = 500

  val timeLimitSeconds = 3 * 3600
  val runs = 5
  val folds = 5

  //lock is just to increment finished datasets counter
  val path: String
  val datasetNames: Seq[String]
  val rndForLock = new Random(System.currentTimeMillis() % 100000)
  val qmap = mutable.Map[(String, String), Int]()
  val memlimit = 28000
  var finished = 0
  var available = true
  var running = true
  var dbToWait: Dataset = null

  /**
   * calcula Q (média de queries necessárias para Rnd atingir acurácia máxima)
   */
  def q(db: Dataset, learner: Learner) = {
    lazy val Q = (for {
      r <- 0 until runs
      f <- 0 until folds
      sql = s"select p from (select run as r,fold as f,learnerid as l,strategyid as s,position as p,sum(value) as t from hit group by strategyid, learnerid, run, fold, position) inner join (select *,sum(value) as a from hit where expe=pred group by strategyid, learnerid, run, fold, position) on r=run and f=fold and s=strategyid and p=position and l=learnerid and r=$r and f=$f and s=1 and l=${db.fetchlid(learner)} order by a/(t+0.0) desc, p asc limit 1;"
    } yield db.exec(sql) match {
        case Right(queue) => queue.head.head
        case Left(str) => safeQuit(s"Problems calculating Q: $str")
      }).sorted.toList(runs * folds / 2).toInt
    acquire()
    val res = qmap.getOrElseUpdate((db.toString, learner.toString), Q)
    release()
    println(s"Q=$res")
    res
  }

  def run(runCore: (Dataset, Int, Int, => Seq[Pattern], => Seq[Pattern], => Standardize) => Unit) {
    running = true
    new Thread(new Runnable() {
      override def run() {
        while (running) {
          Thread.sleep(1500)
          if (Runtime.getRuntime.totalMemory() / 1000000d > memlimit) {
            Thread.sleep(1000)
            safeQuit(s"Limite de $memlimit MB de memoria atingido.", dbToWait)
          }
        }
      }
    }).start()

    (if (parallelDatasets) datasetNames.par else datasetNames) foreach { datasetName =>

      //Reopen connection to write queries.
      println("Beginning dataset " + datasetName + " ...")
      val db = dest(datasetName)
      dbToWait = db
      db.open(debug = true)

      lazy val patts = {
        //Open connection to load patterns.
        println("Loading patterns for dataset " + datasetName + " ...")
        source(db.dbLock.toString)
      }

      (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
        Datasets.kfoldCV(Lazy(new Random(run).shuffle(patts.value)), folds, parallelFolds) { case (tr0, ts0, fold, minSize) =>
          println("    Beginning pool " + fold + " of run " + run + " for " + datasetName + " ...")

          //z-score
          lazy val f = Datasets.zscoreFilter(tr0)
          lazy val pool = {
            val tr = Datasets.applyFilterChangingOrder(tr0, f)
            new Random(run * 100 + fold).shuffle(tr)
          }
          lazy val testSet = {
            val ts = Datasets.applyFilterChangingOrder(ts0, f)
            new Random(run * 100 + fold).shuffle(ts)
          }
          println(s"    data standardized for run $run and fold $fold.")

          runCore(db, run, fold, pool, testSet, f)

          println(Calendar.getInstance().getTime + " : Pool " + fold + " of run " + run + " finished for " + datasetName + " !\n Total of " + finished + s"/${datasetNames.length} datasets finished!")
        }
        println("  Run " + run + " finished for " + datasetName + " !")
        println("")
      }
      inc()
      println("Dataset " + datasetName + " finished! (" + finished + "/" + datasetNames.length + ")")
      println("")
      println("")
      if (!db.readOnly) {
        db.acquire()
        db.save()
        db.release()
      }
      db.close()
    }
    running = false
    Thread.sleep(20)
  }

  def inc() {
    Thread.sleep((rndForLock.nextDouble() * 10).toInt)
    acquire()
    finished += 1
    Thread.sleep((rndForLock.nextDouble() * 10).toInt)
    release()
  }

  def checkRndQueriesAndHitsCompleteness(learner: Learner, db: Dataset, pool: Seq[Pattern], run: Int, fold: Int, testSet: Seq[Pattern], f: Standardize) = {
    //checa se as queries desse run/fold existem para Random/NoLearner
    if (db.isOpen && db.rndCompletePools != runs * folds) {
      println(s"Random Sampling query set of sequences incomplete, found ${db.rndCompletePools}, but ${runs * folds} expected. Skipping dataset $db for fold $fold of run $run (and all other pools).")
      false
    } else {
      //se tabela de matrizes de confusão estiver incompleta para o pool inteiro para Random/learner, retoma ela
      val nc = pool.head.nclasses
      val n = pool.length
      val nn = (nc * nc * nc + db.countHits(RandomSampling(Seq()), learner, run, fold)) / (nc * nc)
      if (nn > n) safeQuit(s"$nn confusion matrices should be lesser than $n for run $run fold $fold for $db", db)
      else if (nn < n) {
        println(s"Completing Rnd hits (found $nn of $n for run $run and fold $fold) ...")
        db.saveHits(RandomSampling(Seq()), learner, run, fold, nc, f, testSet)
        println(s"Run again to continue this pool (run $run and fold $fold) for other stategies!")
        false
      } else {
        //queries and hits complete for Rnd for current pool, testing hits for all pools...
        val queries = db.rndCompletePerformedQueries
        val hits = (db.rndCompleteHits(learner) + folds * runs * nc * nc * nc) / (nc.toDouble * nc)
        if (hits != queries) print(s"This pool is ok (run $run and fold $fold), but ... ")
        if (hits > queries) safeQuit(s" Expected $queries Rnd tuples in table 'hit' for $db, but excessive $hits found!", db)
        else if (hits < queries) {
          println(s" All pools have to be 'hit' by Rnd/${learner} to continue. Re-run to complete Rnd Hits.")
          false
        } else true
      }
    }
  }

  case class Pool()

}
