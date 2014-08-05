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

import java.lang.{Throwable, Exception}
import java.util.Calendar

import al.strategies.{Strategy, RandomSampling}
import app.db.Dataset
import ml.Pattern
import ml.classifiers.{NB, Learner}
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
  val path: String
  lazy val source = Datasets.patternsFromSQLite(path) _

  def dest = Dataset(path) _

  val readOnly = true
  val hardClose = ()
  val args1: Array[String]

  def strats0(run: Int, pool: Seq[Pattern]): Seq[Strategy]

  val samplingSize = 500
  val timeLimitSeconds = 3 * 3600
  val runs = 5
  val folds = 5
  //lock is just to increment finished datasets counter
  val datasetNames: Seq[String]
  val rndForLock = new Random(System.currentTimeMillis() % 100000)
  val qmap = mutable.Map[(String, String), Int]()
  val memlimit = 28000
  var finished = 0
  var skiped = 0
  var available = true
  var running = true
  var dbToWait: Dataset = null

  def strats(run: Int, pool: Seq[Pattern]) = if (parallelStrats) strats0(run, pool).par else strats0(run, pool)

  /**
   * calcula Q (média de queries necessárias para Rnd atingir acurácia máxima)
   */
  def q(db: Dataset, learner: Learner) = {
    //Pega mediana.
    lazy val Q = (for {
      r <- 0 until runs
      f <- 0 until folds
      sql = s"select p from (select run as r,fold as f,learnerid as l,strategyid as s,position as p,sum(value) as t from hit group by strategyid, learnerid, run, fold, position) inner join (select *,sum(value) as a from hit where expe=pred group by strategyid, learnerid, run, fold, position) on r=run and f=fold and s=strategyid and p=position and l=learnerid and r=$r and f=$f and s=1 and l=${db.fetchlid(learner)} order by a/(t+0.0) desc, p asc limit 1;"
    } yield {
      db.exec(sql).get.head.head
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

    (if (parallelDatasets) datasetNames.par else datasetNames).zipWithIndex foreach { case (datasetName, idx) =>
      val datasetNr = idx + 1

      //Open connection to load patterns via weka SQL importer.
      //      println("Loading patterns for dataset " + datasetName + " ...")
      source(datasetName) match {
        case Right(patts) =>

          //Reopen connection to write queries.
          println("Beginning dataset " + datasetName + " ...")
          val db = dest(datasetName)
          dbToWait = db
          db.open(debug = true)

          (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
            Datasets.kfoldCV(Lazy(new Random(run).shuffle(patts)), folds, parallelFolds) { case (tr0, ts0, fold, minSize) => //Esse Lazy é pra evitar shuffles inuteis (se é que alguém não usa o pool no runCore).
              println("    Beginning pool " + fold + " of run " + run + " for " + datasetName + " ...")

              //z-score
              lazy val f = Datasets.zscoreFilter(tr0)
              lazy val pool = {
                val tr = Datasets.applyFilterChangingOrder(tr0, f)
                val res = new Random(run * 100 + fold).shuffle(tr)
                //                println(s"    data standardized for run $run and fold $fold.")
                res
              }
              lazy val testSet = {
                val ts = Datasets.applyFilterChangingOrder(ts0, f)
                new Random(run * 100 + fold).shuffle(ts)
              }

              runCore(db, run, fold, pool, testSet, f)

              println(Calendar.getInstance().getTime + " : Pool " + fold + " of run " + run + " finished for " + datasetName + s" ($datasetNr) !\n Total of " + finished + s"/${datasetNames.length} datasets finished!")
            }
            //            println("  Run " + run + " finished for " + datasetName + " !")
            println("")
          }

          if (!db.readOnly) {
            db.acquire()
            db.save()
            db.release()
          }
          acquire()
          finished += 1
          release()
          println(s"Dataset ($datasetNr)" + datasetName + " finished! (" + finished + "/" + datasetNames.length + ")")
          println("")
          db.close()
        case Left(str) =>
          acquire()
          skiped += 1
          release()
          println(s"Skipping $datasetName ($datasetNr) because $str. $skiped datasets skiped.")
          println("")
      }
    }
    running = false
    Thread.sleep(20)
  }

  def completeForQCalculation(dataset: String) = {
    val db = Dataset(path, createOnAbsence = false, readOnly = true)(dataset)
    db.open()
    val exs = db.n
    val expectedQueries = exs * (folds - 1) * runs

    //checa se as queries desse run/fold existem para Random/NoLearner
    val res = if (db.isOpen && db.rndInAllPools != runs * folds) {
      println(s"Random Sampling query set of sequences incomplete, " +
        s"found ${db.rndInAllPools}, but ${runs * folds} expected. Skipping dataset $db .")
      false

      //checa se todas as queries existem para a base
    } else if (db.rndPerformedQueries > expectedQueries) safeQuit(s"${db.rndPerformedQueries} queries found for $db , it should be $expectedQueries", db)
    else if (db.rndPerformedQueries < expectedQueries) {
      println(s"Random Sampling queries incomplete, " +
        s"found ${db.rndPerformedQueries}, but $expectedQueries expected. Skipping dataset $db .")
      false
    } else {

      //checa se tabela de matrizes de confusão está completa para todos os pools inteiros para Random/NB (NB é a referência para Q)
      val hitExs = db.countPerformedConfMatrices(RandomSampling(Seq()), NB())
      if (hitExs > expectedQueries) safeQuit(s"$hitExs confusion matrices should be lesser than $exs for $db with NB", db)
      else if (hitExs < exs) {
        println(s"Rnd hits incomplete for $db with NB (found $hitExs of $exs).")
        false
      } else true
    }
    db.close()
    res
  }

  def complete(learner: Learner)(dataset: String) = {
    val db = Dataset(path, createOnAbsence = false, readOnly = true)(dataset)
    db.open()
    val Q = q(db, NB())
    val res = strats(-1, Seq()).forall { s =>
      (0 until runs).forall { run =>
        (0 until folds).forall { fold =>
          db.countPerformedConfMatricesForPool(s, learner, run, fold) >= Q
        }
      }
    }
    db.close()
    res
  }
}
