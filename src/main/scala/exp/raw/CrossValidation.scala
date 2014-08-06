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

import java.io.File
import java.lang.{Throwable, Exception}
import java.util.Calendar

import al.strategies.{ClusterBased, Strategy, RandomSampling}
import app.db.Dataset
import ml.Pattern
import ml.classifiers.{NoLearner, NB, Learner}
import util.{Lock, Datasets, Lazy}
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable
import scala.util.Random

/**
 * Created by davi on 05/06/14.
 */
trait CrossValidation extends Lock with ClassName {
  var fileLocked = false
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
  val datasetNames0: Seq[String]
  val rndForLock = new Random(System.currentTimeMillis() % 100000)
  val qmap = mutable.Map[(String, String), Int]()
  val memlimit = 28000
  var finished = 0
  var skiped = 0
  var available = true
  var dbToWait: Dataset = null
  val fileToStopProgram = "/tmp/safeQuit.davi"

  def ee(db: Dataset): Boolean

  lazy val datasetNames = datasetNames0.par.filter { d =>
    val db = Dataset(path, createOnAbsence = false, readOnly = true)(d)
    if (db.isLocked) println(s"${db.dbOriginal} is locked as ${db.dbLock}! Cannot open it. Skipping...")
    db.open()
    val r = ee(db)
    db.close()
    r
  }

  def strats(run: Int, pool: Seq[Pattern]) = if (parallelStrats) strats0(run, pool).par else strats0(run, pool)

  /**
   * calcula Q (média de queries necessárias para Rnd atingir acurácia máxima)
   * Verifique antes se todas Rnd queries estão presentes.
   * learner should be always NB()
   */
  def q_notCheckedIfHasAllRndQueries(db: Dataset, learner: Learner = NB()) = {
    //Pega mediana.
    lazy val Q = {
      val res = (for {
        r <- 0 until runs
        f <- 0 until folds
        sql = s"select p from (select run as r,fold as f,learnerid as l,strategyid as s,position as p,sum(value) as t from hit group by strategyid, learnerid, run, fold, position) inner join (select *,sum(value) as a from hit where expe=pred group by strategyid, learnerid, run, fold, position) on r=run and f=fold and s=strategyid and p=position and l=learnerid and r=$r and f=$f and s=1 and l=${db.fetchlid(learner)} order by a/(t+0.0) desc, p asc limit 1;"
      } yield {
        //        println(sql)
        db.exec(sql).get.head.head
      }).sorted.toList(runs * folds / 2).toInt
      println(s"Q=$res")
      res
    }

    acquire()
    val res = qmap.getOrElseUpdate((db.toString, learner.toString), Q)
    release()
    res
  }

  def run(runCore: (Dataset, Int, Int, => Seq[Pattern], => Seq[Pattern], => Standardize) => Unit) {

    //stops according to time limit or presence of quit-file
    running = true
    new Thread(new Runnable() {
      def run() {
        var reason = "nenhum"
        while (running) {
          1 to 50 takeWhile { _ =>
            Thread.sleep(100)
            running
          }
          val tmpLockingFile = new File(fileToStopProgram)
          if (tmpLockingFile.exists()) {
            running = false
            tmpLockingFile.delete()
            Thread.sleep(100)
            reason = s"$fileToStopProgram found, safe-quiting."
          } else if (Runtime.getRuntime.totalMemory() / 1000000d > memlimit) {
            running = false
            Thread.sleep(100)
            reason = s"Limite de $memlimit MB de memoria atingido."
          }
        }
        safeQuit(reason, dbToWait)
      }
    }).start()

    try {
      (if (parallelDatasets) datasetNames else datasetNames).zipWithIndex foreach { case (datasetName, idx) => //datasets cannot be parallelized anymore
        val datasetNr = idx + 1

        //Open connection to load patterns via weka SQL importer.
        //      println("Loading patterns for dataset " + datasetName + " ...")
        source(datasetName) match {
          case Right(patts) =>

            //Reopen connection to write queries.
            println("Beginning dataset " + datasetName + " ...")
            val db = dest(datasetName)
            dbToWait = db
            db.open(debug = false)

            (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
              println("    Beginning run " + run + " for " + datasetName + " ...")
              Datasets.kfoldCV(Lazy(new Random(run).shuffle(patts)), folds, parallelFolds) { case (tr0, ts0, fold, minSize) => //Esse Lazy é pra evitar shuffles inuteis (se é que alguém não usa o pool no runCore).

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

                println(Calendar.getInstance().getTime + " : Pool " + fold + " of run " + run + " iniciado for " + datasetName + s" ($datasetNr) !")
                runCore(db, run, fold, pool, testSet, f)
                println(Calendar.getInstance().getTime + " : Pool " + fold + " of run " + run + " finished for " + datasetName + s" ($datasetNr) !")

              }
              //            println("  Run " + run + " finished for " + datasetName + " !")
            }

            if (!db.readOnly) {
              db.acquire()
              db.save()
              db.release()
            }
            acquire()
            finished += 1
            release()
            println(s"Dataset (# $datasetNr) " + datasetName + " finished! (" + finished + "/" + datasetNames.length + ")\n")
            db.close()
          case Left(str) =>
            acquire()
            skiped += 1
            release()
            println(s"Skipping $datasetName ($datasetNr) because $str. $skiped datasets skiped.\n")
        }
      }
    } catch {
      case e: Throwable =>
        running = false
        println(s"Exceção inesperada:")
        e.printStackTrace()
        println(s"Exceção inesperada (just exiting to avoid zombie threads...):\n${e.getClass.getName}")
        dbToWait.acquire()
        dbToWait.hardClose()
    }

    running = false
    println("bye!")
  }

  def completeForQCalculation(db: Dataset) = if (!rndQueriesComplete(db)) {
    println("Rnd queries incomplete.")
    false
  } else if (!rndNBHitsComplete(db)) {
    println("Rnd NB hits incomplete.")
    false
  } else true

  def rndQueriesComplete(db: Dataset) = {
    val exs = db.n
    val expectedQueries = exs * (folds - 1) * runs

    //checa se as queries desse run/fold existem para Random/NoLearner
    if (db.isOpen && db.countRndStartedPools != runs * folds) {
      false

      //checa se todas as queries existem para a base
    } else {
      if (db.countRndPerformedQueries > expectedQueries) safeQuit(s"${db.countRndPerformedQueries} queries found for $db , it should be $expectedQueries", db)
      else db.countRndPerformedQueries == expectedQueries
    }
  }

  def rndNBHitsComplete(db: Dataset) = {
    val exs = db.n
    val expectedHits = exs * (folds - 1) * runs

    //checa se tabela de matrizes de confusão está completa para todos os pools inteiros para Random/NB (NB é a referência para Q)
    val hitExs = db.countPerformedConfMatrices(RandomSampling(Seq()), NB())
    if (hitExs > expectedHits) safeQuit(s"$hitExs confusion matrices of Rnd cannot be greater than $expectedHits for $db with NB", db)
    else hitExs == expectedHits
  }

  /**
   * Assumes Rnd queries are complete.
   * @param db
   * @param run
   * @param fold
   * @return
   */
  def rndNBHitsCompleteForPool(db: Dataset, run: Int, fold: Int) = {
    val expectedHits = db.countPerformedQueriesForPool(RandomSampling(Seq()), run, fold)

    //checa se tabela de matrizes de confusão está completa para este pool inteiro para Random/NB (NB é a referência para Q)
    val hitExs = db.countPerformedConfMatricesForPool(RandomSampling(Seq()), NB(), run, fold)
    if (hitExs > expectedHits) safeQuit(s"$hitExs confusion matrices of Rnd cannot be greater than $expectedHits for $db with NB for pool $run/$fold", db)
    else hitExs == expectedHits
  }

  def hitsComplete(learner: Learner)(db: Dataset) = {
    val Q = q_notCheckedIfHasAllRndQueries(db)
    strats(-1, Seq()).forall { s =>
      (0 until runs).forall { run =>
        (0 until folds).forall { fold =>
          db.countPerformedConfMatricesForPool(s, learner, run, fold) >= Q
        }
      }
    }
  }

  def nonRndQueriesComplete(db: Dataset) = {
    val exs = db.n
    val expectedQueriesForClusterBased = exs * (folds - 1) * runs

    strats(-1, Seq()).forall {
      case s: ClusterBased => db.countPerformedQueries(s) == expectedQueriesForClusterBased
      case _: RandomSampling =>
        println("Improper use of nonRndQueriesComplete with Rnd.")
        sys.exit(1)
      case s =>
        val Q = q_notCheckedIfHasAllRndQueries(db)
        (0 until runs).forall { run =>
          (0 until folds).forall { fold =>
            db.countPerformedQueriesForPool(s, run, fold) >= Q
          }
        }
    }
  }
}
