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

package exp

import java.io.File
import java.util.Calendar

import al.strategies.{ClusterBased, RandomSampling, Strategy}
import app.db.ClassName
import app.db.entities.Dataset
import ml.Pattern
import ml.classifiers._
import util.{Datasets, Lazy, Lock}
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable
import scala.util.Random

/**
 * Created by davi on 05/06/14.
 */
trait CrossValidation extends Lock with ClassName {
  val waitingForDBAvailability = 15000
  val debug = false

  def close() = Unit

  def isOpen() = false

  var fileLocked = false

  def parallelDatasets = false //args1(2).contains("d")

  def parallelRuns = args1(2).contains("r")

  def parallelFolds = args1(2).contains("f")

  def parallelStrats = args1(2).contains("s")

  def path: String

  lazy val source = Datasets.patternsFromSQLite(path) _

  def dest = Dataset(path) _

  val readOnly = true
  val hardClose = ()
  val args1: Array[String]

  def strats0(run: Int, pool: Seq[Pattern]): Seq[Strategy]

  val samplingSize = 500
  val timeLimitSeconds = 5 * 3600
  //lock is just to increment finished datasets counter
  val datasetNames0: Seq[String]
  val rndForLock = new Random(System.currentTimeMillis() % 100000)
  val qmap = mutable.Map[String, Int]()
  val memlimit = 28000
  var finished = 0
  var skiped = 0
  var available = true
  var dbToWait: Dataset = null
  val fileToStopProgram = "/tmp/safeQuit.davi"

  def ee(db: Dataset): Boolean

  def strats(run: Int, pool: Seq[Pattern]) = if (parallelStrats) strats0(run, pool).par else strats0(run, pool)

  /**
   * calcula Q (média de queries necessárias para Rnd atingir acurácia máxima)
   * learner should be NB() and 5NN()
   */
  def q(db: Dataset, justWarming: Boolean = false) = {
    val Q = qmap.getOrElseUpdate(db.toString, db.Q)
    if (Q == -1 && !justWarming) justQuit(s"Impossible to calculate Q properly, rnd '$this' hits for NB or 5NN are incomplete!")
    else Q
  }

  def run(runCore: (Dataset, Int, Int, => Seq[Pattern], => Seq[Pattern], => Standardize) => Unit) {
    //stops according to memory limit or presence of quit-file
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
          val tmpLockingFileUnsafe = new File(fileToStopProgramUnsafe)
          if (tmpLockingFileUnsafe.exists()) {
            reason = s"$fileToStopProgramUnsafe found, unsafe-quiting."
            tmpLockingFileUnsafe.delete()
            if (dbToWait != null && dbToWait.isOpen) dbToWait.unsafeQuit(reason) else justQuit(s"Db was closed: $reason")
            running = false
          } else if (tmpLockingFile.exists()) {
            reason = s"$fileToStopProgram found, safe-quiting."
            tmpLockingFile.delete()
            if (dbToWait != null && dbToWait.isOpen) dbToWait.safeQuit(reason) else justQuit(s"Db was closed: $reason")
            running = false
          } else if (Runtime.getRuntime.totalMemory() / 1000000d > memlimit) {
            reason = s"Limite de $memlimit MB de memoria atingido."
            if (dbToWait != null && dbToWait.isOpen) dbToWait.unsafeQuit(reason) else justQuit(s"Db was closed: $reason")
            running = false
          }
        }
      }
    }).start()

    try {
      val lista = datasetNames0.zipWithIndex.toBuffer
      while (lista.nonEmpty) compilerBugSucks(runCore, lista)
    } catch {
      case e: Throwable =>
        running = false
        p(s"Exceção inesperada:")
        e.printStackTrace()
        justQuit(s"Exceção inesperada:")
    }

    running = false
    p("bye!")
  }

  def completeForQCalculation(db: Dataset) = if (!rndQueriesComplete(db)) {
    p("Rnd queries incomplete.")
    false
  } else if (!db.rndHitsComplete(NB()) || !db.rndHitsComplete(KNNBatch(5, "eucl", Seq(), "", weighted = true)) || !db.rndHitsComplete(C45())) {
    p("Rnd NB, 5NN or C45 hits incomplete.")
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
      if (db.countRndPerformedQueries > expectedQueries) justQuit(s"${db.countRndPerformedQueries} queries found for $db , it should be $expectedQueries")
      else db.countRndPerformedQueries == expectedQueries
    }
  }

  /**
   * Assumes Rnd queries are complete.
   * @param db
   * @param run
   * @param fold
   * @return
   */
  def rndHitsCompleteForPool(db: Dataset, run: Int, fold: Int, learner: Learner) = {
    val expectedHits = db.countPerformedQueriesForPool(RandomSampling(Seq()), run, fold)

    //checa se tabela de matrizes de confusão está completa para este pool inteiro para Random/learner (NB ou 5NN poderiam ser as referências para Q)
    val hitExs = db.countPerformedConfMatricesForPool(RandomSampling(Seq()), learner, run, fold)
    if (hitExs > expectedHits) justQuit(s"$hitExs confusion matrices of Rnd cannot be greater than $expectedHits for $db with $learner for pool $run/$fold")
    else hitExs == expectedHits
  }

  def hitsComplete(learner: Learner)(db: Dataset) = {
    val Q = q(db)
    strats(-1, Seq()).forall { s =>
      (0 until runs).forall { run =>
        (0 until folds).forall { fold =>
          val tmp = db.countPerformedConfMatricesForPool(s, learner, run, fold)
          if (db.debug) p(s"$s / $learner $run.$fold : $tmp >= $Q")
          tmp >= Q
        }
      }
    }
  }

  def nonRndQueriesComplete(db: Dataset) = {
    val exs = db.n
    val expectedQueriesForClusterBased = exs * (folds - 1) * runs
    var strat = ""
    var learner = ""
    val r = strats(-1, Seq()).forall {
      case s: ClusterBased => db.countPerformedQueries(s) == expectedQueriesForClusterBased
      case s: RandomSampling => rndQueriesComplete(db)
      case s =>
        val Q = q(db)
        (0 until runs).forall { run =>
          (0 until folds).forall { fold =>
            strat = s.toString
            learner = s.learner.toString
            val tmp = db.countPerformedQueriesForPool(s, run, fold)
            tmp >= Q
          }
        }
    }
    if (!r) p(s"$strat / $learner queries incomplete for $db")
    r
  }


  def compilerBugSucks(runCore: (Dataset, Int, Int, => Seq[Pattern], => Seq[Pattern], => Standardize) => Unit, lista: mutable.Buffer[(String, Int)]) {
    skiped = 0
    p("Datasets restantes: " + lista.toSeq, lista)
    println("------------------------------------------------")
    val lista2 = lista.clone()
    (if (parallelDatasets) lista2.par else lista2) foreach { case (datasetName, idx) => //datasets cannot be parallelized anymore
      val datasetNr = idx + 1

      //test previous progress
      p(s"Testing dataset $datasetName ($datasetNr)", lista)
      val db = Dataset(path, createOnAbsence = false, readOnly = true)(datasetName)
      var incomplete = false
      if (db.isLocked) {
        p(s"${db.dbOriginal} is locked as ${db.dbLock}! Cannot open it. Skipping...", lista)
        Thread.sleep(waitingForDBAvailability)
        lista.append((datasetName, idx))
        skiped += 1
      } else {
        db.open(debug)
        incomplete = ee(db)
        if (incomplete) {
          lista.append((datasetName, idx))
          skiped += 1
          q(db, justWarming = true)
        } //warm start for Q. there is no concurrency here
        else finished += 1
        db.close()
      }

      //process dataset
      if (incomplete) {

        //Open connection to load patterns via weka SQL importer.
        p("Loading patterns for dataset " + datasetName + " ...", lista)
        source(datasetName) match {
          case Right(patts) =>

            //            p("Beginning dataset " + datasetName + " ...")
            val db = dest(datasetName)
            dbToWait = db
            db.open(debug)

            (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
              p("    Beginning run " + run + " for " + datasetName + " ...", lista)
              Datasets.kfoldCV(Lazy(new Random(run).shuffle(patts)), folds, parallelFolds) { case (tr0, ts0, fold, minSize) => //Esse Lazy é pra evitar shuffles inuteis (se é que alguém não usa o pool no runCore).

                //z-score
                lazy val f = Datasets.zscoreFilter(tr0)
                lazy val pool = {
                  val tr = Datasets.applyFilterChangingOrder(tr0, f) //weka is unpredictable: without lazy resulting z-score values differ
                  val res = new Random(run * 100 + fold).shuffle(tr)
                  //                p(s"    data standardized for run $run and fold $fold.")
                  res
                }
                lazy val testSet = {
                  val ts = Datasets.applyFilterChangingOrder(ts0, f)
                  new Random(run * 100 + fold).shuffle(ts)
                }

                p(" : Pool " + fold + " of run " + run + " iniciado for " + datasetName + s" ($datasetNr) !", lista)
                runCore(db, run, fold, pool, testSet, f)
                p(Calendar.getInstance().getTime + " :    Pool " + fold + " of run " + run + " finished for " + datasetName + s" ($datasetNr) !", lista)

              }
              //            p("  Run " + run + " finished for " + datasetName + " !")
            }

            if (!db.readOnly) {
              incCounter()
              db.acquireOp()
              db.save() //não tem problema se der safequit aqui, pois não há mais threads para aguardar
              db.releaseOp()
            }
            acquire()
            finished += 1
            release()
            p(s"Dataset (# $datasetNr) " + datasetName + " finished!\n", lista)
            Thread.sleep(10)
            if (db.isOpen) db.close()
          case Left(str) =>
            acquire()
            skiped += 1
            release()
            p(s"Skipping $datasetName ($datasetNr) because $str.\n", lista)
            Thread.sleep(waitingForDBAvailability)
            lista.append((datasetName, idx))
        }
      }
      lista.remove(0)
    }
  }

  def p(msg: String, lista: Seq[(String, Int)] = Seq()): Unit = {
    if (lista.nonEmpty) println(s"${datasetNames0.length} total; ${lista.length} enqueued; $skiped skipped, $finished finished.")
    println(s"\n${Calendar.getInstance().getTime} $msg")
  }
}
