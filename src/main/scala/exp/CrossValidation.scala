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
import util.{ALDatasets, Datasets, Lazy, Lock}
import weka.core.Instances
import weka.filters.unsupervised.attribute.Standardize

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
 * Created by davi on 05/06/14.
 */
trait CrossValidation extends Lock with ClassName {
  val binarizeNominalAtts: Boolean

  def close() = Unit

  def isOpen() = false

  var fileLocked = false

  def parallelDatasets = false //args1(2).contains("d")

  def parallelRuns = args1(2).contains("r")

  def parallelFolds = args1(2).contains("f")

  def parallelStrats = args1(2).contains("s")

  def path: String

  def dest = Dataset(path) _

  val readOnly = true
  val hardClose = ()
  val args1: Array[String]

  def strats0(run: Int, pool: Seq[Pattern]): Seq[Strategy]

  val samplingSize = 500
  val timeLimitSeconds = 30 * 24 * 3600
  //lock is just to increment finished datasets counter
  val datasetNames0: Seq[String]
  val rndForLock = new Random(System.currentTimeMillis() % 100000)
  val qmap = mutable.Map[String, Int]()
  val memlimit = Source.fromFile("memlimit.txt").getLines().toList.head.toInt
  var finished = 0
  var skipped = 0
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
    if (Q == -1 && !justWarming) justQuit(s"Impossible to calculate Q properly, rnd '$this' hits for NB or 5NN or C45 are incomplete!")
    else Q
  }

  def run(runCore: (Dataset, Int, Int, => Seq[Pattern], => Seq[Pattern], => Standardize, => Map[Int, Pattern]) => Unit) {
    //stops according to memory limit or presence of quit-file
    running = true
    new Thread(new Runnable() {
      def run() {
        var reason = "nenhum"
        while (running) {
          1 to 40 takeWhile { _ =>
            Thread.sleep(100)
            running
          }
          val tmpLockingFile = new File(fileToStopProgram)
          val tmpLockingFileUnsafe = new File(fileToStopProgramUnsafe)
          if (checkExistsForNFS(tmpLockingFileUnsafe)) {
            running = false
            reason = s"$fileToStopProgramUnsafe found, unsafe-quiting."
            tmpLockingFileUnsafe.delete()
            if (dbToWait != null && dbToWait.isOpen) dbToWait.unsafeQuit(reason) else justQuit(s"Db was closed: $reason")
          } else if (checkExistsForNFS(tmpLockingFile)) {
            running = false
            reason = s"$fileToStopProgram found, safe-quiting."
            tmpLockingFile.delete()
            if (dbToWait != null && dbToWait.isOpen) {
              println("closing dbToWait")
              dbToWait.safeQuit(reason)
            } else justQuit(s"Db was closed: $reason")
          } else if (Runtime.getRuntime.totalMemory() / 1000000d > memlimit) {
            running = false
            reason = s"Limite de $memlimit MB de memoria atingido."
            if (dbToWait != null && dbToWait.isOpen) dbToWait.safeQuit(reason) else justQuit(s"Db was closed: $reason")
          }
        }
      }
    }).start()

    try {
      val lista = datasetNames0.zipWithIndex.toBuffer
      while (lista.nonEmpty && running) compilerBugSucks(runCore, lista)
      //      while (lista.nonEmpty) compilerBugSucks(runCore, lista)
    } catch {
      case e: Throwable =>
        running = false
        p(s"Exceção inesperada:")
        e.printStackTrace()
        if (dbToWait != null) dbToWait.unsafeQuit(s">>> Exceção inesperada: ${e.getMessage}") else unsafeQuit(s">>> Exceção inesperada: ${e.getMessage}")
    }
    //    if (dbToWait != null) dbToWait.running = false
    running = false
    p("bye!")
  }

  /**
   * todo: put this inside db
   * @param db
   * @return
   */
  def completeForQCalculation(db: Dataset) = if (!rndQueriesComplete(db)) {
    p("Rnd queries incomplete.")
    false
  } else if (!db.rndHitsComplete(NB()) || !db.rndHitsComplete(KNNBatchb(5, "eucl", Seq(), weighted = true)) || !db.rndHitsComplete(C45())) {
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
  //  def rndHitsCompleteForPool(db: Dataset, run: Int, fold: Int, learner: Learner) = {
  //    val expectedHits = db.countPerformedQueriesForPool(RandomSampling(Seq()), run, fold)
  //
  //    //checa se tabela de matrizes de confusão está completa para este pool inteiro para Random/learner (NB ou 5NN poderiam ser as referências para Q)
  //    val hitExs = db.countPerformedConfMatricesForPool(RandomSampling(Seq()), learner, run, fold)
  //    if (hitExs > expectedHits) justQuit(s"$hitExs confusion matrices of Rnd cannot be greater than $expectedHits for $db with $learner for pool $run/$fold")
  //    else hitExs == expectedHits
  //  }

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


  def compilerBugSucks(runCore: (Dataset, Int, Int, => Seq[Pattern], => Seq[Pattern], => Standardize, => Map[Int, Pattern]) => Unit, lista: mutable.Buffer[(String, Int)]) {
    skipped = 0
    p("Datasets restantes: " + lista.toSeq, lista)
    println("------------------------------------------------")
    val lista2 = lista.clone()
    (if (parallelDatasets) lista2.par else lista2) foreach { case (datasetName, idx) => //datasets cannot be parallelized anymore
      val datasetNr = idx + 1

      //test previous progress
      p(s"Testing dataset $datasetName ($datasetNr)", lista)
      val db0 = Dataset(path, createOnAbsence = false, readOnly = true)(datasetName)
      var resumable = false
      if (db0.isLocked()) {
        p(s"${db0.dbOriginal} is locked as ${db0.dbLock}! Cannot open it. Skipping...", lista)
        rndDelay(5, 1)
        lista.append((datasetName, idx))
        skipped += 1
      } else if (!db0.checkExistsForNFS(db0.dbOriginal)) {
        p(s"${db0.dbOriginal} not found! Cannot open it. Skipping...", lista)
        rndDelay(5, 1)
        lista.append((datasetName, idx))
        skipped += 1
      } else {
        db0.open()
        resumable = ee(db0)
        if (resumable) q(db0, justWarming = true) //warm start for Q. there is no concurrency here
        else {
          if (db0.finished) finished += 1
          else {
            lista.append((datasetName, idx))
            skipped += 1
          }
        }
        db0.close()
      }

      //process dataset
      if (resumable && !db0.isLocked(0)) {

        p("Beginning dataset " + datasetName + " ...")
        val db = dest(datasetName)
        db.open()
        dbToWait = db

        //Open connection to load patterns via weka SQL importer (from dbCopy). This is good to avoid two programs opening the same db at the same time.
        p("Loading patterns for dataset " + datasetName + " ...", lista)
        val ps = if (binarizeNominalAtts) ALDatasets.patternsFromSQLite(folderToCopyDb)(datasetName)
        else Datasets.arff(path + datasetName + ".arff")

        ps match {
          ////////////////
          case Right(patts) =>
            val pmap = if (binarizeNominalAtts) null else patts.map(p => p.id -> p).toMap
            (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
              p("    Beginning run " + run + " for " + datasetName + " ...", lista)
              Datasets.kfoldCV(Lazy(new Random(run).shuffle(patts)), folds, parallelFolds) { case (tr0, ts0, fold, minSize) => //Esse Lazy é pra evitar shuffles inuteis (se é que alguém não usa o pool no runCore).

                //z-score (if the learner is marked as 'semzscore' then it is a non-exclusively-numeric learner and can benefit from crude attributes, i.e. without filter)
                lazy val f = Datasets.zscoreFilter(tr0)

                lazy val pool = if (binarizeNominalAtts) {
                  val tr = Datasets.applyFilter(f)(tr0) //weka is unpredictable: without lazy resulting z-score values differ
                  new Random(run * 100 + fold).shuffle(tr)
                } else {
                  val tr = Datasets.applyFilter(f)(tr0)
                  new Random(run * 100 + fold).shuffle(tr0.zip(tr).sortBy(_._2.vector.toString()).map(_._1)) //changes order like would occurs inside filter and then shuffles
                }

                lazy val testSet = if (binarizeNominalAtts) {
                  val ts = Datasets.applyFilter(f)(ts0)
                  new Random(run * 100 + fold).shuffle(ts)
                } else {
                  val ts = Datasets.applyFilter(f)(ts0)
                  new Random(run * 100 + fold).shuffle(ts0.zip(ts).sortBy(_._2.vector.toString()).map(_._1)) //changes order like would occurs inside filter and then shuffles
                }

                p(" : Pool " + fold + " of run " + run + " iniciado for " + datasetName + s" ($datasetNr) !", lista)
                runCore(db, run, fold, pool, testSet, if (binarizeNominalAtts) f else null, pmap)
                p(Calendar.getInstance().getTime + " :    Pool " + fold + " of run " + run + " finished for " + datasetName + s" ($datasetNr) !", lista)
              }
            }

            if (!db.readOnly) {
              incCounter()
              db.acquireOp()
              //              Thread.sleep(100)
              println(s"Saving $db to close.")
              db.save() //não tem problema se der safequit aqui, pois não há mais threads para aguardar
              db.releaseOp()
            }
            acquire()
            finished += 1
            release()
            p(s"Dataset (# $datasetNr) " + datasetName + " finished!\n", lista)
            Thread.sleep((rnd.nextDouble() * 30).toInt)
            if (db.isOpen()) db.close()
          case Left(str) =>
            acquire()
            skipped += 1
            release()
            p(s"Skipping $datasetName ($datasetNr) because $str.\n", lista)
            rndDelay(5)
            lista.append((datasetName, idx))
        }
      }
      lista.remove(0)
    }
  }

  def p(msg: String, lista: Seq[(String, Int)] = Seq()): Unit = {
    if (lista.nonEmpty) println(s"${datasetNames0.length} total; ${lista.length} enqueued; $skipped skipped, $finished finished.")
    println(s"\n${Calendar.getInstance().getTime} $msg")
  }
}

