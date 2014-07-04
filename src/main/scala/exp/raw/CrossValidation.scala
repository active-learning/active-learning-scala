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

import app.db.Dataset
import ml.Pattern
import util.{Datasets, Lock}

import scala.Right
import scala.util._

/**
 * Created by davi on 05/06/14.
 */
trait CrossValidation extends Lock with ClassName {
  //lock is just to increment finished datasets counter
  val runs: Int
  val folds: Int
  val parallelDatasets: Boolean
  val parallelRuns: Boolean
  val parallelFolds: Boolean
  val datasetNames: Seq[String]
  val source: (String) => Either[String, Stream[Pattern]]
  val dest: (String) => Dataset
  var finished = 0

  var available = true
  val rndForLock = new Random(System.currentTimeMillis() % 100000)

  def inc() {
    Thread.sleep((rndForLock.nextDouble() * 100).toInt)
    acquire()
    finished += 1
    Thread.sleep((rndForLock.nextDouble() * 100).toInt)
    release()
  }

  var running = true

  def run(runCore: (Dataset, Int, Int, Seq[Pattern], Seq[Pattern]) => Unit) {
    running = true
    new Thread(new Runnable() {
      override def run() {
        while (running) {
          Thread.sleep(1000)
          if (Runtime.getRuntime.totalMemory() / 1000000d > 29000) {
            println("Limite de 29000MB de memoria atingido.")
            sys.exit(0)
          }
        }
      }
    }).start()

    (if (parallelDatasets) datasetNames.par else datasetNames) foreach { datasetName =>
      //Open connection to load patterns.
      println("Loading patterns for dataset " + datasetName + " ...")
      val patts = source(datasetName) match {
        case Right(x) => x
        case Left(error) =>
          println(error)
          println(datasetName + " not found; probably it is in use.")
          sys.exit(0)
      }

      //Reopen connection to write queries.
      println("Beginning dataset " + datasetName + " ...")
      val db = dest(datasetName)
      db.open(debug = true)
      (if (parallelRuns) (0 until runs).par else 0 until runs) foreach { run =>
        println("  Beginning run " + run + " for " + datasetName + " ...")
        Datasets.kfoldCV(new Random(run).shuffle(patts), folds, parallelFolds) { case (tr0, ts0, fold, minSize) =>
          println("    Beginning pool " + fold + " of run " + run + " for " + datasetName + " ...")

          //z-score
          val f = Datasets.zscoreFilter(tr0)
          val tr = Datasets.applyFilter(tr0, f)
          val ts = Datasets.applyFilter(ts0, f)

          val pool = new Random(run * 100 + fold).shuffle(tr)
          lazy val testSet = new Random(run * 100 + fold).shuffle(ts) //todo: useless memory being allocated?

          runCore(db, run, fold, pool, testSet)

          println(Calendar.getInstance().getTime + " : Pool " + fold + " of run " + run + " finished for " + datasetName + " !\n Total of " + finished + s"/${datasetNames.length} datasets finished!")
        }
        println("  Run " + run + " finished for " + datasetName + " !")
        println("")
      }
      inc()
      println("Dataset " + datasetName + " finished! (" + finished + "/" + datasetNames.length + ")")
      println("")
      println("")
      if (!db.readOnly) db.save()
      db.close()
    }
    running = false
    Thread.sleep(2000)
  }
}
