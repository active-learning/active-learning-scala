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

import app.db.{Dataset, Lock}
import ml.Pattern
import util.Datasets

import scala.Right
import scala.util._

/**
 * Created by davi on 05/06/14.
 */
trait CrossValidation extends Lock with ClassName {
  //lock is just to increment finished datasets counter
  val parallelDatasets: Boolean
  val parallelRuns: Boolean
  val parallelFolds: Boolean
  val datasetNames: Seq[String]
  val source: (String) => Either[String, Stream[Pattern]]
  val dest: (String) => Dataset
  var finished = 0

  def runCore(db: Dataset, run: Int, fold: Int, pool: Seq[Pattern], testSet: => Seq[Pattern])

  var available = true
  val rnd = new Random(0)

  def inc() {
    Thread.sleep((rnd.nextDouble() * 100).toInt)
    acquire()
    finished += 1
    Thread.sleep((rnd.nextDouble() * 100).toInt)
    release()
  }

  def run() {
    (if (parallelDatasets) datasetNames.par else datasetNames) foreach { datasetName =>
      //Open connection to load patterns.
      println("Loading patterns for dataset " + datasetName + " ...")
      val patts = source(datasetName) match {
        case Right(x) => x
        case Left(error) => println(error); sys.exit(0)
      }

      //Reopen connection to write queries.
      println("Beginning dataset " + datasetName + " ...")
      val db = dest(datasetName)
      db.open(debug = true)
      (if (parallelRuns) (0 until 5).par else 0 until 5) foreach { run =>
        println("  Beginning run " + run + " for " + datasetName + " ...")
        Datasets.kfoldCV(new Random(run).shuffle(patts), 5, parallelFolds) { case (tr0, ts0, fold, minSize) =>
          println("    Beginning pool " + fold + " of run " + run + " for " + datasetName + " ...")

          //z-score
          val f = Datasets.zscoreFilter(tr0)
          val tr = Datasets.applyFilter(tr0, f)
          val ts = Datasets.applyFilter(ts0, f)

          val pool = new Random(run * 100 + fold).shuffle(tr)
          lazy val testSet = new Random(run * 100 + fold).shuffle(ts)

          runCore(db, run, fold, pool, testSet)

          println("    Pool " + fold + " of run " + run + " finished for " + datasetName + " ! Total of " + finished + " datasets finished!")
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
  }
}
