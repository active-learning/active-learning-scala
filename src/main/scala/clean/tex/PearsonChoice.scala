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

package clean.tex

import java.io.PrintWriter

import al.strategies._
import clean.lib._
import ml.classifiers.{KNNBatcha, NoLearner}
import util.{Datasets, Stat}

object PearsonChoice extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with CM {
  lazy val arguments = superArguments
  //++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = "PearsonChoice"
  val measure = Kappa
  val conf = "artigos/bracis15"
  //"hais14-expandido"
  run()

  override def run() = {
    super.run()
    val dss = DsBy(datasets, 200, onlyBinaryProblems = false, notBinary = true)
    println(dss.size)
    val winners = for {
      dataset <- dss.take(5).par
    } yield {
        val ds = Ds(dataset, readOnly = true)
        println(s"$ds ${ds.nclasses}")
        ds.open()
        val patts = ds.patterns
        /*
        @book{evans1996straightforward,
          title={Straightforward statistics for the behavioral sciences},
          author={Evans, James D},
          year={1996},
          publisher={Brooks/Cole}
        }
         .00-.19 “very weak”
         .20-.39 “weak”
         .40-.59 “moderate”
         .60-.79 “strong”
         .80-1.0 “very strong”
         */
        //        Seq(0.8000,0.9000,0.9500,0.9900,0.9950,0.9990,0.9995,0.9999)
        //        Seq(0.80, 0.81, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.90, 0.99, 0.999, 0.9999, 0.99999, 0.999999)
        val accs = Seq(0.9000, 0.9900, 0.9990, 0.9999, 0.99999, 0.999999).zipWithIndex map { case (pearson, idx) =>
          val accs = Datasets.kfoldCV(patts.toVector, 2, parallel = true) { (pool, testset, fold, min) =>
            val learner = KNNBatcha(5, "eucl", pool, weighted = true)
            val strat = HTUFixo(pool, learner, pool, "eucl", 1, 1, debug = false, pearson)
            val queries = strat.queries.take(100)
            val model = learner.build(queries)
            kappa(model.confusion(testset))
          }
          val acc = accs.sum / accs.size
          println(dataset + " " + pearson + " " + acc)
          pearson -> acc
        }
        accs.maxBy(_._2)._1
      }

    println(s"${winners.groupBy(identity).toList.sortBy(_._2.size).map(_._1)} <- hist")
  }
}
