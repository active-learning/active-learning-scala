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

import scala.util.Random

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
    val ranks = for {
      dataset <- dss.take(3000).par
    } yield {
        val ds = Ds(dataset, readOnly = true)
        println(s"$ds ${ds.nclasses}")
        ds.open()
        val patts = Random.shuffle(ds.patterns) //.take(1000)
        /*       .00-.19 “very weak”
                 .20-.39 “weak”
                 .40-.59 “moderate”
                 .60-.79 “strong”
                 .80-1.0 “very strong”         */
        val accs = (-9999 to 9999 by 10).map(x => x / 10000d).zipWithIndex map { case (pearson, idx) =>
            //        val accs = Seq(0.9000, 0.9900, 0.9990, 0.9999, 0.99999, 0.999999).zipWithIndex map { case (pearson, idx) =>
            val kappas = Datasets.kfoldCV(patts.toVector, 10, parallel = true) { (pool, testset, fold, min) =>
            val learner = KNNBatcha(5, "eucl", pool, weighted = true)
            val strat = HTUFixo(pool, learner, pool, "eucl", 1, 1, debug = false, pearson)
            val queries = strat.queries.take(100)
            val model = learner.build(queries)
            kappa(model.confusion(testset))
          }
          val acc = kappas.sum / kappas.size
          println(dataset + " " + pearson + " " + acc)
          -acc
        }
        ranqueia(accs)
      }

    println(ranks.transpose.map(x => Stat.media_desvioPadrao(x.toVector)))
  }
}

/*
ParVector((2.8333333333333335,2.228601953392904), (3.8333333333333335,1.602081978759722), (2.9166666666666665,1.241638702145945), (3.25,1.036822067666386), (4.166666666666667,1.4719601443879744), (4.0,1.2649110640673518))
ParVector((5.833333333333333,1.7511900715418263), (5.833333333333333,1.7511900715418263), (5.833333333333333,1.7511900715418263), (5.833333333333333,1.7511900715418263), (5.833333333333333,1.7511900715418263), (5.833333333333333,1.7511900715418263), (6.416666666666667,1.3570801990548187), (7.666666666666667,1.4023789311975088), (9.666666666666666,3.459287017098562), (7.833333333333333,4.445971959725642), (8.75,4.937104414532874), (8.333333333333334,5.173651192984183), (9.583333333333334,5.024108544474997), (13.666666666666666,0.8755950357709132), (13.083333333333334,1.6857243744653712))
 */