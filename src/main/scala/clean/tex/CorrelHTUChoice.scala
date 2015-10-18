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
import ml.classifiers.{RF, KNNBatcha, NoLearner}
import util.{Datasets, Stat}

import scala.util.Random

object CorrelHTUChoice extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with CM {
  lazy val arguments = superArguments
  //++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = "PearsonChoice"
  val measure = Kappa
  val conf = "artigos/bracis15"
  //"hais14-expandido"
  run()

  def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
    case Nil => Nil
    case ys: List[List[A]] => ys.map {
      _.head
    } :: transpose(ys.map {
      _.tail
    })
  }

  override def run() = {
    super.run()
    val dss = datasets //DsBy(datasets, 200, onlyBinaryProblems = false, notBinary = true)
    println(dss)
    println(dss.size)
    val nums = Seq[Double](-0.999999d, -0.99999, -0.9999, -0.999, -0.99, -0.9, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 0.9000, 0.9900, 0.9990, 0.9999, 0.99999, 0.999999)
    val ranks = for {
      dataset <- dss.take(3000).par
    } yield {
        val ds = Ds(dataset, readOnly = true)
        print(s"${renomeia(ds.dataset)}, ")
        ds.open()
        /*       .00-.19 “very weak”
                 .20-.39 “weak”
                 .40-.59 “moderate”
                 .60-.79 “strong”
                 .80-1.0 “very strong”         */
        //        val accs = (-9999 to 9999 by 100).map(x => x / 10000d).zipWithIndex map { case (pearson, idx) =>
        //        val accs = ((1 to 10) map { run =>
        val patts = Random.shuffle(transpose(Random.shuffle(ds.patterns).groupBy(_.label).map(_._2.toList.take(500)).toList).flatten.take(1000))
        val accs = nums.zipWithIndex map { case (correl, idx) =>
          val kappas = Datasets.kfoldCV(patts.toVector, 10, parallel = true) { (pool, testset, fold, min) =>
            val learner = RF(Random.nextInt(1000000))
            //            val learner = KNNBatcha(5, "eucl", pool, weighted = true)
            val strat = HTUFixoSpea(pool, learner, pool, "eucl", 1, 1, debug = false, correl)
            val queries = strat.queries.take(100)
            val model = learner.build(queries)
            kappa(model.confusion(testset))
          }
          val acc = kappas.sum / kappas.size
          println(ds.n + " " + dataset + " " + correl + " " + acc)
          acc
        }
        //        }).flatten
        ds.close()
        ranqueia(accs)
      }

    nums.zip(ranks.transpose.map(x => Stat.media_desvioPadrao(x.toVector))) foreach println
  }
}
/*
(-0.999999,(9.966666666666667,5.6044200243668225))
(-0.99999,(12.733333333333333,4.624726504140027))
(-0.9999,(7.266666666666667,5.2977982848223535))
(-0.999,(8.866666666666667,4.588287365933062))
(-0.99,(8.766666666666667,4.296454795470774))
(-0.9,(7.166666666666667,5.002380385757317))
(-0.8,(10.3,5.0737419271044075))
(-0.5,(11.866666666666667,4.7714428286071575))
(-0.2,(9.9,6.740601923101103))
(0.0,(11.066666666666666,5.081010398204934))
(0.2,(8.166666666666666,5.300494136893097))
(0.5,(10.333333333333334,5.665265933320173))
(0.8,(10.633333333333333,5.173651192984183))
(0.9,(9.6,4.649116659569398))
(0.99,(9.166666666666666,6.040537661342344))
(0.999,(9.9,4.943249365115449))
(0.9999,(12.2,4.802529095620943))
(0.99999,(10.333333333333334,5.987089284054322))
(0.999999,(11.766666666666667,5.070455976490064))
 */
