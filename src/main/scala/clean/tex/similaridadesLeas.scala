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

import clean.lib._
import ml.classifiers.NoLearner
import util.Datasets

import scala.util.Random

object similaridadesLeas extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with CM {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = this.getClass.getName
  val measure = Kappa
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
    val dss = datasets
    println(dss)
    println(dss.size)
    val mat = learnersfun(learnersStr).par map { learnerfun =>
      val nomelea = learnerfun(Seq(), 42)
      println(s"$nomelea <- learner ")
      val preds = for {
        dataset <- dss.par //.take(2)
      } yield {
          val ds = Ds(dataset, readOnly = true)
          print(s"${renomeia(ds)}, ")
          ds.open()
          val kfoldres = ((1 to 10) map { run =>
            val patts = new Random(run + seed).shuffle(transpose(new Random(run + 1 + seed).shuffle(ds.patterns).groupBy(_.label).map(_._2.toList.take(500)).toList).flatten.take(math.max(100, math.min(1000, ds.patterns.size / 10))))
            Datasets.kfoldCV(patts.toVector, 10, parallel = true) { (tr, testset, fold, min) =>
              val learner = learnerfun(tr, (100 * fold) + run + seed.toInt)
              val model = learner.build(tr)
              println(s"${testset.size} <- testset.size")
              if (testset.size != 10) error("testset.size != 10")
              //            Random.nextInt(2).toDouble +:
              testset map model.predict
            }
          }).flatten
          ds.close()
          ds -> kfoldres.flatten.toList
        }
      nomelea -> preds.toList.sortBy { case (ds, col) => renomeia(ds) }
    }

    val matsorted = mat.toList.sortBy { case (lea, col) => lea.limpa }
    val dsvectors = matsorted map { case (lea, row) =>
      lea -> row.map { case (ds, col) => col }.flatten
    }

    val m = dsvectors.zipWithIndex map { case ((lea, a), i) =>
      lea -> dsvectors.zipWithIndex.map {
        case ((ds, b), i2) => (dist(a)(b) * 100).round
      }
    }
    val sorted = m.map(_._2).sortBy(x => x.sum).transpose.sortBy(x => x.zipWithIndex.find(_._1 == 100).get._2).transpose
    val poeLea = m.map(x => x._2.sorted -> x._1).toMap
    val msorted = sorted map { x => poeLea(x.sorted) -> x }

    println(s"${m} <- m")
    println(s"${sorted} <- sorted")
    println(s"${msorted} <- msorted")

    msorted foreach { case (lea, simis) =>
      println(s"$lea ${simis.mkString(" ")}")
    }
  }

  def dist(a: List[Double])(b: List[Double]) = a.zip(b).map { case (x, y) => if (x == y) 1d else 0d }.sum / a.size
}