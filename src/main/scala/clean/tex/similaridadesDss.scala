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
import util.Datasets

import scala.util.Random

object similaridadesDss extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with CM {
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
    val mat = for {
      dataset <- dss.par
    } yield {
        val ds = Ds(dataset, readOnly = true)
        print(s"${renomeia(ds)}, ")
        ds.open()
        val preds = learnersfun(learnersStr).par.map { learnerfun =>
          val nomelea = learnerfun(Seq(), 42)
          println(s"$nomelea <- learner ")
          val patts = new Random(42).shuffle(transpose(new Random(43).shuffle(ds.patterns).groupBy(_.label).map(_._2.toList.take(500)).toList).flatten.take(100))
          val kfoldres = nomelea -> Datasets.kfoldCV(patts.toVector, 10, parallel = true) { (tr, testset, fold, min) =>
            val learner = learnerfun(tr, fold)
            val model = learner.build(tr)
            println(s"${testset.size} <- testset.size")
            if (testset.size != 10) error("testset.size != 10")
            testset map (x => model.predict(x).toLong)
          }.flatten.toList
          kfoldres
        }.toList
        val m = preds map { case (lea, a) =>
          preds.map {
            case (lea2, b) => dist(a)(b)
          }
        }
        ds.close()
        ds -> m.flatten
      }

    val matsorted = mat.toList.sortBy { case (ds, col) => renomeia(ds) }
    val dsvectors = matsorted

    val m = dsvectors map { case (lea, a) =>
      lea -> dsvectors.map {
        case (lea2, b) => (100 / (1 + eucl(a)(b))).round / 100d
      }
    }
    val sorted = m.map(_._2).sortBy(x => x.sum).transpose.sortBy(x => x.zipWithIndex.find(_._1 == 1).get._2).transpose
    val poeLea = m.map(x => x._2.sorted -> x._1).toMap
    val msorted = sorted map { x => poeLea(x.sorted) -> x }

    println(s"${m} <- m")
    println(s"${sorted} <- sorted")
    println(s"${msorted} <- msorted")

    msorted foreach { case (ds, simis) =>
      println(s"$ds ${simis.mkString(" ")}")
    }
  }

  def dist(a: List[Long])(b: List[Long]) = a.zip(b).map { case (x, y) => if (x == y) 1d else 0d }.sum / a.size

  def eucl(a: List[Double])(b: List[Double]) = math.sqrt(a.zip(b).map { case (x, y) => (x - y) * (x - y) }.sum)
}