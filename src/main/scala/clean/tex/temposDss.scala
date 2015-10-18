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
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.{Datasets, Tempo}

import scala.util.Random

object temposDss extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with CM {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = this.getClass.getName
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
    //    println(dss)
    println(dss.size)
    val mat = for {
      dataset <- dss
    } yield {
        val ds = Ds(dataset, readOnly = true)
        println(s"${renomeia(ds.dataset)}, ")
        ds.open()
        learnersfun(learnersStr).map { learnerfun =>
          val leaFake = learnerfun(Seq(), 42)
          val (v, t) = Tempo.timev {
            val patts0 = new Random(seed.toInt).shuffle(transpose(new Random(1 + seed).shuffle(ds.patterns).groupBy(_.label).map(_._2.toList.take(2000)).toList).flatten.take(4000))
            val patts = if (leaFake.querFiltro) criaFiltro(patts0, 0)._1 else patts0
            Datasets.kfoldCV(patts.toVector, 5, parallel = true) { (tr, testset, fold, min) =>
              val learner = learnerfun(tr, (1000 * fold) + seed.toInt)
              stratsForTempo(tr, learner) foreach { st =>
                st.queries.take(100).toList
              }
            }
          }
          println(s"tempo: $ds $leaFake $t")
          v
        }
        ds.close()
      }
  }

  def dist(a: List[Long])(b: List[Long]) = a.zip(b).map { case (x, y) => if (x == y) 1d else 0d }.sum / a.size

  def eucl(a: List[Double])(b: List[Double]) = math.sqrt(a.zip(b).map { case (x, y) => (x - y) * (x - y) }.sum)

  def spea(a: List[Double])(b: List[Double]) = new SpearmansCorrelation().correlation(a.toArray, b.toArray)
}