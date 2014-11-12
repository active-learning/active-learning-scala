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

import al.strategies.{PassiveAcc, PassiveGme, RandomSampling}
import clean._
import ml.classifiers.{NoLearner, NB}
import util.{Stat, StatTests}

import scala.collection.mutable

object tab extends AppWithUsage with LearnerTrait with StratsTrait with MeasuresTrait {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val context = "tabtex"
  run()

  override def run() = {
    super.run()
    allMeasures(maxtimesteps).dropRight(2) foreach { measure =>
      val sl = mutable.LinkedHashSet[String]()
      //      val res = (if (parallelDatasets) datasets.toList.par else datasets.toList) map { dataset => //nao pode ser paralelo
      val res = datasets.toList map { dataset =>
        val ds = Ds(dataset)
        ds.open()
        sl += "Q/$|\\mathcal{U}|$"
        val ms = for {
          s <- (measure.id match {
            case 11 => Seq(PassiveAcc(NoLearner(), Seq()))
            case 12 => Seq(PassiveGme(NoLearner(), Seq()))
            case _ => Seq()
          }) ++ allStrats()
        } yield {

          if (s.id == 22 || s.id == 23) {
            (specialLearners() map { learner =>
              sl += s"${s.abr} ${learner.toString.take(4)}"
              val vs = for {
                r <- 0 until runs
                f <- 0 until folds
              } yield {
                if (measure.id == 0) -1d
                else ds.getMeasure(s.mea, RandomSampling(Seq()), learner, r, f) match {
                  case Some(v) => v
                  case None => -1d //ds.quit(s"No pass measure for ${(s.mea, s, learner, r, f)}(${(s.mea.id, s.id, learner.id, r, f)})!")
                }
              }
              Seq((Stat.media_desvioPadrao(vs.toVector)._1 * 100).round.toDouble -> -1d)
            }).flatten
          } else if (s.id >= 17 && s.id <= 21) {
            val learner = s.learner
            sl += s"${s.abr} ${learner.toString.take(4)}"
            if (ds.isMeasureComplete(measure, s.id, learner.id)) {
              val vs = for {
                r <- 0 until runs
                f <- 0 until folds
              } yield {
                if (measure.id == 0) -1
                else ds.getMeasure(measure, s, learner, r, f) match {
                  case Some(v) => v
                  case None => ds.quit(s"No svm/maj measure for ${(measure, s, learner, r, f)}!")
                }
              }
              val (va, d) = Stat.media_desvioPadrao(vs.toVector)
              Seq(if (s.id == 21) (va, -1d) else (va, d))
            } else Seq((-1d, -1d))
          } else {

            (learners(learnersStr) map { learner =>
              sl += s"${s.abr} ${learner.toString.take(4)}"
              if (ds.isMeasureComplete(measure, s.id, learner.id)) {
                val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
                } yield {
                  if (measure.id == 0) -1
                  else ds.getMeasure(measure, s, learner, r, f) match {
                    case Some(v) => v
                    case None => ds.quit(s"No measure for ${(measure, s, learner, r, f)}!")
                  }
                }
                Seq(Stat.media_desvioPadrao(vs.toVector))
              } else Seq((-1d, -1d))
            }).flatten
          }
        }
        val res = ds.dataset -> (Seq((maxtimesteps.toDouble, (ds.n.toDouble * 0.8).round.toDouble)) ++ ms.flatten)
        ds.close()
        res
      }
      println(s"")
      println(s"")
      println(s"")

      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(_._1) grouped 50
      //      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList grouped 50
      val tbs = res.map(x => x._1 -> x._2.padTo(sl.size, (-1d, -1d))).toList.sortBy(x => x._2.head) grouped 50
      tbs foreach { case res0 =>
        StatTests.extensiveTable2(res0.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), sl.toVector.map(_.toString), "nomeTab", measure.toString)
      }
      sl.clear()
    }
  }
}
