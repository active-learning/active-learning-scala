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

import al.strategies._
import clean.{StratsTrait, LearnerTrait, AppWithUsage, Ds}
import ml.Pattern
import ml.classifiers.Learner
import util.{Stat, StatTests}

object tab extends AppWithUsage with LearnerTrait with StratsTrait {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "medida:alca|alcg")
  val context = "tabtex"
  run()

  override def run() = {
    ???
    super.run()
    val res = datasets map { dataset =>
      val ds = Ds(path, dataset)
      ds.open()
      val ms = for {
        l <- learners(learnersStr)
        s <- allStrats()
      } yield {
        val vs = for {
          r <- 0 until runs
          f <- 0 until folds
        } yield {
          ds.getMeasure(measure, s, l, r, f) match {
            case Some(v) => v
            case None => justQuit(s"No measure for ${(measure, s, l, r, f)}!")
          }
        }
        //        s.id + " " + l.id
        Stat.media_desvioPadrao(vs.toVector)
      }
      ds.close()
      ds.dataset -> ms
    }
    StatTests.extensiveTable2(res.toSeq, (1 to res.head._2.size).toVector.map(_.toString), "nome", "meas")
    justQuit("Datasets prontos.")
  }
}
