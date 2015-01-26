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

import al.strategies.Passive
import clean._
import clean.meta.RangeGenerator
import clean.res._
import util.{Stat, StatTests}

object friedBalaccpassClassifiers extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "friedBalaccpassClassifierstex"
   val measure = BalancedAcc
   run()

   override def run() = {
      super.run()
      val ls = learners(learnersStr).map(_.abr).toVector
      val res0 = for {
         dataset <- datasets
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val lres = for {
            l <- learners(learnersStr).sortBy(_.abr)
         } yield {
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("passiva não encontrada"))
            Stat.media_desvioPadrao(vs.toVector)
         }
         ds.close()
         ds.dataset -> lres
      }
      val sorted = res0.toList.sortBy(_._1)

      println(s"")
      sorted.grouped(280).foreach { res1 =>
         StatTests.extensiveTable2(1000, res1.toSeq.map(x => x._1.take(3) + x._1.takeRight(12) -> x._2), ls, "tab:balaccClassif", measure.toString, 7)
      }

      println(s"")
      val res = sorted.filter(!_._2.contains(NA, NA))
      val pairs = StatTests.friedmanNemenyi(res.map(x => x._1 -> x._2.map(_._1)), ls)

      println(StatTests.pairTable(pairs, "tablename", "acc"))
   }
}
