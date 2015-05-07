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

import al.strategies.Strategy
import clean.lib._
import ml.classifiers.{BestClassifCV100_10foldReadOnlyKappa, Learner, NoLearner}
import util.{Stat, StatTests}

object coincideBestLeas extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "coincideBestLeas"
   val measurele = ALCKappa
   val measurecl = Kappa

   //s.l.id = l.id quando gnóstico
   val medes = stratsTex(dist).map(s0 => (ds: Ds, s: Strategy, r: Int, f: Int) => (l: Learner) => {
      val s = s0(l)
      val (ti, th, tf, tpass) = ranges(ds)
      measurele(ds, s, l, r, f)(ti, tf).read(ds).getOrElse(ds.error(s"incompleto para ${(ds, s, l, r, f)}!"))
   }) :+ ((ds: Ds, s: Strategy, r: Int, f: Int) => (l: Learner) => {
      val cl = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s)
      measurecl(ds, s, cl, r, f)(-2).read(ds).getOrElse(ds.error(s"incompleto para ${(ds, s, l, r, f)}!"))
   })

   run()

   override def run() = {
      super.run()
      val accs0 = for (mede <- medes) yield {
         val res0 = for {
            dataset <- datasets
         } yield {
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               //descobre melhor aprendiz-hiter
               //               learners(learnersStr) map mede(ds,s,r,f)
            }

            ds.close()
         }
      }
      //      val accs = accs0.toList //.sortBy(_._1)
      //      val dists = for (a <- accs) yield {
      //            val ds = for (b <- accs) yield {
      //               val d = math.sqrt(a._2.zip(b._2).map { case (v1, v2) =>
      //                  val v = v1 - v2
      //                  v * v
      //               }.sum)
      //               ff(100)(1 / (1 + d))
      //            }
      //            a._1 -> ds
      //         }
      //      val arq = s"/home/davi/wcs/tese/stratDists${if (reduz) "redux" else ""}" + ".tex"
      //      println(accs0.head._2.size + " dimensions.")
      //      //      println(s"$arq")
      //      //      val fw = new PrintWriter(arq, "ISO-8859-1")
      //      println(StatTests.distTable(dists, "stratDists", "estratégias", measure.toString))
      //      //      fw.write(StatTests.distTable(dists, "stratDists", "estratégias", measure.toString))
      //      //      fw.close()
   }
}
