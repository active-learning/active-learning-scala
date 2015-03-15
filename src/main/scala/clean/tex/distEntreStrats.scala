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

import al.strategies.Passive
import clean.lib._
import util.{Stat, StatTests}

object distEntreStrats extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "distEntreStratstex"
   val measure = ALCKappa
   val redux = false
   run()

   override def run() = {
      super.run()
      val accs0 = for (s <- if (redux) stratsForTreeRedux().dropRight(4).par else stratsForTree().dropRight(4).par) yield {
         val res0 = for {
            dataset <- datasets
            l <- learners(learnersStr)
         } yield {
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val (ti, th, tf, tpass) = ranges(ds)
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
            } yield try {
                  measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                     println(s"incompleto para ${(ds, s, l, r, f)}!")
                     NA
                     //                  ds.error(s"incompleto para ${(ds, s, l, r, f)}!")
                  }
               } catch {
                  case _: Throwable => println(s"incompleto para ${(ds, s, l, r, f)}!")
                     NA
               }
            //            println(s"$ds $vs")
            ds.close()
            Stat.media_desvioPadrao(vs.toVector)._1
         }
         s.abr -> res0
      }
      val accs = accs0.toList //.sortBy(_._1)
      val dists = for (a <- accs) yield {
            val ds = for (b <- accs) yield {
               val d = math.sqrt(a._2.zip(b._2).map { case (v1, v2) =>
                  val v = v1 - v2
                  v * v
               }.sum)
               ff(100)(1 / (1 + d))
            }
            a._1 -> ds
         }
      val arq="/home/davi/wcs/tese/stratDists" + (if (redux) "Redux" else "") + ".tex"
      println(accs.size + "datasets.")
      println(s"$arq")
      val fw = new PrintWriter(arq, "ISO-8859-1")
      fw.write(StatTests.distTable(dists, "stratDists" + (if (redux) "Redux" else ""), "estratégias", measure.toString))
      fw.close()
   }
}
