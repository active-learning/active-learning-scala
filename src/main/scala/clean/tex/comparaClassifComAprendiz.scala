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

import al.strategies.{RandomSampling, Strategy}
import clean.lib._
import ml.classifiers._
import util.{Stat, StatTests}

object comparaClassifComAprendiz extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = "comparaClassifComAprendiz"
   run()

   override def run() = {
      super.run()
      val measure = ALCKappa
      val strats = stratsTex("all")
      val out = for {
         dataset <- DsByMinSize(datasets, 200).par
         s0 <- strats
      } yield {
         val ds = Ds(dataset, readOnly = true)
         ds.open()
         val (ti, th, tf, tpass) = ranges(ds)

         //descobre best aprendizes (tem sido só um por pool)
         val hists = accsPerPool(ds, s0, learners(learnersStr), (s: Strategy, l: Learner, r: Int, f: Int) => measure(ds, s, l, r, f)(ti, tf))
         val aprendizes = hists map (hist => pegaMelhores(hist, 1)(_._2).head)

         //descobre best classifs pra Rnd Samp (gravei um por pool na base)
         //         val classifs = for {
         //            r <- 0 until runs
         //            f <- 0 until folds
         //         } yield BestClassifCV100_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))

         ds.close()
         val comp = aprendizes.map(_._1.limpa).zip(aprendizes.map { case (lea, v, r, f) =>
            BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s0(lea)).limpa
         }).groupBy(x => x).map(x => x._1 -> x._2.size).toList.sortBy(_._2).reverse
         //                  comp.maxBy(_._2)._2 -> ("\n" + dataset + s" / ${s0(NBBatch()).limp}:\n" + comp.map(x => x._1 + ":" + x._2).mkString("\t"))
         comp.maxBy(_._2)._2 -> comp.filter { case (ss, i) => ss._1 == ss._2}.map(_._2).sum
      }
      //            out.toList.sortBy(_._1).reverse.map(_._2) foreach println
      //      out.toList.sortBy(_._2).reverse.map(_._2) foreach println
      println(out.toList.sortBy(_._2).reverse.map(_._2).sum / (1650 * 25d))
   }
}

/*
Mostra que coincidencia só ocorre em 35% das vezes.
Então desvincular aprendiz de classificador tem um impacto positivo.
Apesar, disso, há alguma correlação, pois se fosse ao acaso, seria 17%.

media obtida de coincidencias:
14343/1650
8.69

media esperada de coincidencias:
25/6
4.16
*/
