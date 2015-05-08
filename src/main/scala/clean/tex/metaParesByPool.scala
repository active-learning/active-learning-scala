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

import java.io.FileWriter

import al.strategies._
import clean.lib._
import ml.classifiers._
import util.{Datasets, Stat}

object metaParesByPool extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = this.getClass.getName.split('.').last.dropRight(1)
   val n = 1
   // 50 100 u2
   val qs = "100"
   val measure = Kappa
   val arq = s"/home/davi/wcs/ucipp/uci/$context.arff"
   run()

   override def run() = {
      super.run()
      val pares = Seq(
         //         MarginFixo(RF(), Seq()),
         HTUFixo(Seq(), RF(), Seq(), "eucl"),
         DensityWeightedTrainingUtilityFixo(Seq(), RF(), Seq(), "eucl"),
         AgDensityWeightedTrainingUtility(Seq(), "eucl")
         //         RandomSampling(Seq())
      )

      //cada dataset produz um bag de metaexemplos (|lista| >= 25)
      lazy val bags = DsByMinSize(datasets, 200).par map { d => (for {
         r <- 0 until runs
         f <- 0 until folds
      } yield {
         val ds = Ds(d, readOnly = true)
         ds.open()

         //descobre vencedores deste pool
         val accs = pares map { p =>
            val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, p)
            p -> measure(ds, p, classif, r, f)(-2).read(ds).getOrElse(error("sem medida"))
         }
         val melhores = pegaMelhores(accs, n)(_._2).map(_._1)

         //transforma vencedores em metaexemplos
         val metaatts = ds.metaAttsrf(r, f) ++ ds.metaAttsFromR(r, f)
         val exs = melhores map (m => metaatts -> m.limpa)
         ds.close()
         exs
      }).flatten
      }

      grava(arq, arff(bags.toList.flatten), print = true)
      println(s"$arq")

      Datasets.arff(arq, dedup = false) match {
         case Left(str) => error(str)
         case Right(patterns) =>
            //            refaz bags
            println(arq)
            val bags = patterns.groupBy(_.vector)

      }

   }

}