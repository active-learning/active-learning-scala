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

import java.io.{File, FileWriter}
import ml.Pattern

import scala.util.Random
import al.strategies._
import clean.lib._
import ml.classifiers._
import util.{Datasets, Stat}

object metaParesByPool extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
   lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val context = this.getClass.getName.split('.').last.dropRight(1)
   val n = 3
   // 50 100 u2
   val qs = "100"
   val melhor = 1
   val measure = Kappa
   run()

   override def run() = {
      super.run()
      val ls = learners(learnersStr)
      val pares = (for {l <- ls; s <- stratsTex("all").map(_(l))} yield s).distinct

      //      val pares =         Seq(
      //         MarginFixo(RF(), Seq()),
      //         HTUFixo(Seq(), RF(), Seq(), "eucl"),
      //         DensityWeightedTrainingUtilityFixo(Seq(), RF(), Seq(), "eucl"),
      //         AgDensityWeightedTrainingUtility(Seq(), "eucl"),
      //         RandomSampling(Seq())
      //      )
      val arq = s"/home/davi/wcs/ucipp/uci/$context-n${n}best${melhor}m$measure${qs}qs-${pares.map(_.id).mkString("-").hashCode}.arff"
      println(arq)
      println(arq.size)

      //cada dataset produz um bag de metaexemplos (|bag| >= 25)
      def bags = DsByMinSize(datasets, 200).par map { d =>
         val ds = Ds(d, readOnly = true)
         ds.open()
         val res = for {
            r <- 0 until runs
            f <- 0 until folds
         } yield {
            //descobre vencedores deste pool
            val accs = pares map { p =>
               val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, p)
               p -> measure(ds, p, classif, r, f)(-2).read(ds).getOrElse(error("sem medida"))
            }
            val melhores = pegaMelhores(accs, n)(_._2 * melhor).map(_._1)

            //transforma vencedores em metaexemplos
            val metaatts = ds.metaAttsrf(r, f) ++ ds.metaAttsFromR(r, f)
            val exs = melhores map (m => metaatts -> m.limpa)
            exs
         }
         ds.close()
         res.flatten
      }

      println(s"$arq")
      if (!new File(arq).exists()) grava(arq, arff(bags.toList.flatten), print = true)
      println(s"$arq")

      val patterns = Datasets.arff(arq, dedup = false).right.get
      // refaz bags por base
      val bagsFromFile = patterns.groupBy(_.vector).values.toSeq
      val accs = cv10x10fold(bagsFromFile, Seq(C45(false, 6), Maj()))
   }

   def cv10x10fold(bagsFromFile: Seq[Vector[Pattern]], leas: Seq[Learner]) = (1 to 10).par map { run =>
      val shuffledbagsFromFile = new Random(run).shuffle(bagsFromFile)
      Datasets.kfoldCV2(shuffledbagsFromFile, 10, parallel = true) { (trbags, tsbags, fold, minSize) =>
         val tr = trbags.flatten
         //refaz bags por duplicidade
         val bags = tsbags.flatten.groupBy(x => x.vector)
         val bagssize = bags.size.toDouble
         leas map { le =>
            val m = le.build(tr)
            (bags.map(_._2) map { tsbag =>
               tsbag.map(_.label).contains(m.predict(tsbag.head))
            }).count(_ == true) / bagssize
         }
      }
   }
}