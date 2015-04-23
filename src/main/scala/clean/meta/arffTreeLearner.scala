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
package clean.meta

import java.io.{PrintWriter, FileWriter}

import al.strategies._
import clean.lib._
import ml.classifiers._
import util.{Datasets, Stat, StatTests}

object arffTreeLearner extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator {
   val perdedores = false
   val minobjs = 10
   val measure = Kappa
   val context = "arffTreeLearnerApp"
   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val n = 1
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val metadata0 = for {
         name <- datasets.toList
         (ti, tf, budix) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()
            val (tmin, thalf, tmax, tpass) = ranges(ds)
            ds.close()
            //            Seq((tmin, thalf, "\"$\\cent\\leq 50$\""), (tmin, thalf, "baixo"), (thalf, tmax, "alto"))
            //                        Seq((tmin, thalf, "baixo"), (thalf, tmax, "alto"))
            Seq((tmin, tmax, "alto"))
         }
      } yield {
         val ds = Ds(name, readOnly = true)
         ds.open()
         val medidas = for (l <- learners(learnersStr)) yield {
            try {
               val ms = for {
                  r <- 0 until Global.runs
                  f <- 0 until Global.folds
               } yield measure(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.error(s" base incompleta para intervalo [$ti;$tf] e pool ${(l, r, f)}."))
               l.abr -> Stat.media_desvioPadrao(ms.toVector)
            } catch {
               case _: Throwable => ds.error(s" base incompleta para intervalo [$ti;$tf] e pool ${l}.")
            }
         }
         val res = if (perdedores) pegaMelhores(medidas, n)(-_._2._1).map(_._1).map { bs =>
            (ds.metaAttsHumanAndKnowingLabels, "NB", bs, budix, "ambos", "nenhuma")
         } else pegaMelhores(medidas, n)(_._2._1).map(_._1).map { bs =>
            (ds.metaAttsHumanAndKnowingLabels, "NB", bs, budix, "ambos", "nenhuma")
         }
         ds.close()
         res
      }
      val metadata = metadata0.flatten.toList
      //      metadata foreach println

      //cria ARFF
      val pred = metadata.map(_._3)
      val labels = pred.distinct.sorted
      val data = metadata.map { case (numericos, learne, vencedora, budget, attPref, boundaryType) => numericos.mkString(",") + s",$budget,$learne,$attPref,$boundaryType," + "\"" + vencedora + "\""}
      val numAtts = humanNumAttsNames
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++
         List("@attribute \"orçamento\" {\"$\\cent\\leq 50$\",baixo,alto}", "@attribute algoritmo {" + learners(learnersStr).map(x => "\"" + x.abr + "\"").mkString(",") + "}", "@attribute \"atributo aceito\" {\"numérico\",\"nominal\",\"ambos\"}", "@attribute \"fronteira\" {\"rígida\",\"flexível\",\"nenhuma\"}", "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}", "@data")
      val pronto = header ++ data
      //      pronto foreach println

      val arq = s"/home/davi/wcs/ucipp/uci/metaTreeLearner$measure" + s"${if (perdedores) "perd" else ""}.arff"
      println(arq)
      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close
      println(s"${data.size}")

      //constrói e transforma árvore
      val tex = s"/home/davi/wcs/tese/treeLearner$measure" + s"${if (perdedores) "perd" else ""}.tex"
      println(tex)
      C45(laplace = false, minobjs, 1).tree(arq, tex)
   }
}