package clean.meta

import java.io.{PrintWriter, FileWriter}

import al.strategies._
import clean.lib._
import ml.classifiers._
import util.{Datasets, Stat, StatTests}

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
object arffTree extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator {
   val perdedores = true

   //   val bestLearner = true
   //   val minObjs = if (perdedores) 4 else 4
   val bestLearner = false
   val minObjs = if (perdedores) 20 else 12

   val measure = ALCKappa
   val context = "metaAttsTreeApp"
   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val metadata0 = for {
         name <- datasets.toList.par

         l <- if (bestLearner) Seq(learners(learnersStr).map { l =>
            val ds = Ds(name, readOnly = true)
            ds.open()
            val vs = for (r <- 0 until runs; f <- 0 until folds) yield Kappa(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
            ds.close()
            l -> Stat.media_desvioPadrao(vs.toVector)._1
         }.maxBy(_._2)._1)
         else learners(learnersStr)

         (ti, tf, budix) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()
            val (tmin, thalf, tmax, tpass) = ranges(ds)
            ds.close()
            //            Seq((tmin, thalf, "\"$\\cent\\leq 50$\""), (tmin, thalf, "baixo"), (thalf, tmax, "alto"))
            //            Seq((tmin, thalf, "baixo"), (thalf, tmax, "alto"))
            Seq((tmin, tmax, "alto"))
         }

      } yield {
         val strats = stratsForTreeReduxMah().take(6) ++ stratsForTreeReduxMah().drop(7).take(1) ++ stratsForTreeReduxMah().drop(9)
         val sss = l match {
            case _: SVMLibRBF => strats.dropRight(2)
            case _: NinteraELM => strats.dropRight(4) ++ strats.takeRight(2).dropRight(1)
            case _: RF => strats.dropRight(4) ++ strats.takeRight(1)
            case _ => strats.dropRight(4)
         }

         val ds = Ds(name, readOnly = true)
         ds.open()
         val medidas = for (s <- sss) yield {
            try {
               val ms = for {
                  r <- 0 until Global.runs
                  f <- 0 until Global.folds
               } yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                     //                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                     -2d
                  }
               s.abr -> Stat.media_desvioPadrao(ms.toVector)
            } catch {
               case _: Throwable =>
                  //                     ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l)}.", 40)
                  s.abr ->(NA, NA)
            }

         }
         val res = if (medidas.exists(x => x._2._1 == -2d)) Seq()
         else if (perdedores) medidas.groupBy(_._2._1).toList.sortBy(_._1).take(1).map(_._2.map(_._1)).flatten.map { bs =>
            (ds.metaAttsHumanAndKnowingLabels, l.abr, bs, budix, l.attPref, l.boundaryType)
         }
         else medidas.groupBy(_._2._1).toList.sortBy(_._1).reverse.take(1).map(_._2.map(_._1)).flatten.map { bs =>
            (ds.metaAttsHumanAndKnowingLabels, l.abr, bs, budix, l.attPref, l.boundaryType)
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
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute \"orçamento\" {\"$\\cent\\leq 50$\",baixo,alto}", "@attribute algoritmo {" + learners(learnersStr).map(x => "\"" + x.abr + "\"").mkString(",") + "}", "@attribute \"atributo aceito\" {\"numérico\",\"nominal\",\"ambos\"}", "@attribute \"fronteira\" {\"rígida\",\"flexível\",\"nenhuma\"}", "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}", "@data")
      val pronto = header ++ data
      pronto foreach println

      val arq = "/home/davi/wcs/ucipp/uci/metaTree" + (if (bestLearner) "Best" else "") + s"$perdedores.arff"
      println(arq)
      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close
      println(s"${data.size}")

      //constrói e transforma árvore
      val tex = "/home/davi/wcs/tese/tree" + (if (bestLearner) "Best" else "") + s"$perdedores.tex"
      println(tex)
      C45(laplace = false, minObjs).tree(arq, tex)
   }
}