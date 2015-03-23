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
   /*
   Antes, se colocasse human=false, apenas um intervalo e apenas um learner,
   daria na mesma que usar arffMeta.scala="Winner" com apenas um learner.

   Quando ties=true, o metaexemplo é repetido em caso de empate.
   Caso contrário, apenas o melhor vencedor serve de rótulo.
    */
   val ties = false
   val bestLearner = true
   val context = "metaAttsTreeApp"
   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val measure = ALCKappa
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val pool = Seq()
      val learner = NoLearner()
      val sss = //stratsForTreeRedux()
         Seq(
            ExpErrorReductionMargin(learner, pool, "entropy") //
            , DensityWeighted(learner, pool, 1, "eucl") //
            , AgDensityWeightedTrainingUtility(pool, "maha") //
            , DensityWeightedTrainingUtility(learner, pool, "manh") //
            , HTU(learner, pool, "manh")
            , DensityWeightedTrainingUtility(learner, pool, "maha") //

            , SVMmultiRBF(pool, "SIMPLEw") //
            , RandomSampling(pool)
            , ClusterBased(pool)
            , AgDensityWeightedTrainingUtility(pool, "eucl") //
            , AgDensityWeightedTrainingUtility(pool, "manh") //
            , HTU(learner, pool, "eucl") //
            , HTU(learner, pool, "maha") //
            , new SGmulti(learner, pool, "consensus") //
            , Margin(learner, pool)
            , DensityWeightedTrainingUtility(learner, pool, "eucl") //
            , ExpErrorReductionMargin(learner, pool, "balacc")
            , SVMmultiRBF(pool, "BALANCED_EEw")
            , ExpELMChange(pool),
            QBC(pool)
         )

      val ss = sss.map(_.abr).toVector
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
            Seq((tmin, tmax, "alto"))
         }

      } yield {
         val ds = Ds(name, readOnly = true)
         ds.open()
         val res = if (ties) {
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
               multiplicadorDeAmostra <- 0 to 7
            } yield {
               val poolStr = (100 * r + f).toString
               val medidas = for (s <- sss) yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                  ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                  NA
               }
               poolStr -> medidas
            }
            val winners = StatTests.clearWinners(vs, ss)
            ss.map { x =>
               if (winners.contains(x)) Option(ds.metaAttsHumanAndKnowingLabels, l.abr, x, budix, l.attPref, l.boundaryType)
               else None
            }.flatten
         } else {
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
            if (medidas.exists(x => x._2._1 == -2d)) Seq()
            else medidas.groupBy(_._2._1).toList.sortBy(_._1).reverse.take(1).map(_._2.map(_._1)).flatten.map { bs =>
               (ds.metaAttsHumanAndKnowingLabels, l.abr, bs, budix, l.attPref, l.boundaryType)
            }
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
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute \"orçamento\" {\"$\\cent\\leq 50$\",baixo,alto}", "@attribute aprendiz {" + learners(learnersStr).map(x => "\"" + x.abr + "\"").mkString(",") + "}", "@attribute \"atributo aceito\" {\"numérico\",\"nominal\",\"ambos\"}", "@attribute \"fronteira\" {\"rígida\",\"flexível\",\"nenhuma\"}", "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}", "@data")
      val pronto = header ++ data
      pronto foreach println

      val arq = "/home/davi/wcs/ucipp/uci/metaTree" + (if (ties) "Ties" else "") + (if (bestLearner) "Best" else "") + ".arff"
      println(arq)
      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()
      println(s"${data.size}")




      def trav(t: Tree): String = t match {
         case Root(children) => """\node[line width=0.3ex, decision] {""" + children.head.asInstanceOf[Obj].cond + "}\n" + (children map trav).mkString("\n")
         case Node(cond, operador, valor, children) => "child {node [decision, label=" + op(operador, valor) + "] {" + children.head.asInstanceOf[Obj].cond + "}\n" + (children map trav).mkString("\n") + "}"
         case l@Leaf(cond, operador, valor, texto, pureza) => "child {node [outcome, label=" + op(operador, valor) + "] {" + texto + "\\\\($" + pureza + "\\%$)" + "}}"
         case _ => justQuit(s"erro matching")
      }

      def op(operador: String, valor: String) = operador match {
         case "=" => valor
         case "<=" => "$\\leq" + ((valor.toDouble * 1000).round / 1000d) + "$"
         case ">" => "$>" + ((valor.toDouble * 1000).round / 1000d) + "$"
      }

      val ps = Datasets.arff(arq, dedup = false).right.get
      val l = C45(laplace = false, 4)
      val m = l.build(ps)

      val arq2 = "/home/davi/wcs/tese/tree.tex"
      println(s"")
      println(arq2)
      val fw2 = new PrintWriter(arq2, "ISO-8859-1")
      fw2.write(trav(Parsing.parse(l.tree(m))) + ";")
      fw2.close()
   }
}