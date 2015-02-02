package clean.meta

import java.io.FileWriter

import clean.lib._
import util.{Stat, StatTests}

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
   val ties = true
   val context = "metaAttsTreeApp"
   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val measure = ALCKappa
   //   val measure = ALCBalancedAcc
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val ss = stratsForTreeSemSVMRedux.map(_.abr).toVector
      val metadata0 = for {
         name <- datasets.toList

         l <- learners(learnersStr).par
         (ti, tf, budix) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()
            val (tmin, thalf, tmax, tpass) = ranges(ds)
            ds.close()
            Seq((tmin, thalf, 0), (thalf, tmax, 1))
         }

      } yield {
         val ds = Ds(name, readOnly = true)
         //         println(s"$ds")
         ds.open()
         val res = if (ties) {
            val vs = for {
               r <- 0 until runs
               f <- 0 until folds
               duplicadorDeAmostra <- 0 to 3
            } yield {
               val poolStr = (100 * r + f).toString
               val medidas = for (s <- stratsForTreeSemSVMRedux) yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                  ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                  -2d
               }
               poolStr -> medidas
            }
            val winners = StatTests.clearWinners(vs, ss)
            ss.map { x =>
               if (winners.contains(x)) Option(ds.metaAttsHumanAndKnowingLabels, l.abr, x, if (budix == 0) "baixo" else "alto")
               else None
            }.flatten
         } else {
            val medidas = for (s <- stratsForTreeSemSVMRedux) yield {
               val ms = for {
                  r <- 0 until Global.runs
                  f <- 0 until Global.folds
               } yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                     ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                     -2d
                  }
               s.abr -> Stat.media_desvioPadrao(ms.toVector)
            }
            if (medidas.exists(x => x._2._1 == -2d)) Seq() else Seq((ds.metaAttsHumanAndKnowingLabels, l.abr, medidas.maxBy(_._2._1)._1, if (budix == 0) "baixo" else "alto"))
         }
         ds.close()
         res
      }
      val metadata = metadata0.flatten.toList
      //      metadata foreach println

      //cria ARFF
      val pred = metadata.map(_._3)
      val labels = pred.distinct.sorted
      val data = metadata.map { case (numericos, learner, vencedora, budget) => numericos.mkString(",") + s",$budget,$learner,$vencedora"}
      val numAtts = humanNumAttsNames
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute \"orçamento\" {baixo,alto}", "@attribute learner {" + learners(learnersStr).map(_.abr).mkString(",") + "}", "@attribute class {" + labels.mkString(",") + "}", "@data")
      val pronto = header ++ data
      pronto foreach println

      val fw = new FileWriter("/home/davi/wcs/ucipp/uci/metaTree" + (if (ties) "Ties" else "") + ".arff")
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()
      println(s"${data.size}")
   }
}

/*
learner = NB
|   %nominais <= 33.333333: ATUeuc (328.0/264.0)
|   %nominais > 33.333333: SGmulti (77.0/61.0)
learner = 5NN
|   #classes <= 3
|   |   #exemplos/#atributos <= 47.054545: EERent (135.0/105.0)
|   |   #exemplos/#atributos > 47.054545: ATUmah (81.0/67.0)
|   #classes > 3: ATUeuc (115.0/88.0)
learner = VFDT
|   orçamento = baixo: ATUeuc (169.0/123.0)
|   orçamento = alto
|   |   entropia da classe <= 0.947361: Rnd (130.0/114.0)
|   |   entropia da classe > 0.947361: TUeuc (88.0/75.0)
learner = C4.5
|   orçamento = baixo: ATUeuc (172.0/130.0)
|   orçamento = alto
|   |   #exemplos <= 614.4: ATUeuc (140.0/122.0)
|   |   #exemplos > 614.4: ATUmah (82.0/64.0)
learner = SVM: ATUmah (350.0/217.0)
learner = CIELM
|   entropia da classe <= 0.805125: TUeuc (74.0/56.0)
|   entropia da classe > 0.805125: TUmah (304.0/225.0)

Number of Leaves  : 	14

Size of the tree : 	23
 */