package clean.meta

import java.io.PrintWriter

import al.strategies._
import clean.lib._
import ml.classifiers._
import util.Stat

object arffTree extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator {
  val perdedores = true
  val pioresQRnd = perdedores
  val mostrar = 0.9
  val measure = ALCKappa
  val context = "metaAttsTreeApp"
  val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
  val n = if (!perdedores) 3 else 1
  val pioresAignorar = 0
  val minObjs = if (!perdedores) 120 else 120
  run()

  def ff(x: Double) = (x * 100).round / 100d

  override def run() = {
    super.run()
    val pequeno = "\"$50$\""
    val grande = "\"$100$\""
    //    val pequeno = "\"$|Y| \\\\leq \\\\cent\\\\ simbolomenor 50$\""
    //    val grande = "\"$|Y| \\\\leq \\\\cent\\\\ simbolomenor 100$\""
    val bestLearners = pioresAignorar > 0
    val metadata0 = for {
      name <- datasets.toList.par

      l <- if (bestLearners) dispensaMelhores(learners(learnersStr).map { l =>
        val ds = Ds(name, readOnly = true)
        ds.open()
        val vs = for (r <- 0 until runs; f <- 0 until folds) yield Kappa(ds, Passive(Seq()), l, r, f)(-1).read(ds).getOrElse(ds.quit("Kappa passiva não encontrada"))
        ds.close()
        l -> Stat.media_desvioPadrao(vs.toVector)._1
      }, pioresAignorar)(-_._2).map(_._1)
      else learners(learnersStr)

      (ti, tf, budix) <- {
        val ds = Ds(name, readOnly = true)
        ds.open()
        val (tmin, thalf, tmax, tpass) = ranges(ds)
        ds.close()
        //        Seq((tmin, thalf, pequeno), (thalf + 1, tmax, grande))
        Seq((tmin, thalf, pequeno), (tmin, tmax, grande))
      }
    } yield {
        val strats = stratsTexForGraficoComplexo //Simples
        val ds = Ds(name, readOnly = true)
        ds.open()
        val medidas = for (s0 <- strats) yield {
          val s = s0(l)
          try {
            val ms = for {
              r <- 0 until Global.runs
              f <- 0 until Global.folds
            } yield {
                measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse(ds.error(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}."))
              }
            s.abr -> Stat.media_desvioPadrao(ms.toVector)
          } catch {
            case _: Throwable => ds.error(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l)}.")
          }
        }
        val rnd = medidas.find(_._1 == RandomSampling(Seq()).limp).get._2._1
        val selected = if (pioresQRnd) medidas.filter(_._2._1 < rnd)
        else if (perdedores) pegaMelhores(medidas, n)(-_._2._1)
        else pegaMelhores(medidas, n)(_._2._1)
        val res = selected.map(bs => (ds.metaAttsHumanAndKnowingLabels, l.abr, bs._1, budix, l.attPref, l.boundaryType))
        ds.close()
        res
      }
    val metadata = metadata0.flatten.toList
    //      metadata foreach println

    //cria ARFF
    val pred = metadata.map(_._3)
    val labels = pred.distinct.sorted
    val data = metadata.map { case (numericos, learne, vencedora, budget, attPref, boundaryType) => numericos.mkString(",") + s",$budget,$learne,$attPref,$boundaryType," + "\"" + vencedora + "\"" }
    val numAtts = Ds("iris", readOnly = true).humanNumAttsNames
    val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute \"orçamento\" {" + pequeno + "," + grande + "}", "@attribute algoritmo {" + learners(learnersStr).map(x => "\"" + x.abr + "\"").mkString(",") + "}", "@attribute \"atributo aceito\" {\"numérico\",\"nominal\",\"ambos\"}", "@attribute \"fronteira\" {\"rígida\",\"flexível\",\"nenhuma\"}", "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}", "@data")
    val pronto = header ++ data
    //      pronto foreach println

    val arq = s"/home/davi/wcs/arff/metaTree$measure" + (if (bestLearners) s"Best-$pioresAignorar" else "") + s"${if (perdedores) "perd" else ""}.arff"
    val fw = new PrintWriter(arq, "UTF-8")
    pronto foreach (x => fw.write(s"$x\n"))
    fw.close
    println(s"${data.size}")

    //constrói e transforma árvore
    val tex = s"/home/davi/wcs/tese/tree$measure" + (if (bestLearners) s"Best-$pioresAignorar" else "") + s"${if (perdedores) "perd" else ""}.tex"
    println(tex)
    C45(laplace = false, minObjs, mostrar).tree(arq, tex)
  }
}

object mostraAtributosBons extends App{
  //constrói e transforma árvore
  val tex = s"/home/davi/wcs/tese/attsBons.tex"
  val arq=""
  println(tex)
  C45(laplace = false, 10).tree(arq, tex)
}

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
