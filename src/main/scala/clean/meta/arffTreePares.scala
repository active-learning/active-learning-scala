package clean.meta

import java.io.FileWriter

import clean.lib._
import ml.classifiers._
import util.Stat

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
object arffTreePares extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator {
   val perdedores = false
   val mostrar = 1
   //0.333
   val measure = Kappa
   val context = "metaAttsTreeparesApp"
   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val n = if (perdedores) 1 else 3
   val pioresAignorar = 0
   val minObjs = if (perdedores) 45 else 20
   val qs50 = true
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val bestLearners = pioresAignorar > 0
      val arq = s"/home/davi/wcs/ucipp/uci/metaTree$measure" + (if (bestLearners) s"Best-$pioresAignorar" else "") + s"${if (perdedores) "perd" else ""}${if (qs50) "50" else ""}.arff"
      println(arq)
      val tex = s"/home/davi/wcs/tese/treePares$measure" + (if (bestLearners) s"Best-$pioresAignorar" else "") + s"${if (perdedores) "perd" else ""}${if (qs50) "50" else ""}.tex"
      println(tex)
      val ls = learners(learnersStr)
      val strats = (for {l <- ls; s <- stratsTex("all").map(_(l))} yield s).distinct
      val metadata0 = for {
         name <- datasets.toList.filter { dataset =>
            val ds = Ds(dataset, readOnly = true)
            ds.open()
            val r = ds.poolSize >= 200
            ds.close()
            r
         }
      } yield {
         val ds = Ds(name, readOnly = true)
         ds.open()
         val medidas = for (s <- strats) yield {
            val ms = (for {
               r <- 0 until runs
               f <- 0 until folds
            } yield {
               val classif = if (qs50) BestClassifCV50_10foldReadOnlyKappa(ds, r, f, s) else BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s)
               classif.limpa -> measure(ds, s, classif, r, f)(if (qs50) -3 else -2).read(ds).getOrElse {
                  println((ds, s, s.learner, classif, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
               }
            }) map (_._2)
            s.abrev -> Stat.media_desvioPadrao(ms.toVector)
         }
         val res = if (perdedores) pegaMelhores(medidas, n)(-_._2._1).map { bs => (ds.metaAttsHumanAndKnowingLabels, bs._1)}
         else pegaMelhores(medidas, n)(_._2._1).map { bs => (ds.metaAttsHumanAndKnowingLabels, bs._1)}
         ds.close()
         res
      }
      val metadata = metadata0.flatten.toList
      //      metadata foreach println

      //cria ARFF
      val pred = metadata.map(_._2)
      val labels = pred.distinct.sorted
      val data = metadata.map { case (numericos, vencedora) => numericos.mkString(",") + "\"" + vencedora + "\""}
      val numAtts = humanNumAttsNames
      val header = List("@relation data") ++ numAtts.split(",").map(i => s"@attribute $i numeric") ++ List("@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}", "@data")
      val pronto = header ++ data
      //      pronto foreach println

      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()
      println(s"${data.size}")

      //constrói e transforma árvore
      C45(laplace = false, minObjs, mostrar).tree(arq, tex)
   }
}