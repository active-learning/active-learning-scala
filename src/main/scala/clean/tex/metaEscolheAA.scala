package clean.tex

import java.io.File

import clean.lib._
import ml.Pattern
import ml.classifiers._
import util.{Datasets, Stat, Tempo}

object metaEscolheAA extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with MetaTrait {
  lazy val arguments = superArguments ++
    List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "rank", "ntrees", "vencedorOuPerdedor(use1):1|-1", "runs", "folds", "ini", "fim", "porPool:p", "guardaSohRank:true|false")

  val context = this.getClass.getName.split('.').last.dropRight(1)
  val dedup = false
  val measure = ALCKappa
  val dsminSize = 100
  //n=2 estraga stats
  val n = 1
  run()

  override def run() = {
    super.run()
    println(s"escolhe strat p/cada alg")
    if (porPool && (rus != 1 || ks != 90 || !porRank)) justQuit("porPool ativado com parametros errados!")
    val metaclassifs = (patts: Vector[Pattern]) => if (porRank) Vector()
    else Vector(//NB não funciona porque quebra na discretização
      PCT(),
      RF(42, ntrees),
      RoF(42, ntrees),
      ABoo(42, ntrees),
      //esses 3 precisam de seleção de parâmetros/modelo:
      //      SVMLibRBF(),
      //      C45(false, 5),
      //      KNNBatcha(5, "eucl", patts),
      Chute(),
      Maj()
    )

    val combstrats = (2 to stratsPMetaStrat.size).flatMap(n => stratsPMetaStrat.combinations(n).toList)
    val combleas = (2 to learners(learnersStr).size).flatMap(n => learners(learnersStr).combinations(n).toList)
    for (sts1 <- combstrats; les1 <- combleas) {
      val pares1 = for {s <- sts1; l <- les1} yield s -> l
      val txts = pares1.map(x => x._1(x._2).limp + "-" + x._2.limp)

      //    pares1 foreach { case (estr, apren) =>
      Tempo.start
      val parName = "par"
      val arq = s"/home/davi/wcs/arff/$context-$porPool-n${if (porRank) 1 else n}best${criterio}m$measure-$ini.$fim-${parName + (if (porRank) "Rank" else "")}-${txts.mkString.replace("-", "").replace("EE", "").replace("euc", "").replace("NN", "").replace("multi", "")}-U$dsminSize.arff"
      val labels = txts
      val labelssts = txts
      println(s"${txts} <- txts")

      //cada dataset produz um bag de metaexemplos (|bag| >= 25)
      println(s"${datasets.size} <- dssss.size")
      def bagsNaN = datasets.par map { d =>
        val ds = Ds(d, readOnly = true)
        ds.open()
        val (ti0, th0, tf0, tpass) = ranges(ds)
        val ti = ini match {
          case "ti" => ti0
          case "th" => th0 + 1
        }
        val tf = fim match {
          case "th" => th0
          case "tf" => tf0
        }
        val res = for {
          r <- 0 until runs
          f <- 0 until folds
        } yield {
            //descobre vencedores deste pool
            val accs = pares1 map { case (s, l) =>
              (s(NoLearner()).limpa, l.limpa) -> measure(ds, s(l), l, r, f)(ti, tf).read(ds).getOrElse(justQuit("sem medida"))
            }
            val metaatts0 = ds.metaAttsrf(r, f, suav = false).map(x => (x._1, x._2.toString, x._3)) ++ ds.metaAttsFromR(r, f).map(x => (x._1, x._2.toString, x._3))
            val metaatts = ("\"bag_" + pares1.size + "\"", ds.dataset, "string") +: metaatts0
            val insts = if (porRank) {
              //rank legivel por clus e ELM
              List(metaatts ++ ranqueia(accs.map(_._2)).zipWithIndex.map { case (x, i) => (s"class$i", x.toString, "numeric") } -> "")
            } else {
              //Acc
              val melhores = pegaMelhores(accs, n)(_._2 * criterio).map(_._1)
              melhores map (m => metaatts -> (m._1 + "-" + m._2))
            }
            //insts.map(x => x._1.tail.map(x => (x._2.toDouble * 100).round / 100d).mkString(" ")) foreach println
            //            println(s"$r $f")
            insts
          }
        ds.close()
        res.flatten
      }
      def bagsCbases = bagsNaN
      if (!new File(arq).exists()) grava(arq, arff(labels.mkString(","), bagsCbases.toList.flatten, print = true, context, porRank))

      val patterns = Datasets.arff(arq, dedup, rmuseless = false) match {
        case Right(x) =>
          //não consegui descobrir como aumentar qtd de atributos no weka (vai ficar sem atts desvio.
          val osbags = x.groupBy(_.base).map(_._2)
          val ps = (osbags map meanPattern(porRank)).toVector
          patts2file(ps, arq + ".arff")
          if (porPool) osbags.flatten.toVector else ps
        case Left(m) => error(s"${m} <- m")
      }

      if (porRank) print(s"$parName Spearman correl. $rus*$ks-fold CV. " + arq + " ")
      else print(s"$parName Accuracy. $rus*$ks-fold CV. " + arq + " ")

      val ra = if (porRank) "ra" else "ac"
      val metads = new Db("metanew", readOnly = false)
      metads.open()
      //      select ra,cr,i,f,st,ls,rs,fs,mc,nt,dsminsize from r
      val sql69 = s"select mc from r where porPool='$porPool' and ra='$ra' and cr=$criterio and i='$ini' and f='$fim' and st='$parName' and ls='${txts.mkString(";")}' and rs=$rus and fs=$ks and nt=$ntrees and dsminsize='$dsminSize'"
      println(s"${sql69} <- sql69")
      metads.readString(sql69) match {
        //        case x: List[Vector[String]] if x.map(_.head).intersect(metaclassifs(Vector()).map(_.limp)).size == 0 =>
        case x: List[Vector[String]] if x.isEmpty | true=>
          val cvs = cv(porPool, ini, fim, labelssts, parName, ntrees, patterns, metaclassifs, porRank, rus, ks).toVector
          def fo(x: Double) = "%2.3f".format(x)

          //        porMetaLea foreach { case (nome, resultados) =>
          //          val accTr = Stat.media_desvioPadrao(resultados.map(_.accTr))
          //          val accTs = Stat.media_desvioPadrao(resultados.map(_.accTs))
          //          val accBalTr = Stat.media_desvioPadrao(resultados.map(_.accBalTr))
          //          val accBalTs = Stat.media_desvioPadrao(resultados.map(_.accBalTs))
          //          //            val r = resultados reduce (_ ++ _)
          //          //            val (resumoTr, resumoTs) = r.resumoTr -> r.resumoTs
          //          metads.write(s"insert into r values ('$ra', $criterio, '$ini', '$fim', '$parName', '${txts.mkString(";")}', $rus, $ks, '$nome', $ntrees, $dsminSize, ${accTr._1}, ${accTr._2}, ${accTs._1}, ${accTs._2}, ${accBalTr._1}, ${accBalTr._2}, ${accBalTs._1}, ${accBalTs._2}, '$porPool')")
          //          (nome, accTs) -> s"${nome.padTo(8, " ").mkString}:\t${fo(accTr._1)}/${fo(accTr._2)}\t${fo(accTs._1)}/${fo(accTs._2)}"
          //        }

          val cvsf = cvs.flatten
          if (guardaSohRank) {
            val writes = cvsf map {
              //              case Resultado("PCTr-a" | "defr-a" | "rndr-a", _, _) =>
              //              case Resultado("PCTr" | "defr" | "rndr", esperados, preditos) =>
              case (ds, seq) =>
                seq map { case Resultado(metalea, esperados, preditos) =>
                  val rankEsperado = esperados.map(_._1.split(" ").map(_.toDouble)).transpose.map(x => x.sum / x.size).mkString(" ")
                  val rankPredito = preditos.map(_._2.split(" ").map(_.toDouble)).transpose.map(x => x.sum / x.size).mkString(" ")
                  val speaEsperado = Stat.media_desvioPadrao(esperados.map(_._3).toVector)._1
                  val speaEsperadostd = Stat.media_desvioPadrao(esperados.map(_._3).toVector)._2
                  val speaPredito = Stat.media_desvioPadrao(preditos.map(_._3).toVector)._1
                  val speaPreditostd = Stat.media_desvioPadrao(preditos.map(_._3).toVector)._2
                  //              if (!readOnly)
                  s"insert into rank values ('$ds', '$ra', $criterio, '$ini', '$fim', '$parName', '${txts.mkString(";")}', $rus, $ks, '$metalea', $ntrees, $dsminSize, '$rankEsperado', $speaEsperado, $speaEsperadostd, '$rankPredito', $speaPredito, $speaPreditostd, '$porPool')"
                }
            }
            metads.batchWrite(writes.flatten.toList)
          }

        case x: List[Vector[String]] => println(s"${x} <- rows already stored")
      }
      metads.close()
      println()
      //      if (!porRank) Datasets.arff(arq, dedup, rmuseless = false) match {
      //        case Right(x) => if (apenasUmPorBase) {
      //          val ps = (x.groupBy(_.base).map(_._2) map meanPattern(porRank)).toVector
      //          patts2file(ps, arq + "umPorBase")
      //          C45(laplace = false, 5, 1).tree(arq + "umPorBase.arff", arq + "umPorBase" + ".tex")
      //          println(s"${arq} <- arq")
      //        }
      //      }
    }
  }
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
