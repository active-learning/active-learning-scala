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

import java.io.{File, FileWriter, OutputStream, PrintStream}

import clean.lib.{FilterTrait, Log, Rank}
import clus.Clus
import ml.Pattern
import ml.classifiers.{Learner, NinteraELM, RF}
import ml.models.ELMModel
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.{Datasets, Stat}
import weka.core.DenseInstance
import weka.core.converters.ArffSaver

import scala.util.Random

trait MetaTrait extends FilterTrait with Rank with Log {
  def clusSettings(natts: Int, targets: Int, seed: Int, arqtr: String, arqts: String) = {
    val ultimoDesc = natts + 1
    val primeiroTarget = ultimoDesc + 1
    val ultimoTarget = primeiroTarget + targets - 1
    Seq(
      "[Data]",
      s"File = $arqtr.arff",
      s"TestSet = $arqts.arff",
      "",
      "[General]",
      s"RandomSeed = $seed",
      "",
      "[Attributes]",
      s"Descriptive = 2-$ultimoDesc",
      s"Target = $primeiroTarget-$ultimoTarget",
      s"Clustering = $primeiroTarget-$ultimoTarget",
      "Weights = 1",
      "",
      "[Tree]",
      "Heuristic = VarianceReduction",
      "FTest = 1",
      "PruningMethod = M5Multi",
      "M5PruningMult = 1",
      "",
      "[Output]",
      "WritePredictions = {Test}"
    ).mkString("\n")
  }


  def arff(strats: String, exemplos: Seq[(Seq[(String, String, String)], String)], print: Boolean = false, context: String, rank: Boolean) = {
    lazy val labels = exemplos.map(x => x._2).distinct.sorted
    lazy val classAtt = "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}"
    val attsNameType = exemplos.head._1 map { case (no, va, ty) => s"$no $ty" }
    val header = List(s"@relation autogeneratedby$context") ++ (attsNameType map (nt => s"@attribute $nt")) ++ (if (rank) List() else List(classAtt))
    val data = exemplos.map(x => "\"" + x._1.head._2 + "\"," + x._1.tail.map(_._2).mkString(",") + (if (rank) "" else s"," + "\"" + x._2 + "\""))
    if (print) {
      //      println("labels: " + labels.mkString(" "))
      //      println(s"#classes: ${labels.size}")
      println(s"#atributos: ${exemplos.head._1.size}")
      println(s"#exemplos: ${exemplos.size}")
    }
    Seq("%" + strats) ++ header ++ Seq("@data") ++ data
  }

  def grava(arq: String, linhas: Seq[String], print: Boolean = false): Unit = {
    val fw = new FileWriter(arq)
    linhas foreach (x => fw.write(s"$x\n"))
    fw.close()
    if (print) linhas foreach println
  }

  def instances2file(patterns: Vector[Pattern], arq: String) {
    val as = new ArffSaver()
    as.setInstances(Datasets.patterns2instances(patterns))
    as.setFile(new File(arq + ".arff"))
    as.writeBatch()
  }

  val originalStream = System.out
  val dummyStream = new PrintStream(new OutputStream() {
    def write(b: Int) {}
  })

  def cv(patterns: Vector[Pattern], leas: Vector[Pattern] => Vector[Learner], rank: Boolean, rs: Int, ks: Int) = {
    (1 to rs).par map { run =>
      val shuffled = new Random(run).shuffle(patterns)
      val bags = shuffled.groupBy(_.base).values.toVector

      Datasets.kfoldCV2(bags, ks, parallel = true) { (trbags, tsbags, fold, minSize) =>
        val seed = run * 100 + fold
        val tr0 = trbags.flatten.toVector
        val ts0 = tsbags.flatten.toVector
        val (tr, ts) = if (rank) tr0 -> ts0
        else {
          val f = Datasets.removeBagFilter(tr0)
          Datasets.applyFilter(f)(tr0) -> Datasets.applyFilter(f)(ts0)
        }

        lazy val (trf, tsf) = replacemissingNom2binRmuselessZscore(tr, ts)
        //pega apenas um ex. por base (um que tiver label mais frequente)
        lazy val tr_trfSemParecidos = Seq(tr, trf) map { trx =>
          if (!rank) tr0.zip(trx).groupBy(_._1.base).map { case (k, lists) =>
            val (_, list) = lists.unzip
            val moda = list.groupBy(_.label).toList.sortBy(_._2.size).last._1
            val attsMedio = media(list.map(_.array))
            val pa = list.head
            val id = pa.id
            val inst = new DenseInstance(1d, attsMedio :+ moda)
            inst.setDataset(pa.dataset)
            Pattern(id, inst, missed = false, pa.parent)
          }.toSeq
          else tr0.zip(trx).groupBy(_._1.base).map { case (k, lists) =>
            val (_, list) = lists.unzip
            val rankMedio = media(list.map(_.targets))
            val descMedio = media(list.map(_.array))
            val pa = list.head
            val id = pa.id
            val inst = new DenseInstance(1d, pa.toDoubleArray.take(1) ++ descMedio ++ rankMedio)
            inst.setDataset(pa.dataset)
            Pattern(id, inst, missed = false, pa.parent)
          }.toSeq
        }
        lazy val (trSemParecidos1, trfSemParecidos1) = tr_trfSemParecidos.head.toVector -> tr_trfSemParecidos(1).toVector

        if (leas(tr).isEmpty) {
          //ELM
          val l = NinteraELM(seed)
          val m0 = l.batchBuild(trfSemParecidos1).asInstanceOf[ELMModel]
          val L = l.LForMeta(m0, LOO = false)
          println(s"${L} <- L")
          val mfull0 = l.batchBuild(trf).asInstanceOf[ELMModel]
          val mfull = l.fullBuildForMeta(L, mfull0)
          val ELMRanks = tsf.toVector map { p => mfull.output(p) }

          //clus; seed tb serve pra situar run e fold durante paralelização
          val arqtr = s"/run/shm/tr$seed"
          val arqts = s"/run/shm/ts$seed"
          instances2file(trSemParecidos1, arqtr)
          instances2file(ts, arqts)
          val f = new FileWriter(s"/run/shm/clus$seed.s")
          f.write(clusSettings(patterns.head.nattributes, patterns.head.nclasses, seed, arqtr, arqts))
          f.close()

          System.setOut(dummyStream)
          Clus.main(Array(s"/run/shm/clus$seed"))
          System.setOut(originalStream)

          val clusPredictionsARFF = Datasets.arff(s"/run/shm/clus$seed.test.pred.arff", dedup = false, rmuseless = false) match {
            case Right(x) => x
            case Left(m) => error(s"${m} <- m")
          }

          val clusOrigRanks_clusPrunRanks = Vector("Original-p", "Pruned-p") map { str =>
            clusPredictionsARFF.map { pa =>
              pa.array.zipWithIndex.flatMap { case (v, i) => if (pa.attribute(i).name.startsWith(str)) Some(v) else None }
            }
          }
          val tstargets = ts.toSeq map (_.targets)
          val rankMedio = media(tstargets)
          val defaultRanks = ts map (_ => rankMedio)

          clusOrigRanks_clusPrunRanks ++ Vector(ELMRanks, defaultRanks) flatMap { ranks =>
            val spears = ranks.zip(tstargets) map { case (ranking, targets) =>
              try {
                val res = new SpearmansCorrelation().correlation(ranking, targets)
                if (res.isNaN) 0d
                //justQuit("\n\n\nNaN no Spear:" + ranking.toList + " " + targets.toList + "\n\n\n")
                else res
              } catch {
                case x: Throwable => error("\n " + ranking.toList + "\n " + rankMedio.toList + "\n " + targets.toList + " \n" + x)
              }
            }
            Seq(0d, Stat.media_desvioPadrao(spears)._1)
          }

        } else {
          //weka
          leas(tr) flatMap { le =>
            val (trtestbags, tstestbags, m) = if (le.querFiltro) {
              val mo = le match {
                case NinteraELM(_, _) =>
                  val l = NinteraELM(seed)
                  //pega apenas um ex. por base (um que tiver label mais frequente)
                  val m0 = l.batchBuild(trfSemParecidos1).asInstanceOf[ELMModel]
                  val L = l.LForMeta(m0, LOO = true)
                  println(s"${L} <- L")
                  val m = l.batchBuild(trf).asInstanceOf[ELMModel]
                  l.fullBuildForMeta(L, m)
                case RF(_, n, _, _) => RF(seed, n).build(tr)
                case _ => le.build(trf)
              }
              (trf.groupBy(x => x.vector), tsf.groupBy(x => x.vector), mo)
            } else (tr.groupBy(x => x.vector), ts.groupBy(x => x.vector), le.build(tr))
            Seq(trtestbags, tstestbags) map (bags => (bags.map(_._2) map { tsbag =>
              tsbag.map(_.label).contains(m.predict(tsbag.head))
            }).count(_ == true) / bags.size.toDouble)
          }
        }
      }
    }
  }
}