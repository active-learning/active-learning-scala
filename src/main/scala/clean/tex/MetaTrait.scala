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
import java.util.UUID

import clean.lib.{Db, FilterTrait, Log, Rank}
import clus.Clus
import ml.{PatternParent, Pattern}
import ml.classifiers._
import ml.models.{RandomRank, FakeModelRank, EnsembleModel}
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.{Datasets, Stat}
import weka.attributeSelection.{BestFirst, AttributeSelection, WrapperSubsetEval, GreedyStepwise}
import weka.classifiers.`lazy`.IBk
import weka.classifiers.trees.RandomForest
import weka.core.{Attribute, Instances, DenseInstance}
import weka.core.converters.ArffSaver
import weka.filters.supervised.instance.{SMOTE, ClassBalancer}
import weka.filters.unsupervised.attribute.PrincipalComponents

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

trait MetaTrait extends FilterTrait with Rank with Log {
  def clusSettings(ntrees: Int, natts: Int, targets: Int, seed: Int, arqtr: String, arqts: String, first: Int = 2) = {
    val ultimoDesc = natts + first - 1
    val primeiroTarget = ultimoDesc + 1
    val ultimoTarget = primeiroTarget + targets - 1
    (Seq(
      "[Data]",
      s"File = $arqtr.arff",
      s"TestSet = $arqts.arff",
      "",
      "[General]",
      s"RandomSeed = $seed",
      "",
      "[Attributes]",
      s"Descriptive = $first-$ultimoDesc",
      s"Target = $primeiroTarget-$ultimoTarget",
      s"Clustering = $primeiroTarget-$ultimoTarget",
      "Weights = 1",
      "",
      "[Tree]",
      "Heuristic = VarianceReduction",
      "FTest = 1") ++
      (if (ntrees == 1) Seq("PruningMethod = M5Multi", //ajuda C45 quebra
        "M5PruningMult = 1", "")
      else Seq("")) ++ //ajuda mais
      Seq("[Output]",
        //      "WritePredictions = {Train,Test}",
        "WritePredictions = {Test}",
        "") ++
      (if (ntrees == 1) Seq("ShowModels = {Default, Pruned, Others}", "PrintModelAndExamples = Yes")
      else Seq("[Ensemble]",
        s"Iterations = $ntrees",
        "EnsembleMethod = Bagging", //Bagging, RForest, RSubspaces, BagSubspaces só funfou bagging
        ""))
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

  def patts2file(patterns: Vector[Pattern], arq: String) {
    val as = new ArffSaver()
    as.setInstances(Datasets.patterns2instances(patterns))
    as.setFile(new File(arq + ".arff"))
    as.writeBatch()

    val f = Source.fromFile(arq + ".arff")
    val lis = f.getLines().toList
    f.close()
    val lis2 = lis

    //parte pra PCA que nao funfa sem PCA:
    //   val lis2 =  map {
    //      case x if x.toLowerCase.startsWith("@attribute") && !x.contains(" class ") =>
    //        val y = x.toLowerCase.replace("@attribute '", "@attributeº'").replace("@attribute -", "@attributeº'-").replace("@attribute 0", "@attributeº'0").replace("... numeric", "...'ºnumeric").replace(" numeric", "ºnumeric")
    //        y.replace(" ", "-").replace("º", " ")
    //      case x => x
    //    }

    val fw = new FileWriter(arq + ".arff")
    fw.write(lis2.mkString("\n"))
    fw.close()
  }

  val originalStream = System.out
  val dummyStream = new PrintStream(new OutputStream() {
    def write(b: Int) {}
  })

  /**
   * Para fazer undersample.
   * @param rank
   * @param bag
   * @return
   */
  def meanPattern(rank: Boolean)(bag: Vector[Pattern]) = if (!rank) {
    val moda = bag.groupBy(_.label).toList.sortBy(_._2.size).last._1
    //não tem problema tirar média de atributo nominal, desde que ele seja constante, senão vai ser um nr quebrado (ou arredondamento sem sentido?)
    if (bag.map(_.base).distinct.size > 1) ???
    val attsMedio = media(bag.map(_.toDoubleArray.dropRight(1)))
    val pa = bag.head
    val id = bag.minBy(_.id).id
    val inst = new DenseInstance(1d, attsMedio :+ moda)
    inst.setDataset(pa.dataset)
    Pattern(id, inst, missed = false, pa.parent)
  } else {
    val rankMedio = media(bag.map(_.targets))
    val descMedio = media(bag.map(_.array))
    val pa = bag.head
    val id = pa.id
    val inst = new DenseInstance(1d, pa.toDoubleArray.take(1) ++ descMedio ++ rankMedio)
    inst.setDataset(pa.dataset)
    Pattern(id, inst, missed = false, pa.parent)
  }


  /**
   * Para fazer umPorBase.
   * @param rank
   * @param bag
   * @return
   */
  //não tem problema tirar média de atributo nominal, desde que ele seja constante, senão vai ser um nr quebrado (ou arredondamento sem sentido?)
  def meanPatternComDesvios(rank: Boolean, dat: PatternParent)(bag: Vector[Pattern]) = if (!rank) {
    val moda = bag.groupBy(_.label).toList.sortBy(_._2.size).last._1
    if (bag.map(_.base).distinct.size > 1) ???
    val (attsm, attsd) = Stat.media_desvioPadraol(bag.map(_.toDoubleArray.dropRight(1).toVector)).toArray.unzip
    val id = bag.minBy(_.id).id
    val inst = new DenseInstance(1d, attsm ++ attsd :+ moda)
    inst.setDataset(dat.dataset)
    Pattern(id, inst, missed = false, dat)
  } else {
    val rankMedio = media(bag.map(_.targets))
    val (attsm, attsd) = Stat.media_desvioPadraol(bag.map(_.array.toVector)).toArray.unzip
    val id = bag.minBy(_.id).id
    val pa = bag.head
    val inst = new DenseInstance(1d, pa.toDoubleArray.take(1) ++ attsm ++ attsd ++ rankMedio)
    inst.setDataset(dat.dataset)
    Pattern(id, inst, missed = false, dat)
  }

  def normRank(ranking: Array[Double]) = {
    val nclasses = ranking.size
    val (min, max) = ranking.min -> ranking.max
    ranking.map(x => 1 + (nclasses - 1) * (x - min) / (max - min))
  }

  def fo(x: Double) = "%2.1f".format(x)

  def cv(porPool: Boolean, ti: String, tf: String, labels: Seq[String], strat: String, ntrees: Int, patterns: Vector[Pattern], leas: Vector[Pattern] => Vector[Learner], rank: Boolean, rs: Int, ks: Int, readOnly: Boolean = false) = {
    //id serve pra evitar conflito com programas paralelos
    val id = "_id" + UUID.randomUUID() + patterns.map(_.id).mkString.hashCode + System.currentTimeMillis.hashCode

    val metads = new Db("metanew", readOnly)
    metads.open()

    val rrr = (0 to rs - 1).par.map { run =>
      val bagsrefeito = patterns.groupBy(_.base).values.toVector
      val shuffled = new Random(run).shuffle(bagsrefeito)

      Datasets.kfoldCV2(shuffled, ks, parallel = true) { (trbags, tsbags, fold, minSize) =>
        val base = tsbags.head.head.nomeBase

        //seed tem sobreposição acima de 100 folds
        if (ks > 100) ???
        val seed = run * 100 + fold

        val (tr0, ts0) = trbags.flatten.toVector -> tsbags.flatten.toVector
        val (tr, ts) = if (rank) tr0 -> ts0
        else {
          val f = Datasets.removeBagFilter(tr0)
          val (tr00, ts00) = Datasets.applyFilter(f)(tr0) -> Datasets.applyFilter(f)(ts0)
          tr00 -> ts00
        }

        if (tr.isEmpty || ts.isEmpty) {
          tr foreach println
          println("---------")
          ts foreach println
          println("Empty set before weka filters. quiting...")
          sys.exit(0)
        }
        //        lazy val (trf, tsf) = replacemissingNom2binRmuselessZscore(tr, ts)
        //pega apenas um ex. por base (um que tiver label mais frequente)
        //        lazy val tr_trfSemParecidos = Seq(tr0.zip(tr).groupBy(_._1.base).map(_._2.map(_._2)), tr0.zip(trf).groupBy(_._1.base).map(_._2.map(_._2))) map { bags =>
        //          bags map meanPattern(rank)
        //        }
        val trSemParecidos = (tr0.zip(tr).groupBy(_._1.base).map(_._2.map(_._2)) map meanPattern(rank)).toVector
        //        lazy val (trSemParecidos, trfSemParecidos) = tr_trfSemParecidos.head.toVector -> tr_trfSemParecidos(1).toVector
        //        println(s"sizes: ${trSemParecidos.size} ${ts.size}")

        //se for ranking
        if (leas(tr).isEmpty) {
          //clus; seed tb serve pra situar run e fold durante paralelização
          val arqtr = s"/run/shm/tr$seed$id"
          val arqtrts = s"/run/shm/trts$seed$id"
          patts2file(trSemParecidos, arqtr) //sem redundantes: 48/54; com todos 43/44
          patts2file(tr ++ ts, arqtrts) //coleta predições de tr e ts num só arquivo //tanto faz se tr tem parecidos, pois não vou usar a table r nesse caso, mas ts é com parecidos
          val f = new FileWriter(s"/run/shm/clus$seed$id.s")
          f.write(clusSettings(ntrees, patterns.head.nattributes, patterns.head.nclasses, seed, arqtr, arqtrts))
          f.close()

          System.setOut(dummyStream)
          Clus.main(Array("-forest", "-silent", s"/run/shm/clus$seed$id"))
          System.setOut(originalStream)

          //coleta predições de tr e ts num só arquivo
          val clusTSPredictionsARFF = Datasets.arff(s"/run/shm/clus$seed$id.test.pred.arff", dedup = false, rmuseless = false) match {
            case Right(x) => x
            case Left(m) => error(s"${m} <- m")
          }
          new File(arqtr + ".arff").delete
          new File(arqtrts + ".arff").delete
          new File(s"/run/shm/clus$seed$id.s").delete()
          new File(s"/run/shm/clus$seed$id.train.1.pred.arff").delete
          new File(s"/run/shm/clus$seed$id.test.pred.arff").delete
          new File(s"/run/shm/clus$seed$id.out").delete
          new File(s"/run/shm/clus$seed$id.model").delete
          val clusTRTSRanks = clusTSPredictionsARFF.zip(tr ++ ts).map { case (pa, pats) =>
            pats.id -> pa.array.zipWithIndex.flatMap { case (v, i) => if (pa.attribute(i).name.startsWith("Original-p")) Some(v) else None }
          }
          val clusFM = FakeModelRank(clusTRTSRanks.toMap)
          //Default
          val rankMedio = media(tr.toSeq map (_.targets)) //tanto faz tb
          val defaultFM = FakeModelRank((tr ++ ts).map(x => x.id -> rankMedio).toMap)

          val filaDeInserts = mutable.Queue[String]()
          val resres = Vector(RandomRank(seed), clusFM, defaultFM).zip(Vector("rndr", "PCTr", "defr")) flatMap { case (fm, alg) =>
            val spearsTrTs = Seq(tr, ts).zipWithIndex.map { case (tx, idx) =>
              val speaPorComb = mutable.Queue[(String, String, Double)]()
              tx foreach { pat =>
                val (ranking, targets) = fm.output(pat) -> pat.targets
                val r = try {
                  val res = new SpearmansCorrelation().correlation(ranking, targets)
                  val rree = if (res.isNaN) 0d else res
                  rree
                } catch {
                  case x: Throwable => error("\n " + ranking.toList + "\n " + rankMedio.toList + "\n " + targets.toList + " \n" + x)
                }
                speaPorComb += ((targets.map(_.toInt).mkString(" "), (normRank(ranking) map fo).mkString(" "), r))
              }
              speaPorComb
            }
            val spearsTrTsAcc = Seq(tr, ts).zipWithIndex map { case (tx, idx) =>
              val hitPorComb = mutable.Queue[(String, String, Double)]()
              if (porPool && tx.size != 25 && idx == 1) justQuit("cadê os 25 pools?")
              var r = 0
              var f = 0
              tx foreach { pat =>
                if ((r > 4 || f > 4) && idx == 1) justQuit("r>4 ou f>4")
                val esperado = pat.targets.zipWithIndex.minBy(_._1)._2
                val predito = fm.output(pat).zipWithIndex.minBy(_._1)._2
                val hit = if (predito == esperado) 1d else 0d
                hitPorComb += ((esperado.toString, predito.toString, hit))

                //se for cjt de teste
                if (idx == 1) {
                  filaDeInserts += (if (porPool) {
                    val esperadoStr = labels(esperado)
                    val preditoStr = labels(predito)
                    s"insert into e values ('$base', $ks, '$ti', '$tf', '$strat', '$labels', '${alg + "-a"}', '$esperadoStr', '$preditoStr', $r, $f)"
                  } else if (ks == patterns.size) {
                    val esperadoStr = labels(esperado)
                    val preditoStr = labels(predito)
                    s"insert into e values ('$base', $ks, '$ti', '$tf', '$strat', '$labels', '${alg + "-a"}', '$esperadoStr', '$preditoStr', -1, -1)"
                  } else "")
                }

                /*
                val melhorRank = pat.targets.min
                val esperados = pat.targets.zipWithIndex.filter(_ == melhorRank).map(_._2)
                val predito = fm.output(pat).zipWithIndex.minBy(_._1)._2
                // com n>1 (ou n==1 com empates) statisticas p/ maj vão sair maiores que o verdadeiro valor maj, com esse criterio abaixo (n: número de vencedores)
                val hit = if (esperados.contains(predito)) 1d else 0d
                speaPorComb += ((esperado.toString, predito.toString, hit))
                 */

                f += 1
                if (f == 5) {
                  r += 1
                  f = 0
                }
              }
              hitPorComb
            }
            Vector(Resultado(alg + "-a", spearsTrTsAcc.head, spearsTrTsAcc(1)), Resultado(alg, spearsTrTs.head, spearsTrTs(1)))
          }
          if (!readOnly) metads.batchWrite(filaDeInserts.filter(_.nonEmpty).toList)
          //          println(s"${filaDeInserts.filter(_.nonEmpty).size} <- filaDeInserts.filter(_.nonEmpty).size")
          base -> resres.toSeq

        } else {
          val sqls=mutable.Queue[String]()
          val resres = leas(Vector()) map { mc =>
            val (trtest, tstest) = if (mc.querFiltro) {
              val (trfSemParecidos, binaf, zscof) = criaFiltro(trSemParecidos, -1)
              val tsf = aplicaFiltro(ts, -1, binaf, zscof)
              trfSemParecidos -> tsf
            } else trSemParecidos -> ts
            val mo = mc match {
              case Maj() => Maj().build(trtest)
              case RF(_, n, _, _) => RF(seed, n).build(trtest)
              case RoF(_, n) => RoF(seed, n).build(trtest)
              case ABoo(_, n) => ABoo(seed, n).build(trtest)
              case Chute(_) => Chute(seed).build(trtest)
              case PCT(_, _, _) => PCT(ntrees, seed, tr ++ ts).build(trtest) //os testes são em tr+ts, não em trSemParecidos+ts
            }

            if (rank) error("rank")
            if (tstest.size != 25) error("tstest.size!=25")
            Vector(trtest -> (0 until trtest.size).map(x => x -> -1), tstest -> (for (a <- 0 to 4; b <- 0 to 4) yield a -> b)) foreach { case (tx, rfs) =>
              (tx, rfs).zipped foreach { case (pat, (r, f)) =>
                val esperado = pat.nominalLabel.split("-").last
                val pred = mo.predict(pat).toInt
                val predito = pat.classAttribute().value(pred).split("-").last
                if (!porPool) {
                  justQuit("!porPool")
                  sys.exit(0)
                } else {
                  val sql = s"insert into acc values ('${if (trtest == tx) "tr" else "ts"}', '$base', $ks, '$ti', '$tf', '$strat', '$labels', '${mc.limp}', '$esperado', '$predito', '$r', '$f')"
                  println(s"${sql} <- sql ")
                  sqls+=sql
                }
              }
            }
            Resultado("", mutable.Queue(("", "", 0d)), mutable.Queue(("", "", 0d)))
          }
          if (!readOnly) metads.batchWrite(sqls.toList)
          "basefake" -> resres.toSeq
        }
      }.toList
    }.toArray
    metads.close()
    rrr
  }

  def mapZipper(map1: Map[String, Int], map2: Map[String, Int]) = {
    for (key <- map1.keys ++ map2.keys)
      yield (key, map1.getOrElse(key, 0), map2.getOrElse(key, 0))
  }
}

/**
 * Cada entrada nas listas corresponde a um resultado(acc) ou uma combinação ocorrida de ranking(spearman).
 * Pode haver duplicados.
 * (nomeEsperado, nomePredito, valor)
 */
case class Resultado(metalearner: String, valsTr: mutable.Queue[(String, String, Double)], valsTs: mutable.Queue[(String, String, Double)]) {
  val tottr = valsTr.size
  val totts = valsTs.size
  lazy val histTr = pretty(valsTr.groupBy(_._1))
  lazy val histTs = pretty(valsTs.groupBy(_._1))
  lazy val histTrPred = pretty(valsTr.groupBy(_._2))
  lazy val histTsPred = pretty(valsTs.groupBy(_._2))
  lazy val (accTr, accTs) = valsTr.map(_._3).sum / tottr -> valsTs.map(_._3).sum / totts
  //com LOO, algumas das classes de teste não terão nenhuma ocorrência esperada: NaN; então é melhor sumarizar resultados com ++ ou vai estragar as medidas?; ou usar 10-fold e considerar zero
  lazy val (histEsperadoTr, histEsperadoTs) = valsTr.groupBy(_._1).toList.sortBy(_._1).map(_._2.size) -> valsTs.groupBy(_._1).toList.sortBy(_._1).map(_._2.size)
  lazy val (histPreditoTr, histPreditoTs) = valsTr.groupBy(_._2).toList.sortBy(_._1).map(_._2.size) -> valsTs.groupBy(_._2).toList.sortBy(_._1).map(_._2.size)
  lazy val (histAcertosTr, histAcertosTs) = valsTr.groupBy(_._1).toList.sortBy(_._1).map(x => x._2.count(x => x._1 == x._2)) -> valsTs.groupBy(_._1).toList.sortBy(_._1).map(x => x._2.count(x => x._1 == x._2))
  lazy val (histAccsTr, histAccsTs) = histAcertosTr.zip(histEsperadoTr).map(x => x._1 / x._2.toDouble) -> histAcertosTs.zip(histEsperadoTs).flatMap { x =>
    val den = x._2.toDouble
    if (den == 0) None else Some(x._1 / den)
  }
  lazy val resumoTr = histEsperadoTr.take(6).mkString(" ") + "; " + histPreditoTr.take(6).mkString(" ") + "; " + histAcertosTr.take(6).mkString(" ")
  lazy val resumoTs = histEsperadoTs.take(6).mkString(" ") + "; " + histPreditoTs.take(6).mkString(" ") + "; " + histAcertosTs.take(6).mkString(" ")
  lazy val (accBalTr, accBalTs) = histAccsTr.sum / histAccsTr.size -> histAccsTs.sum / histAccsTs.size

  def ++(that: Resultado) = if (that.metalearner != metalearner) ???
  else Resultado(metalearner, that.valsTr ++ valsTr, that.valsTs ++ valsTs)

  def pretty(s: Map[String, mutable.Queue[(String, String, Double)]]) = s.toList.sortBy(_._2.size).reverseMap(x => x._2.size + s" <- " + x._1)
}