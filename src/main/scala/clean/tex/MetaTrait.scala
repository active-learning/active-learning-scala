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

import clean.lib.{FilterTrait, Log, Rank}
import clus.Clus
import ml.{PatternParent, Pattern}
import ml.classifiers._
import ml.models.{RandomRank, FakeModelRank, EnsembleModel, ELMModel}
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


  def cv(pct: Double, smote: Boolean, ntrees: Int, attsel: String, patterns: Vector[Pattern], leas: Vector[Pattern] => Vector[Learner], rank: Boolean, rs: Int, ks: Int) = {
    //id serve pra evitar conflito com programas paralelos
    val id = "_id" + UUID.randomUUID() + patterns.map(_.id).mkString.hashCode + System.currentTimeMillis.hashCode
    (1 to rs).par map { run =>
      val shuffled = new Random(run).shuffle(patterns)
      val bags = shuffled.groupBy(_.base).values.toVector

      Datasets.kfoldCV2(bags, ks, parallel = true) { (trbags, tsbags, fold, minSize) =>
        //seed tem sobreposição acima de 100 folds
        if (ks >= 100) ???
        val seed = run * 100 + fold

        val (tr0, ts0) = trbags.flatten.toVector -> tsbags.flatten.toVector

        val (tr, ts) = if (rank) tr0 -> ts0
        else {
          val f = Datasets.removeBagFilter(tr0)
          val (tr00, ts00) = Datasets.applyFilter(f)(tr0) -> Datasets.applyFilter(f)(ts0)
          tr00 -> ts00
        }

        lazy val (trf, tsf) = replacemissingNom2binRmuselessZscore(tr, ts)
        //pega apenas um ex. por base (um que tiver label mais frequente)
        lazy val tr_trfSemParecidos = Seq(tr0.zip(tr).groupBy(_._1.base).map(_._2.map(_._2)), tr0.zip(trf).groupBy(_._1.base).map(_._2.map(_._2))) map { bags =>
          bags map meanPattern(rank)
        }
        lazy val (trSemParecidos, trfSemParecidos) = tr_trfSemParecidos.head.toVector -> tr_trfSemParecidos(1).toVector

        if (leas(tr).isEmpty) {
          //ELMBag
          val elmFM = (1 to ntrees).foldLeft(FakeModelRank(Map())) { (fm, seedinc) =>
            val l = NinteraELM(seed + seedinc * 10000)
            //selecionar com todos foi pior (e bem mais lento) que tirando similares 38.6 < 44.0
            var m0 = l.batchBuild(trfSemParecidos).asInstanceOf[ELMModel]
            val L = l.LForMeta(m0, LOO = false)
            //treinar com todos foi melhor que tirando similares 44.0 > 42.5 (subamostras não melhorou muito, então nem deixei)
            m0 = l.batchBuild(trf).asInstanceOf[ELMModel]
            m0 = l.fullBuildForMeta(L, m0)
            fm + FakeModelRank(((trf ++ tsf) map (x => x.id -> m0.output(x).clone())).toList.toMap) //array.clone is needed to free FM object
          }

          //clus; seed tb serve pra situar run e fold durante paralelização
          val arqtr = s"/run/shm/tr$seed$id"
          val arqtrts = s"/run/shm/trts$seed$id"
          patts2file(trSemParecidos, arqtr) //sem redundantes: 48/54; com todos 43/44
          patts2file(tr ++ ts, arqtrts) //coleta predições de tr e ts num só arquivo
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
          val rankMedio = media(tr.toSeq map (_.targets))
          val defaultFM = FakeModelRank((tr ++ ts).map(x => x.id -> rankMedio).toMap)

          def fo(x: Double) = "%2.1f".format(x)

          Vector(RandomRank(seed), clusFM, elmFM, defaultFM, clusFM.normalized + elmFM.normalized).zip(Vector("rndr", "PCTr", "ELMr", "defr", "PEr")) flatMap { case (fm, alg) =>
            val spearsTrTs = Seq(tr, ts).map { tx =>
              val speaPorComb = mutable.Queue[(String, String, Double)]()
              tx foreach { pat =>
                val (ranking, targets) = fm.output(pat) -> pat.targets
                val r = try {
                  val res = new SpearmansCorrelation().correlation(ranking, targets)
                  if (res.isNaN) 0d
                  //justQuit("\n\n\nNaN no Spear:" + ranking.toList + " " + targets.toList + "\n\n\n")
                  else res
                } catch {
                  case x: Throwable => error("\n " + ranking.toList + "\n " + rankMedio.toList + "\n " + targets.toList + " \n" + x)
                }
                speaPorComb += ((targets.map(_.toInt).mkString(" "), (normRank(ranking) map fo).mkString(" "), r))
              }
              speaPorComb
            }
            val spearsTrTsAcc = Seq(tr, ts).map { tx =>
              val speaPorComb = mutable.Queue[(String, String, Double)]()
              tx foreach { pat =>
                val esperado = pat.targets.zipWithIndex.minBy(_._1)._2
                val predito = fm.output(pat).zipWithIndex.minBy(_._1)._2
                val hit = if (predito == esperado) 1d else 0d
                speaPorComb += ((esperado.toString, predito.toString, hit))
                /*
                val melhorRank = pat.targets.min
                val esperados = pat.targets.zipWithIndex.filter(_ == melhorRank).map(_._2)
                val predito = fm.output(pat).zipWithIndex.minBy(_._1)._2
                // com n>1 (ou n==1 com empates) statisticas p/ maj vão sair maiores que o verdadeiro valor maj, com esse criterio abaixo (n: número de vencedores)
                val hit = if (esperados.contains(predito)) 1d else 0d
                speaPorComb += ((esperado.toString, predito.toString, hit))
                 */
              }
              speaPorComb
            }
            Vector(Resultado(alg + "-a", spearsTrTsAcc.head, spearsTrTsAcc(1)), Resultado(alg, spearsTrTs.head, spearsTrTs(1)))
          }

        } else {
          //usando terminação fs pra indicar filtro
          val (trfs, trffs, tsfs, tsffs, trfSemParecidos1fs) = if (smote) {
            val sm = new SMOTE()
            sm.setDebug(false)
            sm.setDoNotCheckCapabilities(true)
            sm.setInputFormat(Datasets.patterns2instances(tr))
            sm.setRandomSeed(seed)
            sm.setPercentage(pct)
            val r = Seq(tr) map Datasets.applyFilterIdRnd(sm)

            val smf = new SMOTE()
            smf.setDebug(false)
            smf.setDoNotCheckCapabilities(true)
            smf.setInputFormat(Datasets.patterns2instances(trf))
            smf.setRandomSeed(seed)
            smf.setPercentage(pct)
            val rf = Seq(trf, trfSemParecidos) map Datasets.applyFilterIdRnd(smf)

            (r(0), rf(0), ts, tsf, rf(1))
          } else attsel.splitAt(3) match {
            case ("pca", comps) =>
              val pca = new PrincipalComponents()
              pca.setDebug(false)
              pca.setDoNotCheckCapabilities(true)
              pca.setInputFormat(Datasets.patterns2instances(tr))
              pca.setMaximumAttributes(comps.toInt)
              val seq = Seq(tr, ts) map Datasets.applyFilterIdRnd(pca)

              val pcaf = new PrincipalComponents()
              pcaf.setDebug(false)
              pcaf.setDoNotCheckCapabilities(true)
              pcaf.setInputFormat(Datasets.patterns2instances(trf))
              pcaf.setMaximumAttributes(comps.toInt)
              val seqf = Seq(trf, tsf, trfSemParecidos) map Datasets.applyFilterIdRnd(pcaf)

              (seq(0), seqf(0), seq(1), seqf(1), seqf(2))

            case ("fs", _) =>
              val att = new AttributeSelection
              val attf = new AttributeSelection
              val eval = new WrapperSubsetEval
              val evalf = new WrapperSubsetEval
              val data = Seq(tr, ts) map Datasets.patterns2instancesId
              val dataf = Seq(trf, tsf, trfSemParecidos) map Datasets.patterns2instancesId
              val sample = Datasets.patterns2instancesId(tr)
              val samplef = Datasets.patterns2instancesId(trf)
              eval.buildEvaluator(sample)
              evalf.buildEvaluator(samplef)
              //            val cla = new RandomForest
              //            val claf = new RandomForest
              val cla = new IBk
              val claf = new IBk
              //            cla.setDoNotCheckCapabilities(true)
              //            claf.setDoNotCheckCapabilities(true)
              //            cla.setNumTrees(20)
              //            claf.setNumTrees(20)
              cla.setKNN(5)
              claf.setKNN(5)
              eval.setFolds(10)
              eval.setClassifier(cla)
              eval.setThreshold(0.01)
              eval.setSeed(fold * run)
              evalf.setFolds(10)
              evalf.setClassifier(claf)
              evalf.setThreshold(0.01)
              evalf.setSeed(fold * run)
              att.setEvaluator(eval)
              attf.setEvaluator(evalf)
              val sea = new BestFirst()
              sea.setLookupCacheSize(10)
              //            sea.setSearchTermination(3)
              val seaf = new BestFirst()
              seaf.setLookupCacheSize(10)
              //            seaf.setSearchTermination(3)
              /*
                      Searches the space of attribute subsets by greedy hillclimbing augmented with a backtracking facility.
                      Setting the number of consecutive non-improving nodes allowed controls the level of backtracking done.
                      Best first may start with the empty set of attributes and search forward, or start with the full set of
                      attributes and search backward, or start at any point and search in both directions (by considering all
                      possible single attribute additions and deletions at a given point).
                       */
              sea.setStartSet("1")
              seaf.setStartSet("1")
              att.setSearch(sea)
              attf.setSearch(seaf)
              att.SelectAttributes(sample)
              attf.SelectAttributes(samplef)
              println(s"${att.selectedAttributes().toList} <- att.selectedAttributes()")
              println(s"${attf.selectedAttributes().toList} <- attf.selectedAttributes()")
              val seq = data map (x => Datasets.instances2patternsId(att.reduceDimensionality(x)).toVector)
              val seqf = dataf map (x => Datasets.instances2patternsId(attf.reduceDimensionality(x)).toVector)
              (seq(0), seqf(0), seq(1), seqf(1), seqf(2))
            case _ => (tr, trf, ts, tsf, trfSemParecidos)
          }

          leas(trfs) map { le =>
            val (trtestbags, tstestbags, m) = if (le.querFiltro) {
              val mo = le match {
                case NinteraELM(_, _) =>
                  //ELMBag
                  (1 to ntrees).foldLeft(FakeModelRank(Map())) { (fm, seedinc) =>
                    val l = NinteraELM(seed + seedinc * 10000)
                    //pega apenas a média dos exs. de cada base
                    //foi melhor filtrar: 41,7 > 36,9
                    var m0 = l.batchBuild(trfSemParecidos1fs).asInstanceOf[ELMModel]
                    val L = l.LForMeta(m0, LOO = false)
                    //41,7 > 39,3 (subamostragem não ajudou muito)
                    //new Random(seed + seedinc * 10001).shuffle(trffs).take((trffs.size ).round.toInt)
                    m0 = l.batchBuild(trffs).asInstanceOf[ELMModel]
                    l.fullBuildForMeta(L, m0)
                    fm + FakeModelRank(((trffs ++ tsffs) map (x => x.id -> m0.output(x).clone())).toList.toMap) //array.clone is needed(?) to free FM object
                  }
                case SVMLibRBF(_) => SVMLibRBF(seed).build(trffs) //SVM fica um pouco mais rápida sem exemplos redundantes, mas 42,5 > 33,1
                case PCTELM(_, _, _) => PCTELM(ntrees, seed, (trffs ++ tsffs).toVector).build(trffs)
                case _ => le.build(trffs)
              }
              (trffs.groupBy(x => x.id), tsffs.groupBy(x => x.id), mo)
            } else {
              val mo = le match {
                case RF(_, n, _, _) => RF(seed, n).build(trfs)
                case Chute(_) => Chute(seed).build(trfs)
                case PCT(_, _, _) => PCT(ntrees, seed, trfs ++ tsfs).build(trfs)
                case _ => le.build(trfs)
              }
              (trfs.groupBy(x => x.id), tsfs.groupBy(x => x.id), mo)
            }
            val tr_ts = Vector(trtestbags, tstestbags) map { bags =>
              val resPorClasse = mutable.Queue[(String, String, Double)]()
              bags.map(_._2) foreach { xbag =>
                // com n>1 (ou n==1 com empates) statisticas p/ maj vão sair maiores que o verdadeiro valor maj, com esse criterio abaixo (n: número de vencedores)
                val esperado = xbag.head.nominalLabel
                val pred = m.predict(xbag.head).toInt
                val re = if (xbag.map(_.label).contains(pred)) 1d else 0d
                resPorClasse += ((esperado, xbag.head.classAttribute.value(pred), re))
              }
              resPorClasse
              /*
                val resPorClasse = mutable.Queue[(Int, Int, Option[Double])]()
                bags.map(_._2) foreach { xbag =>
                  val esperados = xbag.map(_.label.toInt).sorted
                  val pred = m.predict(xbag.head).toInt
                  val re = if (esperados.contains(pred)) 1d else 0d
                  resPorClasse += ((esperados.head, pred, Some(re)))
                  esperados foreach { esperado =>
                    resPorClasse += ((esperado, xbag.head.classAttribute.value(pred), None))
                  }
                }
                resPorClasse
               */
            }
            Resultado(le.limpa, tr_ts.head, tr_ts(1))
          }
        }
      }
    }
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