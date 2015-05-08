///*
// active-learning-scala: Active Learning library for Scala
// Copyright (c) 2014 Davi Pereira dos Santos
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package clean.meta
//
//import java.io.FileWriter
//
//import al.strategies._
//import clean.lib._
//import ml.Pattern
//import ml.classifiers._
//import ml.models.ELMModel
//import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
//import util.{Datasets, Stat}
//
//import scala.collection.mutable
//import scala.util.Random
//
//object arffMetaPares2steps extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator with FilterTrait with Rank {
//   val context = "metaPares2stepsApp"
//   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
//   val n = 1
//   //qs: 50 100 50ou100
//   val qs = "100"
//   //modo: dup rank
//   val modo = "acc"
//   val arq = s"/home/davi/wcs/ucipp/uci/metaPares2steps$modo$qs.arff"
//   val measure = Kappa
//   run()
//
//   def ff(x: Double) = (x * 100).round / 100d
//
//   override def run() = {
//      super.run()
//      val mapaAtts = mutable.Map[Ds, List[String]]()
//      val metadata0 = (for {
//         dataset <- datasets.toList.filter { dataset =>
//            val ds = Ds(dataset, readOnly = true)
//            ds.open()
//            val r = ds.poolSize >= (if (qs == "50") 100 else 200)
//            ds.close()
//            if (qs == "u2") !r else r
//         }
//         b <- qs match {
//            case "50ou100" => Seq(50, 100)
//            case "u2" | "50" | "100" => Seq(0)
//         }
//      } yield {
//         val ds = Ds(dataset, readOnly = true)
//         ds.open()
//         val metaAtts = mapaAtts.getOrElseUpdate(ds, ds.metaAtts.map(_.toString))
//         val seqratts = (for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield ds.metaAttsFromR(r, f)).transpose.map(_.toVector)
//         val rattsmd = seqratts map Stat.media_desvioPadrao
//         val (rattsm0, rattsd) = rattsmd.unzip
//         val rattsm = rattsm0.map(_.toString)
//
//         val classifs = (for {
//            r <- 0 until runs
//            f <- 0 until folds
//         } yield {
//            val classif = qs match {
//               case "50ou100" if b == 50 => BestClassifCV50_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))
//               case "50ou100" if b == 100 => BestClassifCV100_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))
//               case "u2" => BestClassifCVU2_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))
//               case "50" => BestClassifCV50_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))
//               case "100" => BestClassifCV100_10foldReadOnlyKappa(ds, r, f, RandomSampling(Seq()))
//            }
//            classif.limpa
//         }).toList
//         val hist = classifs.groupBy(x => x).map(x => x._1 -> x._2.size).toSeq
//         //         lazy val rank = "multilabel" + ranqueia(sres.map(_._2)).mkString(",")
//         val res = modo match {
//            case "acc" => pegaMelhores(hist, n)(_._2).map(_._1) map (x => metaAtts ++ rattsm :+ b.toString :+ "\"" + x + "\"")
//            //            case "Spear" => Seq(metaAtts ++ rattsm :+ b.toString :+ "\"" + rank + "\"")
//            //            case "acc" => pegaMelhores(sres, n)(_._2).map(_._1) map (x => metaAtts.map(_ => "0") ++ rattsm :+ b.toString :+ "\"" + x + "\"")
//            //            case "Spear" => Seq(metaAtts.map(_ => "0") ++ rattsm :+ b.toString :+ "\"" + rank + "\"")
//            //            case "acc" => pegaMelhores(sres, n)(_._2).map(_._1) map (x => metaAtts ++ rattsm.map(_ => "0") :+ b.toString :+ "\"" + x + "\"")
//            //            case "Spear" => Seq(metaAtts ++ rattsm.map(_ => "0") :+ b.toString :+ "\"" + rank + "\"")
//         }
//         ds.close()
//         res
//      }).flatten
//      val metadata = metadata0.toList
//
//      //cria ARFF
//      val pred = metadata map (_.last)
//      val labels = pred.distinct.sorted
//      val data = metadata map (x => x.mkString(","))
//      val header = List("@relation data") ++
//         (nonHumanNumAttsNames + "," + attsFromRNames).split(",").map(i => "@attribute " + i + " numeric") ++
//         List("@attribute budget numeric", "@attribute class {" + labels.mkString(",") + "}", "@data")
//      val pronto = header ++ data
//      println(s"$arq")
//      val fw = new FileWriter(arq)
//      pronto foreach (x => fw.write(s"$x\n"))
//      fw.close()
//      pronto foreach println
//
//      //processa arff
//      println(s"${data.size} metaexemplos; ${metadata.head.size} metaatributos")
//      println(labels.size + " pseudometaclasses")
//
//      //NB, C45, KNN, VFDT
//      Datasets.arff(arq, dedup = false) match {
//         case Left(str) => error("problemas abrindo arff:" + str)
//         case Right(patterns) =>
//            val grupos = new Random(1740).shuffle(patterns.groupBy(x => x.vector.dropRight(1)).map(_._2)).toArray
//            //            val grupos = patterns.groupBy(x => x.vector.dropRight(1)).map(_._2).toArray
//            print(s" qtd de grupos = ${grupos.size} ")
//            val accs = Datasets.kfoldCV2(grupos.toSeq.map(_.toVector), 10) { (tr0: Seq[Vector[Pattern]], ts0: Seq[Vector[Pattern]], fold, min) =>
//               val tr = tr0.flatten
//               val ts = ts0.flatten
//               modo match {
//                  case "acc" =>
//                     val ls = Seq(
//                        C45(), //C45(laplace = false,12),
//                        RF(50),
//                        NBBatch(),
//                        KNNBatcha(5, "eucl", tr, weighted = true),
//                        KNNBatcha(5, "eucl", tr, weighted = false),
//                        KNNBatchb(5, "eucl", tr, weighted = true),
//                        SVMLibRBF(), //não tira proveito de exemplos duplicados
//                        NinteraELM(), //não tira proveito de exemplos duplicados
//                        Maj())
//                     val trios = ls map {
//                        case l: NinteraELM =>
//                           val (fpool, binaf, zscof) = criaFiltro(tr, 1)
//                           val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
//                           var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
//                           m = l.modelSelectionFull(m)
//                           (m, ftestSet)
//                        case l: SVMLibRBF =>
//                           val (fpool, binaf, zscof) = criaFiltro(tr, 1)
//                           val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
//                           val m = l.build(fpool)
//                           (m, ftestSet)
//                        case l =>
//                           val m = l.build(tr)
//                           (m, ts)
//                     }
//                     trios map { case (m, tes) =>
//                        val hitsByDupGroup = tes.groupBy(x => x.vector) map { case (pat, pats) =>
//                           if (pats.map(_.label).contains(m.predict(pats.head))) 1d else 0d
//                        }
//                        hitsByDupGroup.sum / hitsByDupGroup.size
//                     }
//                  //                  case "Spear" =>
//                  //                     val learnerSeed = 0
//                  //                     val (fpool, binaf, zscof) = criaFiltro(tr, 1)
//                  //                     val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
//                  //                     val l = NinteraELM(learnerSeed)
//                  //                     var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
//                  //                     m = l.modelSelectionFull(m)
//                  //                     val rankMedio = media(tr.toSeq map (p => p.nominalSplit))
//                  //                     val twoSpears = ftestSet map { p =>
//                  //                        val spear = new SpearmansCorrelation().correlation(m.output(p), p.nominalSplit)
//                  //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio, p.nominalSplit)
//                  //                        //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio.zipWithIndex.map(_._2.toDouble), p.nominalSplit)
//                  //                        (spear, spearMaj)
//                  //                     }
//                  //                     val (spearELM, spearMaj) = twoSpears.unzip
//                  //                     Seq(Stat.media_desvioPadrao(spearELM)._1, Stat.media_desvioPadrao(spearMaj)._1)
//               }
//            }
//            //            val accs = Datasets.LOO(grupos) { (tr0: Seq[Vector[Pattern]], ts: Vector[Pattern]) =>
//            //               val tr = new Random(tr0.flatten.map(_.id).sum).shuffle(tr0.flatten)
//            //               modo match {
//            //                  case "acc" =>
//            //                     val ls = Seq(
//            //                        C45(),
//            //                        //                        RF(50),
//            //                        //                        NBBatch(),
//            //                        //                        KNNBatcha(5, "eucl", tr.flatten, weighted = true),
//            //                        //                        KNNBatcha(5, "eucl", tr.flatten, weighted = false),
//            //                        //                        KNNBatchb(5, "eucl", tr.flatten, weighted = true),
//            //                        //                        SVMLibRBF(), //não tira proveito de exemplos duplicados
//            //                        //                        NinteraELM(), //não tira proveito de exemplos duplicados
//            //                        Maj())
//            //                     val trios = ls map {
//            //                        case l: NinteraELM =>
//            //                           val (fpool, binaf, zscof) = criaFiltro(tr, 1)
//            //                           val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
//            //                           var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
//            //                           m = l.modelSelectionFull(m)
//            //                           (m, ftestSet)
//            //                        case l: SVMLibRBF =>
//            //                           val (fpool, binaf, zscof) = criaFiltro(tr, 1)
//            //                           val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
//            //                           val m = l.build(fpool)
//            //                           (m, ftestSet)
//            //                        case l =>
//            //                           val m = l.build(tr)
//            //                           (m, ts)
//            //                     }
//            //                     trios map { case (m, tes) =>
//            //                        val hitsByDupGroup = tes.groupBy(x => x.vector.dropRight(1)) map { case (pat, pats) =>
//            //                           if (pats.map(_.label).contains(m.predict(pats.head))) 1d else 0d
//            //                        }
//            //                        hitsByDupGroup.sum / hitsByDupGroup.size
//            //                     }
//            //
//            //                  case "Spear" =>
//            //                     val learnerSeed = 0
//            //                     val (fpool, binaf, zscof) = criaFiltro(tr, 1)
//            //                     val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
//            //                     val l = NinteraELM(learnerSeed)
//            //                     var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
//            //                     m = l.modelSelectionFull(m)
//            //                     val rankMedio = media(tr.toSeq map (p => p.nominalSplit))
//            //                     val twoSpears = ftestSet map { p =>
//            //                        val spear = new SpearmansCorrelation().correlation(m.output(p), p.nominalSplit)
//            //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio, p.nominalSplit)
//            //                        //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio.zipWithIndex.map(_._2.toDouble), p.nominalSplit)
//            //                        (spear, spearMaj)
//            //                     }
//            //                     val (spearELM, spearMaj) = twoSpears.unzip
//            //                     Seq(Stat.media_desvioPadrao(spearELM)._1, Stat.media_desvioPadrao(spearMaj)._1)
//            //               }
//            //            }
//            //            //            val accs = Datasets.LTO(grupos) { (tr: Seq[Vector[Pattern]], ts: Seq[Vector[Pattern]]) =>
//            //            //               modo match {
//            //            //                  case "acc" =>
//            //            //                     val ls = Seq(
//            //            //                        RF(),
//            //            //                        //                        C45(),
//            //            //                        //                                                NBBatch(),
//            //            //                        //                                                                  KNNBatchb(5, "eucl", tr.flatten, weighted = true),
//            //            //                        //                                                KNNBatcha(5, "eucl", tr.flatten, weighted = true),
//            //            //                        //                        //                  KNNBatch(50, "eucl", tr.flatten, weighted = true),
//            //            //                        //                        //                  SVMLib(), //não tira proveito de exemplos duplicados
//            //            //                        Maj())
//            //            //                     val trios = ls map {
//            //            //                        case l: NinteraELM =>
//            //            //                           val (fpool, binaf, zscof) = criaFiltro(tr.flatten, 1)
//            //            //                           val ftestSet = aplicaFiltro(ts.flatten, 1, binaf, zscof)
//            //            //                           var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
//            //            //                           m = l.modelSelectionFull(m)
//            //            //                           (m, ftestSet)
//            //            //                        case l =>
//            //            //                           val m = l.build(tr.flatten)
//            //            //                           (m, ts.flatten)
//            //            //                     }
//            //            //                     trios map { case (m, tes) =>
//            //            //                        val hitsByDupGroup = tes.groupBy(x => x) map { case (pat, pats) =>
//            //            //                           if (pats.map(_.label).contains(m.predict(pat))) 1d else 0d
//            //            //                        }
//            //            //                        hitsByDupGroup.sum / hitsByDupGroup.size
//            //            //                     }
//            //            //
//            //            //                  case "Spear" =>
//            //            //                     val learnerSeed = 0
//            //            //                     val (fpool, binaf, zscof) = criaFiltro(tr.flatten, 1)
//            //            //                     val ftestSet = aplicaFiltro(ts.flatten, 1, binaf, zscof)
//            //            //                     val l = NinteraELM(learnerSeed)
//            //            //                     var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
//            //            //                     m = l.modelSelectionFull(m)
//            //            //                     val rankMedio = media(ftestSet.toSeq map (p => p.nominalSplit))
//            //            //                     val twoSpears = ftestSet map { p =>
//            //            //                        val spear = new SpearmansCorrelation().correlation(m.output(p), p.nominalSplit)
//            //            //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio, p.nominalSplit)
//            //            //                        spear -> spearMaj
//            //            //                     }
//            //            //                     val (spearELM, spearMaj) = twoSpears.unzip
//            //            //                     Seq(Stat.media_desvioPadrao(spearELM)._1, Stat.media_desvioPadrao(spearMaj)._1)
//            //            //               }
//            //            //            }
//            println(s"")
//            println(s"$modo\t\tdesvio\n" + accs.transpose.map { x =>
//               val (m, d) = Stat.media_desvioPadrao(x.toVector)
//               "%5.30f".format(m) + "\t\t" + "%5.3f".format(d)
//            }.mkString("\n"))
//            println(s" qtd de grupos = ${grupos.size} ")
//
//         //            val wil = accs.map(x => ("a", x(0)) ->("b", x(1)))
//         //            println(wil)
//         //            println(Stat.wilcoxon(wil))
//      }
//      println(s"qtd de metaexemplos: ${data.size}")
//   }
//}