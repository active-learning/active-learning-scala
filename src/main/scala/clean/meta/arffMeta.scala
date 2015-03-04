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

package clean.meta

import java.io.FileWriter

import clean.lib._
import ml.Pattern
import ml.classifiers._
import ml.models.ELMModel
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.{Datasets, Stat, StatTests}

import scala.collection.mutable

object arffMeta extends AppWithUsage with StratsTrait with LearnerTrait with RangeGenerator with FilterTrait with Rank {
   /*
   "Rank" => prediz ranking das strats
   "Ties" => prediz vencedores empatados (via multirrótulo no ARFF) com ELM
             se a predição estiver entre os empatados, então acertou
   "TiesDup" => prediz vencedores empatados (via repetição de exemplos no ARFF)
                com learners que façam contagem (NB, C45, KNN, VFDT)
                se a predição estiver entre os empatados, então acertou
   "Winner" => prediz apenas o melhor
   "WinnerDP" => prediz apenas o melhor, mas roda LOO várias vezes sorteando aprendiz; pode retornar Desvio Padrão no futuro
   "Acc" => prediz acc em cada strat com ELM

   Escolher mais abaixo se sorteia learner, budget ou nada.
   Escolher tb quais metaatts fazem parte (suav etc)
   */
   //   val modo = "Rank"
   //      val modo = "Ties"
   //      val modo = "TiesDup"
   val modo = "Winner"
   //   val modo = "WinnerDP"
   val arq = s"/home/davi/wcs/ucipp/uci/metaAcc$modo.arff"
   val context = "metaAttsAccApp"
   val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm")
   val measure = ALCKappa
   //   val measure = ALCBalancedAcc
   val redux = true
   run()

   def ff(x: Double) = (x * 100).round / 100d

   override def run() = {
      super.run()
      val ls = learners(learnersStr)
      val mapaAtts = mutable.Map[Ds, List[Double]]()
      val mapaSuav = mutable.Map[(Ds, Learner), Double]()
      val strats = if (redux) stratsForTreeRedux().dropRight(1) else stratsForTree()
      val ss = strats.map(_.abr).toVector
      val metadata0 = for {
         name <- datasets.toList.take(150).par
         (ti, tf, budix) <- {
            val ds = Ds(name, readOnly = true)
            ds.open()
            //            val tmp = ranges(ds, 2, 200) // <- verificar!!! verificar tb argumentos do programa!!!
            //            ds.close()
            //            Seq(tmp.zipWithIndex.map(x => (x._1._1, x._1._2, x._2)).apply(rnd.nextInt(2)))

            //varia budget(cuidado: válido apenas para TiesDup, Winner ou outro modo que faça o LOO por patterns agrupados)
            val (tmin, thalf, tmax, tpass) = ranges(ds)
            ds.close()
            Seq((tmin, thalf, "\"$\\cent\\leq 50$\""), (tmin, thalf, "baixo"), (thalf, tmax, "alto"))
            //            Seq((tmin, thalf, "baixo"), (thalf, tmax, "alto"))
            //            Seq((tmin, tmax, "alto"))
         }
         l <- if (modo != "WinnerDP") ls else Seq(ls(rnd.nextInt(ls.size)))
      } yield {
         val ds = Ds(name, readOnly = true)
         println(s"${l.abr} $ds")
         ds.open()
         val metaAtts = mapaAtts.getOrElseUpdate(ds, ds.metaAtts)
         lazy val suav = mapaSuav.getOrElseUpdate((ds, l), ds.suavidade(l))
         //escolher se sorteia, fixa ou varia learner (pra variar, comentar abaixo e descomentar mais acima)
         //         val l = allLearners()(rnd.nextInt(allLearners().size)) //warning: estrats de learner único permanecem sempre com seus learners (basicamente ExpELMC)
         //         val l = KNNBatch(5, "eucl", Seq(), weighted = true)

         val seqratts = (for (r <- 0 until Global.runs; f <- 0 until Global.folds) yield ds.attsFromR(r, f)).transpose.map(_.toVector)
         val rattsmd = seqratts map Stat.media_desvioPadrao
         val (rattsm, _) = rattsmd.unzip

         val res = modo match {
//            case "Acc" => //prediz acc em cada strat
//               println(s"filtrar sVMmulti com learner errado");
//               ???
//               val vs = for {
//                  s <- strats
//               } yield {
//                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968 || s.id == 969) s.learner else l
//                  val ms = for {
//                     r <- 0 until Global.runs
//                     f <- 0 until Global.folds
//                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
//                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
//                        NA
//                     }
//                  s.abr -> Stat.media_desvioPadrao(ms.toVector)
//               }
//               if (vs.exists(x => x._2._1 == NA)) Seq()
//               else Seq((ds.metaAtts ++ rattsm, l.abr, "multilabel" + vs.map(_._2._1).mkString(","), budix, l.attPref, l.boundaryType, suav))
            case "TiesDup" => //qualquer metalearner que conte prediz vencedores empatados
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               //                  duplicadorDeAmostra <- 0 to 20
               } yield {
                  val poolStr = (100 * r + f).toString
                  val medidas = for {
                     s <- strats
                     ???
                  } yield if (l.id != 5 && (s.id >= 17 && s.id <= 21 || s.id == 968000 || s.id == 969000)) None
                     else Some(measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                        NA
                     })
                  poolStr -> medidas.flatten
               }
               if (vs.exists(_._2.contains(NA))) ???
               val winners = StatTests.clearWinners(vs, ss)
               ss.map { x =>
                  if (winners.contains(x)) Option(metaAtts ++ rattsm, "na", x, budix, "ambos", "nenhuma", 0d)
                  //                if (winners.contains(x)) Option(metaAtts ++ rattsm, l.abr, x, budix, "ambos", "nenhuma", 0d)
                  //                if (winners.contains(x)) Option(metaAtts ++ rattsm, "na", x, budix, l.attPref, l.boundaryType, suav)
                  else None
               }.flatten
            case "Ties" => //prediz vencedores empatados
               val vs = for {
                  r <- 0 until runs
                  f <- 0 until folds
               //                  duplicadorDeAmostra <- 0 to 1
               } yield {
                  val poolStr = (100 * r + f).toString
                  ???
                  val medidas = for {
                     s <- strats
                     le = if (s.id >= 17 && s.id <= 21 || s.id == 968000 || s.id == 969000) s.learner else l
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                        -2d
                     }
                  poolStr -> medidas
               }
               val winners = StatTests.clearWinners(vs, ss)
               val binario = ss.map(x => if (winners.contains(x)) 1 else 0)
               if (vs.exists(x => x._2.contains(-2d))) Seq()
               else Seq((ds.metaAtts ++ rattsm, "na", "multilabel" + binario.mkString(","), budix, "ambos", "nenhuma", 0d))
            //               else Seq((ds.metaAtts ++ rattsm, l.abr, "multilabel" + binario.mkString(","), budix, "ambos", "nenhuma", 0d))
            //               else Seq((ds.metaAtts ++ rattsm, "na", "multilabel" + binario.mkString(","), budix, l.attPref, l.boundaryType, suav))
            //               else Seq((ds.metaAtts ++ rattsm, "na", "multilabel" + binario.mkString(","), budix, "ambos", "nenhuma", 0d)) //l.attPref, l.boundaryType, suav))
            case "Rank" => //prediz ranking
               val vs = for {
                  s <- strats
               } yield {
                  ???
                  val le = if (s.id >= 17 && s.id <= 21 || s.id == 968000 || s.id == 969000) s.learner else l
                  val ms = for {
                     r <- 0 until Global.runs
                     f <- 0 until Global.folds
                  } yield measure(ds, s, le, r, f)(ti, tf).read(ds).getOrElse {
                        ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, le, r, f)}.", 40)
                        -2d
                     }
                  s.abr -> Stat.media_desvioPadrao(ms.toVector)
               }
               lazy val rank = ranqueia(vs.map(_._2._1))
               if (vs.exists(x => x._2._1 == -2d)) Seq()
               else Seq((ds.metaAtts ++ rattsm, l.abr, "multilabel" + rank.mkString(","), budix, l.attPref, l.boundaryType, suav))
            case "Winner" | "WinnerDP" => //qualquer learner prediz apenas o melhor
               val vs0 = for {
                  s <- strats
                  ???
               } yield if (l.id != 5 && (s.id >= 17 && s.id <= 21 || s.id == 968000 || s.id == 969000)) None
                  else {
                     val ms = for {
                        r <- 0 until Global.runs
                        f <- 0 until Global.folds
                     } yield measure(ds, s, l, r, f)(ti, tf).read(ds).getOrElse {
                           ds.log(s" base incompleta para intervalo [$ti;$tf] e pool ${(s, l, r, f)}.", 40)
                           NA
                        }
                     Some(s.abr -> Stat.media_desvioPadrao(ms.toVector))
                  }
               val vs = vs0.flatten
               if (vs.exists(_._2._1 == NA)) ???
               //                                             else Seq((metaAtts ++ rattsm, "na", vs.maxBy(_._2._1)._1, budix, "ambos", "nenhuma", 0d))
               //               else Seq((metaAtts ++ rattsm, l.abr, vs.maxBy(_._2._1)._1, budix, "ambos", "nenhuma", 0d))
               //                           else Seq((metaAtts ++ rattsm, "na", vs.maxBy(_._2._1)._1, budix, l.attPref, l.boundaryType, suav))
               else Seq((metaAtts ++ rattsm, l.abr, vs.maxBy(_._2._1)._1, budix, l.attPref, l.boundaryType, suav))
         }

         ds.close()
         res
      }
      val metadata = metadata0.flatten.toList

      //cria ARFF
      val pred = metadata.map(_._3)
      val labels = pred.distinct.sorted
      val data = metadata.map { case (numericos, learner, vencedores, budget, attPref, boundaryType, suavidade) =>
         numericos.mkString(",") + s",$budget,$learner,$attPref,$boundaryType,$suavidade," + "\"" + vencedores + "\""
      }
      val header = List("@relation data") ++
         nonHumanNumAttsNames.split(",").map(i => s"@attribute $i numeric") ++
         List("@attribute \"orçamento\" {\"$\\cent\\leq 50$\",baixo,alto}",
            "@attribute aprendiz {" + ls.map(x => "\"" + x.abr + "\"").mkString(",") + ",na}",
            "@attribute \"atributo aceito\" {\"numérico\",\"nominal\",\"ambos\"}",
            "@attribute \"fronteira\" {\"rígida\",\"flexível\",\"nenhuma\"}",
            "@attribute suavidade numeric",
            "@attribute class {" + labels.map(x => "\"" + x + "\"").mkString(",") + "}", "@data")

      val pronto = header ++ data

      println(s"$arq")
      val fw = new FileWriter(arq)
      pronto foreach (x => fw.write(s"$x\n"))
      fw.close()
      pronto foreach println

      //processa arff
      println(s"${data.size} metaexemplos")
      println(labels.size + " pseudoclasses")

      //NB, C45, KNN, VFDT
      Datasets.arff(arq, dedup = false) match {
         case Left(str) => error("problemas abrindo arff:" + str)
         case Right(patterns) =>
            val grupos = patterns.groupBy(x => x.vector.dropRight(5)).map(_._2).toArray
            print(s" qtd de grupos = ${grupos.size} ")
            val accs = Datasets.LOO(grupos) { (tr: Seq[Vector[Pattern]], ts: Vector[Pattern]) =>
               modo match {
                  case "Rank" | "Ties" =>
                     //                  println(s"Pool $run.$fold (${tr.size} instances) ...")
                     val learnerSeed = 0
                     val (fpool, binaf, zscof) = criaFiltro(tr.flatten, 1)
                     val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
                     val l = NinteraELM(learnerSeed)
                     var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
                     m = l.modelSelectionFull(m)
                     val rankMedio = media(ftestSet.toSeq map (p => p.nominalSplit))
                     val hitsELMeSpears = ftestSet map { p =>
                        val spear = new SpearmansCorrelation().correlation(m.output(p), p.nominalSplit)
                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio, p.nominalSplit)
                        (if (p.nominalSplit(m.predict(p).toInt) == p.nominalSplit.max) 1d else 0d, (spear, spearMaj))
                     }
                     val (hitsELM, spears) = hitsELMeSpears.unzip
                     val (spearELM, spearMaj) = spears.unzip
                     val hitsMaj = ftestSet map { p => if (p.nominalSplit(2) == p.nominalSplit.max) 1d else 0d}
                     modo match {
                        case "Rank" => Seq(Stat.media_desvioPadrao(spearELM)._1, Stat.media_desvioPadrao(spearMaj)._1)
                        case "Ties" => Seq(Stat.media_desvioPadrao(hitsELM)._1, Stat.media_desvioPadrao(hitsMaj)._1)
                     }
                  case "Winner" | "WinnerDP" | "TiesDup" =>
                     val ls = Seq(C45(),
                        NBBatch(),
                        //                  KNNBatch(5, "eucl", tr.flatten, weighted = true),
                        KNNBatchb(5, "eucl", tr.flatten, weighted = false),
                        //                  KNNBatch(50, "eucl", tr.flatten, weighted = true),
                        //                  SVMLib(), //não tira proveito de exemplos duplicados
                        //                  NinteraELM(), //não tira proveito de exemplos duplicados
                        Maj())
                     val trios = ls map {
                        case l: NinteraELM =>
                           val (fpool, binaf, zscof) = criaFiltro(tr.flatten, 1)
                           val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
                           var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
                           m = l.modelSelectionFull(m)
                           (m, ftestSet)
                        case l =>
                           val m = l.build(tr.flatten)
                           (m, ts)
                     }
                     modo match {
                        case "TiesDup" => trios map { case (m, tes) =>
                           val hitsByDupGroup = tes.groupBy(x => x) map { case (pat, pats) =>
                              if (pats.map(_.label).contains(m.predict(pat))) 1d else 0d
                           }
                           hitsByDupGroup.sum / hitsByDupGroup.size
                        }
                        case "Winner" | "WinnerDP" => trios map { case (m, tes) =>
                           m.accuracy(tes)
                        }
                     }
               }
            }
            print(accs.transpose.map { x =>
               val (m, d) = Stat.media_desvioPadrao(x.toVector)
               "%5.3f".format(m) + "/" + "%5.3f".format(d)
            }.mkString(" "))
            print(s" qtd de grupos = ${grupos.size} ")
      }
      println(s"qtd de metaexemplos: ${data.size} $modo")
   }
}