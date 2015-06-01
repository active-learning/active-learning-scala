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

import java.io.{File, FileWriter}
import ml.Pattern

import scala.util.Random
import al.strategies._
import clean.lib._
import ml.classifiers._
import util.{Datasets, Stat}

object metaParesByPool extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with MetaTrait {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "rank")
  val context = this.getClass.getName.split('.').last.dropRight(1)
  //n=3 ajuda levemente se for  classif
  val n = 1
  val dedup = false
  // 50 100 u2 - só tem 100 por enquanto pra não-ALC
  //1 ou -1
  val featureSel = false
  //  val measure = Kappa
  val measure = ALCKappa
  //1 100 (!= 100, só para ALC)
  val dsminSize = 1
  val qs = "200"
  val (rus, ks) = 1 -> 10
  //melhores 1; ou piores -1
  val melhor = 1
  run()

  override def run() = {
    super.run()
    val ls = learners(learnersStr)
    //    val ss = stratsTexRedux("eucl")
    //                val ss = stratsTex("all")
    //        val ss = Seq(
    //          (l: Learner) => MarginFixo(l, Seq()),
    //          (l: Learner) => ExpErrorReductionMarginFixo(l, Seq(), "entropy")
    //        )
    val ss = Seq((l: Learner) => HTUFixo(Seq(), l, Seq(), "maha"))
    //      //          HTUFixo(Seq(), RF(), Seq(), "eucl")
    //      //          DensityWeightedTrainingUtilityFixo(Seq(), RF(), Seq(), "eucl")
    //      //      AgDensityWeightedTrainingUtility(Seq(), "eucl"),
    //      //      ClusterBased(Seq()),
    //      //      RandomSampling(Seq())
    //    )
    val pares = for {l <- ls; s <- ss} yield s -> l
    //    $rus.$ks
    val arq = s"/home/davi/wcs/arff/$context-n${n}best${melhor}m$measure${qs}qs-${pares.map { case (s, l) => s(l).id }.mkString("-").hashCode.toLong.abs + (if (porRank) "Rank" else "")}-${learnerStr.replace(" ", ".")}-p$dsminSize.arff"
    val labels = pares.map { case (s, l) => s(l).limpa }

    //cada dataset produz um bag de metaexemplos (|bag| >= 25)
    def bags = DsByMinSize(datasets, dsminSize).par map { d =>
      val ds = Ds(d, readOnly = true)
      ds.open()
      val (ti, th, tf, tpass) = ranges(ds)
      val res = for {
        r <- 0 until runs
        f <- 0 until folds
      } yield {
          //descobre vencedores deste pool
          val accs = pares map { case (s, l) =>

            //            p -> measure(ds, Passive(Seq()), ds.bestPassiveLearner, r, f)(ti,tf).read(ds).getOrElse(error("sem medida"))

            s(l) -> measure(ds, s(l), l, r, f)(ti, tf).read(ds).getOrElse(error("sem medida"))

            //            val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s(l))
            //            s(l) -> measure(ds, s(l), classif, r, f)(-2).read(ds).getOrElse(error("sem medida"))
          }
          //gera metaexemplos
          val metaatts0 = ds.metaAttsrf(r, f).map(x => (x._1, x._2.toString, x._3)) ++ ds.metaAttsFromR(r, f).map(x => (x._1, x._2.toString, x._3))
          val metaatts = ("\"bag_" + pares.size + "\"", ds.dataset, "string") +: metaatts0
          if (porRank) {
            //rank legivel por clus e ELM
            List(metaatts ++ ranqueia(accs.map(_._2)).zipWithIndex.map { case (x, i) => (s"class$i", x.toString, "numeric") } -> "")

            //inutil
            //            List(metaatts ++ ranqueia(accs.map(_._2)).zipWithIndex.map { case (x, i) => (s"class$i", x, "{" + labels.mkString(",") + "}") } -> "")

            //rank legivel só por ELM
            //            lazy val rank = "multilabel" + ranqueia(accs.map(_._2)).mkString(",")
            //            List(metaatts -> rank)

          } else {
            val melhores = pegaMelhores(accs, n)(_._2 * melhor).map(_._1)
            melhores map (m => metaatts -> m.limpa)
          }
        }
      ds.close()
      res.flatten
    }

    println(s"$arq")
    if (!new File(arq).exists()) grava(arq, arff(labels.mkString(","), bags.toList.flatten, print = true, context, porRank))
    println(s"$arq")

    val patterns = Datasets.arff(arq, dedup) match {
      case Right(x) => x
      case Left(m) => error(s"${m} <- m")
    }

    println(s"${patterns.size} <- patterns.size")

    // refaz bags por base
    val metaclassifs = (patts: Vector[Pattern]) => if (porRank) Vector()
    else Vector(
      //      CIELMBatch(), C45(false, 5), C45(false, 25), C45(false, 50), C45(false, 100),
      //      KNNBatcha(5, "eucl", patts),
      //      RF(42,5), RF(42,20),
      RF(42, 100),
      //      SVMLibRBF(),
      Maj())
    //    val metaclassifs = (patts: Vector[Pattern]) => if (porRank) Vector() else Vector(CIELMBatch(), C45(false, 50), KNNBatcha(5, "eucl", patts), RF(), Maj())
    val bagsFromFile = patterns.groupBy(_.vector).values.toVector
    val accs = if (featureSel) ??? else Stat.media_desvioPadraol(cv(patterns, metaclassifs, porRank, rus, ks).flatten.toVector)
    (accs.zipWithIndex.filter(_._2 % 2 == 0) zip accs.zipWithIndex.filter(_._2 % 2 == 1)) foreach println
  }
}

/*
LOO
(((1.0,0.0),0),                                 ((0.4859574468085105,0.33280598871826866),1))
(((0.31531914893617036,0.003835531787380181),2),((0.3153191489361701,0.356704456226357),3))

10x10fold
(((0.9999810084033613,9.350942920921797E-5),0),((0.4641466666666666,0.07960157814577024),1))
(((0.3153243697478991,0.0031217002361253045),2),((0.3157333333333332,0.02729393198190391),3))

 LOO:       RF100 49% +-33; Maj 32% +-37
 10x10fold: RF100 46% +-8;  Maj 32% +-3
 */