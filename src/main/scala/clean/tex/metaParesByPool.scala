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
  val dedup = false

  //se mudar medida, precisa verficar mais dois lugares: dsminSize e no código. ALC é mais fácil.
  //  val measure = Kappa
  val measure = ALCKappa

  //1 100 200 (Kappa exige 200)
  val dsminSize = 1

  //melhores 1; ou piores -1 (-1 é mais difícil pra acc e rank)
  val melhor = 1

  val n = 1
  val featureSel = false

  val (ini, fim) = ("ti", "tf")
  val (rus, ks) = 10 -> 10
  run()

  override def run() = {
    super.run()
    val ls = learners(learnersStr)
    //        val ss = stratsTexRedux("eucl")
    //        val ss = stratsTexRedux("all")
    //        val ss = stratsTex("all")
    //    val ss = Seq((l: Learner) => MarginFixo(l, Seq()), (l: Learner) => ExpErrorReductionMarginFixo(l, Seq(), "entropy"))//, (l: Learner) => HTUFixo(Seq(), l, Seq(), "maha"))
    //    val ss = Seq((l: Learner) => MarginFixo(l, Seq()))
    //        val ss = Seq(
    //          (l: Learner) => MarginFixo(l, Seq()),
    //          (l: Learner) => ExpErrorReductionMarginFixo(l, Seq(), "entropy")
    //        )
    //    val ss = Seq((l: Learner) => ExpErrorReductionMarginFixo(l, Seq(), "entropy"))
    val ss = Seq((l: Learner) => HTUFixo(Seq(), l, Seq(), "maha"))
    //    val ss = Seq((l: Learner) => HTUFixo(Seq(), l, Seq(), "eucl"), (l: Learner) => HTUFixo(Seq(), l, Seq(), "manh"), (l: Learner) => HTUFixo(Seq(), l, Seq(), "maha"))
    //      //          DensityWeightedTrainingUtilityFixo(Seq(), RF(), Seq(), "eucl")
    //    val ss = Seq((l: Learner) => AgDensityWeightedTrainingUtility(Seq(), "eucl"), (l: Learner) => AgDensityWeightedTrainingUtility(Seq(), "manh"), (l: Learner) => AgDensityWeightedTrainingUtility(Seq(), "maha"))
    //      //      ClusterBased(Seq()),
    //      //      RandomSampling(Seq())
    //    )
    val pares = for {l <- ls; s <- ss} yield s -> l
    //    $rus.$ks
    val arq = s"/home/davi/wcs/arff/$context-n${if (porRank) 1 else n}best${melhor}m$measure-$ini.$fim-${pares.map { case (s, l) => s(l).id }.mkString("-").hashCode.toLong.abs + (if (porRank) "Rank" else "")}-${learnerStr.replace(" ", ".")}-p$dsminSize.arff"
    val labels = pares.map { case (s, l) => s(l).limpa }

    //cada dataset produz um bag de metaexemplos (|bag| >= 25)
    def bagsNaN = DsByMinSize(datasets, dsminSize).par map { d =>
      val ds = Ds(d, readOnly = true)
      ds.open()
      val (ti0, th0, tf0, tpass) = ranges(ds)
      val ti = if (ini == "ti") ti0 else th0
      val tf = if (fim == "th") th0 else tf0
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
    def bags = bagsNaN
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
    else Vector(//NB não funciona porque quebra na discretização
      CIELMBatch(),
      C45(false, 5),
      C45(false, 25),
      KNNBatcha(5, "eucl", patts),
      KNNBatcha(25, "eucl", patts),
      RF(42, 500),
      SVMLibRBF(),
      NinteraELM(),
      Maj())
    //    val metaclassifs = (patts: Vector[Pattern]) => if (porRank) Vector() else Vector(CIELMBatch(), C45(false, 50), KNNBatcha(5, "eucl", patts), RF(), Maj())
    val accs = Stat.media_desvioPadraol(cv(featureSel, patterns, metaclassifs, porRank, rus, ks).flatten.toVector)
    val algs = if (porRank) {
      println(s"Pearson correl.: higher is better. $rus*$ks-fold CV. $measure$ini-$fim${if (featureSel) "FeatSel" else ""}")
      //      Seq("PCT       \t", "PCTpruned \t", "ELM       \t", "baseline  \t")
      Seq("PCT       \t", "ELM       \t", "baseline  \t")
    } else {
      println(s"Accuracy: higher is better. $rus*$ks-fold CV. $n best. $measure$ini-$fim${if (featureSel) "FeatSel" else ""}")
      metaclassifs(Vector()).map(x => x.limpa.padTo(10, " ").mkString + "\t")
    }
    val fo = "%2.3f"
    val out = (algs zip (accs.zipWithIndex.filter(_._2 % 2 == 0).map(x => "\t" + fo.format(x._1._1) + "\t" + fo.format(x._1._2) + "\t") zip accs.zipWithIndex.filter(_._2 % 2 == 1).map(x => "\t" + fo.format(x._1._1) + "\t" + fo.format(x._1._2) + "\t"))).sortBy(_._2._2).reverseMap(x => x._1 + x._2)
    out foreach println
  }
}