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

package al.strategies

import ml.Pattern
import ml.classifiers._
import ml.models.Model
import util.{Datasets, Tempo}

import scala.util.Random

/**
 * Defaults to Uncertainty if it misses the label.
 * Resuming is not exact, but who cares?
 * When resuming queries it is impossible to know if the last label estimate was right. It is assumed it hit.
 * @param learner
 * @param pool
 * @param criterion
 * @param sample
 * @param debug
 */
case class ExpErrorReduction(learner: Learner, pool: Seq[Pattern], criterion: String, sample: Int, debug: Boolean = false)
  extends StrategyWithLearner with Sample with EntropyMeasure {
  lazy val criterionInt = criterion match {
    case "entropy" => Ventropy
    case "accuracy" => Vaccuracy
    case "gmeans" => Vgmeans
    case "gmeans+residual" => VgmeansResidual
  }
  override val toString = "Expected Error Reduction s" + sample + " (" + criterion + ")"
  //Strategy with empty pool exists only to provide its name.
  val Ventropy = 0
  val Vaccuracy = 1
  val Vgmeans = 2
  val VgmeansResidual = 3

  protected def next(current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val res = if (labeled.last.missed) {
      Uncertainty(learner, distinct_pool).next(current_model, unlabeled, labeled) //todo: for multiclass, margin is better than unc. but the original paper don't do it this way.
    } else {
      val unlabeledSize = unlabeled.size
      val rnd = new Random(unlabeledSize)
      val unlabeledSamp = if (unlabeledSize > sample_internal) rnd.shuffle(unlabeled).take(sample_internal) else unlabeled
      lazy val optimistic_patterns = unlabeledSamp.map { p =>
        p.relabeled_reweighted(current_model.predict(p), 1, new_missed = false)
      }.toVector

      val (selected, label_estimate, _) = (
        for (pattern <- unlabeledSamp; c <- 0 until nclasses) yield {
          lazy val artificially_labeled_pattern = pattern.relabeled_reweighted(c, 1, new_missed = false)
          lazy val art_model = learner.update(current_model)(artificially_labeled_pattern)
          criterionInt match {
            case Ventropy => (pattern, c, criterion_entropy(art_model, unlabeledSamp))
            case Vgmeans => (pattern, c, 1 - criterion_gmeans(art_model, optimistic_patterns))
            case VgmeansResidual => (pattern, c, 1 - criterion_gmeansResidual(art_model, optimistic_patterns))
            case Vaccuracy => (pattern, c, 1 - criterion_accuracy(art_model, optimistic_patterns))
          }
        }).minBy(_._3)
      selected.relabeled_reweighted(selected.label, selected.weight, label_estimate != selected.label)

    }
    res
  }

  private def criterion_accuracy(m: Model, label_estimated_patterns: Vector[Pattern]) =
    m.hits(label_estimated_patterns) / label_estimated_patterns.length.toDouble

  private def criterion_gmeans(m: Model, label_estimated_patterns: Vector[Pattern]) = {
    val pseudo_accuracies_per_class = (0 until nclasses) map {
      c =>
        val only_this_class = label_estimated_patterns.filter(_.label == c)
        val hits = only_this_class count m.hit
        hits.toDouble / only_this_class.length
    }
    //    math.sqrt(pseudo_accuracies_per_class.product)
    pseudo_accuracies_per_class.product //faster
  }

  private def criterion_gmeansResidual(m: Model, label_estimated_patterns: Vector[Pattern]) = {
    val pseudo_accuracies_per_class = (0 until nclasses) map {
      c =>
        val only_this_class = label_estimated_patterns.filter(_.label == c)
        val hits = only_this_class count m.hit
        hits.toDouble / only_this_class.length
    }
    pseudo_accuracies_per_class.map(_ + 0.00001).product
  }

  /**
   * O exemplo que causa a menor entropia é o desejado,
   * pois isso indica que ele e o rótulo vão conferir mais certeza com relação ao pool.
   * @param m
   * @param unlabeled
   * @return
   */
  private def criterion_entropy(m: Model, unlabeled: Seq[Pattern]) = unlabeled.map {
    u => val d = m.distribution(u)
      entropy(d)
  }.sum
}

object EERTest extends App {

  //    val patts = new Random(0).shuffle(Datasets.arff(true)("/home/davi/unversioned/experimentos/fourclusters.arff").right.get).take(4000)
  //  val patts = new Random(0).shuffle(Datasets.arff(true)("/home/davi/wcs/ucipp/uci/magic.arff", true).right.get)
  val patts0 = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci")("gas-drift").right.get.take(1000000))
  //  val patts0 = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci/")("iris").right.get.take(10000))
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilterChangingOrder(patts0, filter)
  val n = (patts.length * 0.5).toInt

  println(patts.length + " " + patts.head.nclasses)
  //  val s = ExpErrorReduction(learner, patts.take(n), "entropy", 25) //25:7s 400:280s
  //  val s = SGmulti(learner, patts.take(n), "consensus")
  //1000:1s
  //    val s = MahalaWeightedTrainingUtility(learner, patts.take(n), 1, 1)//13s
  //    val s = MahalaWeightedRefreshedTrainingUtility(learner, patts.take(n), 1, 1, 25)//25:90000s
  val s = ExpErrorReduction(learner, patts.take(n), "gmeans", 25)
  //25:7s 100:15s 200:45s 400:300s 1000:2000s
  //  val m = learner.build(patts.take(n))
  //  println(m.accuracy(patts.drop(n)))
  Tempo.start
  val l = s.queries.take(3)
  Tempo.print_stop
  sys.exit(0)
  //          val s = ExpErrorReduction(learner, patts.take(n), "accuracy", 25) //25:7s 250:260s
  //  val s = DensityWeightedTrainingUtility(learner, patts.take(n), 1, 1, "eucl")
  //  val s = ClusterBased(patts.take(n))
  val ac1 = l.drop(patts.head.nclasses) map {
    q => m = learner.update(m)(q)
      m.accuracy(patts.drop(n))
  }
  (patts.head.nclasses until patts.head.nclasses * 2).zipWithIndex foreach { case (n, i) =>
    Tempo.start
    l(n)
    //    if (i % 20 == 0) {
    print(i + " ")
    Tempo.print_stop
    //    }
  }
  sys.exit(1)
  val lr = l.reverse
  val ac2 = lr.drop(patts.head.nclasses) map {
    q => mr = learner.update(mr)(q)
      mr.accuracy(patts.drop(n))
  }
  var m = learner.build(l.take(patts.head.nclasses))
  var mr = learner.build(lr.take(patts.head.nclasses))

  def learner = NB() //5, "eucl", patts) //interaELMNoEM(20)
  ac1.zip(ac2).foreach { case (a, b) => println(a + " " + b)}
  //  ac1.zip(ac2).foreach { case (a, b) => println(a)}
}
