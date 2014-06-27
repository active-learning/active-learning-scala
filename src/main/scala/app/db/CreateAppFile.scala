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

package app.db

import java.io.{FileWriter, File}

import al.strategies._
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import app.ArgParser
import ml.classifiers._
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import al.strategies.Margin
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy
import al.strategies.Uncertainty
import ml.classifiers.interaELM
import al.strategies.Margin
import ml.classifiers.NoLearner
import al.strategies.SGmultiJS
import al.strategies.MahalaWeightedRefreshedTrainingUtility
import al.strategies.ExpErrorReduction
import al.strategies.ClusterBased
import al.strategies.MahalaWeightedTrainingUtility
import al.strategies.SGmulti
import al.strategies.RandomSampling
import al.strategies.MahalaWeightedRefreshed
import al.strategies.FastPerfectRealisticAccuracy
import al.strategies.MahalaWeighted
import al.strategies.DensityWeightedTrainingUtility
import al.strategies.Entropy

object CreateAppFile extends App {
  //commented code: aproveitar no futuro qnd for gerar tabela de meta-atributos
  //val (path, names) = ArgParser.testArgs(getClass.getSimpleName.dropRight(1), args, 2, "Version " + 0.2 + ". Creates the SQLite file for the whole als app.")
  //case class MetaAtt(dataset: Dataset, name: String, value: Float) extends Entity

  val sampleSize = 500
  val sampleSizePerfect = 1000
  val strats = List(
    RandomSampling(Seq()),
    ClusterBased(Seq()),

    FastPerfectRealisticAccuracy(interaELM(1), Seq(), sampleSizePerfect),
    Uncertainty(interaELM(1), Seq()),
    Entropy(interaELM(1), Seq()),
    Margin(interaELM(1), Seq()),
    new SGmulti(interaELM(1), Seq(), "consensus"),
    new SGmulti(interaELM(1), Seq(), "majority"),
    new SGmultiJS(interaELM(1), Seq()),
    ExpErrorReduction(interaELM(1), Seq(), "entropy", sampleSize),
    ExpErrorReduction(interaELM(1), Seq(), "accuracy", sampleSize),
    DensityWeightedTrainingUtility(interaELM(1), Seq(), 1, 1, "cheb"),
    DensityWeightedTrainingUtility(interaELM(1), Seq(), 1, 1, "eucl"),
    DensityWeightedTrainingUtility(interaELM(1), Seq(), 1, 1, "maha"),
    DensityWeightedTrainingUtility(interaELM(1), Seq(), 1, 1, "manh"),
    MahalaWeighted(interaELM(1), Seq(), 1),
    MahalaWeightedRefreshed(interaELM(1), Seq(), 1, sampleSize),
    MahalaWeightedTrainingUtility(interaELM(1), Seq(), 1, 1),
    MahalaWeightedRefreshedTrainingUtility(interaELM(1), Seq(), 1, 1, sampleSize),

    FastPerfectRealisticAccuracy(NB(), Seq(), sampleSizePerfect),
    Uncertainty(NB(), Seq()),
    Entropy(NB(), Seq()),
    Margin(NB(), Seq()),
    new SGmulti(NB(), Seq(), "consensus"),
    new SGmulti(NB(), Seq(), "majority"),
    new SGmultiJS(NB(), Seq()),
    ExpErrorReduction(NB(), Seq(), "entropy", sampleSize),
    ExpErrorReduction(NB(), Seq(), "accuracy", sampleSize),
    DensityWeightedTrainingUtility(NB(), Seq(), 1, 1, "cheb"),
    DensityWeightedTrainingUtility(NB(), Seq(), 1, 1, "eucl"),
    DensityWeightedTrainingUtility(NB(), Seq(), 1, 1, "maha"),
    DensityWeightedTrainingUtility(NB(), Seq(), 1, 1, "manh"),
    MahalaWeighted(NB(), Seq(), 1),
    MahalaWeightedRefreshed(NB(), Seq(), 1, sampleSize),
    MahalaWeightedTrainingUtility(NB(), Seq(), 1, 1),
    MahalaWeightedRefreshedTrainingUtility(NB(), Seq(), 1, 1, sampleSize),

    FastPerfectRealisticAccuracy(C45(), Seq(), sampleSizePerfect),
    Uncertainty(C45(), Seq()),
    Entropy(C45(), Seq()),
    Margin(C45(), Seq()),
    new SGmulti(C45(), Seq(), "consensus"),
    new SGmulti(C45(), Seq(), "majority"),
    new SGmultiJS(C45(), Seq()),
    ExpErrorReduction(C45(), Seq(), "entropy", sampleSize),
    ExpErrorReduction(C45(), Seq(), "accuracy", sampleSize),
    DensityWeightedTrainingUtility(C45(), Seq(), 1, 1, "cheb"),
    DensityWeightedTrainingUtility(C45(), Seq(), 1, 1, "eucl"),
    DensityWeightedTrainingUtility(C45(), Seq(), 1, 1, "maha"),
    DensityWeightedTrainingUtility(C45(), Seq(), 1, 1, "manh"),
    MahalaWeighted(C45(), Seq(), 1),
    MahalaWeightedRefreshed(C45(), Seq(), 1, sampleSize),
    MahalaWeightedTrainingUtility(C45(), Seq(), 1, 1),
    MahalaWeightedRefreshedTrainingUtility(C45(), Seq(), 1, 1, sampleSize)
  )

  //ensures there is no previous file
  val alsFileStr = ArgParser.appPath + "app.db"
  val dbOriginal = new File(alsFileStr)
  if (dbOriginal.exists) {
    println("File " + dbOriginal + " already exists! Delete it first.")
    sys.exit(0)
  }

  val learners = Seq(NoLearner(), NB(), C45(), interaELM(1),
    KNN(5, "eucl"), HT(),
    IELM(1), EIELM(1), CIELM(1), OSELM(math.sqrt(1).toInt)
  )

  //opens (creates) app.db
  val af = AppFile(create = true)
  af.open(debug = true)
  af.createOtherTables()
  af.createTableOfLearners(learners)
  af.createTableOfStrategies(strats)
  af.close()
}
