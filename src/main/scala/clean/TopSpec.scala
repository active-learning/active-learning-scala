package clean

import al.strategies._
import ml.Pattern
import ml.classifiers._
import ml.neural.elm.ELM
import util.Datasets

import scala.io.Source
import scala.util.Random

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

object TopSpec extends Lock with App {
  lazy val datasets = Source.fromFile("a").getLines().toList
  val learnerSeed = 12
  val path = "/home/davi/testuci"

  def learner(pool: Seq[Pattern]) = List(
    NB(),
    KNNBatch(5, "eucl", pool, weighted = true),
    VFDT(),
    ECIELM(learnerSeed),
    EIELM(learnerSeed),
    interaELM(learnerSeed),
    SVMLib(learnerSeed)
  )

  def strats(pool: => Seq[Pattern]) = learner(pool).map { learner =>
    List(
      Margin(learner, pool),
      new SGmultiJS(learner, pool),
      DensityWeightedTrainingUtility(learner, pool, "eucl"),
      DensityWeightedTrainingUtility(learner, pool, "maha"),
      MahalaWeightedTrainingUtility(learner, pool, 1, 1),
      ExpErrorReductionMargin(learner, pool, "gmeans+residual", 5)
    )
  }.flatten

  val run = 0
  val fold = 4
  datasets foreach { dataset =>
    //  datasets.par foreach { dataset =>
    val ds = Ds(path, dataset)
    ds.open()
    ds.log(s"Processing ${ds.n} instances ...")
    val tr = new Random(3460).shuffle(ds.patterns).groupBy(_.label).map(_._2.take(2)).toList.flatten
    println(tr)
    val ts = ds.patterns.filter(_ != tr).take(ds.nclasses * 2)
    println(ts)
    strats(Seq()) foreach {
      strat =>
        println(s"$strat ...")

        //Ordena pool,testSet e aplica filtro se preciso.
        val needsFilter = (strat, strat.learner) match {
          case (_, _: ELM) => true
          case (DensityWeightedTrainingUtility(_, _, "maha", _, _, _), _) => true
          case (_: MahalaWeightedTrainingUtility, _) => true
          case _ => false
        }

        println("bina")
        lazy val binaf = Datasets.binarizeFilter(tr)
        lazy val binarizedTr = Datasets.applyFilter(binaf)(tr)
        lazy val binarizedTs = Datasets.applyFilter(binaf)(ts)

        println("tr")
        lazy val zscof = Datasets.zscoreFilter(binarizedTr)
        lazy val pool = if (!needsFilter) new Random(fold).shuffle(tr.sortBy(_.id))
        else {
          val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
          new Random(fold).shuffle(filteredTr.sortBy(_.id))
        }

        println("ts")
        lazy val testSet = if (!needsFilter) new Random(fold).shuffle(ts.sortBy(_.id))
        else {
          val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
          new Random(fold).shuffle(filteredTs.sortBy(_.id))
        }


        //Q
        ds.writeQueries(pool, RandomSampling(pool), run, fold, pool.length)
        println("rnddsqs")
        val rnddsqs = ds.queries(RandomSampling(pool), run, fold)
        println("NB")
        ds.writeHits(pool, testSet, rnddsqs, RandomSampling(pool), run, fold)(NB())
        println("c45")
        ds.writeHits(pool, testSet, rnddsqs, RandomSampling(pool), run, fold)(C45())
        println("knn")
        ds.writeHits(pool, testSet, rnddsqs, RandomSampling(pool), run, fold)(KNNBatch(5, "eucl", pool, weighted = true))
        val Q = ds.Q.getOrElse(ds.calculaQ)
        println(s"Q: $Q")
        if (RandomSampling(pool).queries.sameElements(rnddsqs)) println("ok rnd qs") else println(s"rndqueries != dsrndQueries")



        println("sid find")
        val strategy = strats(pool).find(_.id == strat.id).get

        //Spec needs this mutexes.
        println("queries")
        val queries = strategy.queries.take(ds.nclasses)
        println("write qs")
        ds.writeQueries(pool, strategy, run, fold, Q)
        println("read qs")
        val dsQueries = ds.queries(strategy, run, fold)
        println(s"${ds.nclasses} queries should remain the same after written and read for $ds/$strategy/${strategy.learner}")
        if (queries.sameElements(dsQueries)) println("ok qs") else println(s"$queries != \n$dsQueries")

        println("hits")
        val hits = strategy.learner.build(queries).confusion(pool)
        ds.writeHits(pool, testSet, dsQueries, strategy, run, fold)(strategy.learner)
        val dsHits = ds.getHits(strategy, strategy.learner, run, fold)

        println(s"${ds.nclasses} conf. mat.s should remain the same after written and read for $ds/$strategy/${strategy.learner}")
        if (hits.sameElements(dsHits)) println("ok") else println(s"$hits != \n$dsHits")
        println(s"$strategy ok.")
    }
    ds.close()
  }
}