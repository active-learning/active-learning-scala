package clean

import al.strategies._
import ml.Pattern
import ml.classifiers._
import ml.neural.elm.ELM
import util.Datasets

import scala.collection.mutable
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

class TopSpec extends UnitSpec with Blob with Lock {
  lazy val datasets = Source.fromFile("juntos.txt").getLines().toList
  val path = "/run/shm/testuci"

  def learner(pool: Seq[Pattern]) = List(
    NB()
    ,
    KNNBatch(5, "eucl", pool, weighted = true),
    VFDT(),
    ECIELM(learnerSeed),
    EIELM(learnerSeed),
    interaELM(learnerSeed),
    SVMLib(learnerSeed)
  )

  def strats(pool: => Seq[Pattern]) = learner(pool).map { learner =>
    List(
      Margin(learner, pool)
      ,
      new SGmultiJS(learner, pool),
      DensityWeightedTrainingUtility(learner, pool, "eucl"),
      DensityWeightedTrainingUtility(learner, pool, "maha"),
      MahalaWeightedTrainingUtility(learner, pool, 1, 1),
      ExpErrorReductionMargin(learner, pool, "gmeans+residual", sample = 3)
    )
  }.flatten

  val run = 0
  val fold = 0
  val learnerSeed = run * 10000 + fold
  val asserts = mutable.Queue[() => Unit]()
  datasets.filter(!_.startsWith("#")).par foreach { dataset =>
    val ds = Ds(path, dataset)
    ds.open()
    ds.log(s"Processing ${ds.n} instances ...")
    val tr0 = new Random(0).shuffle(ds.patterns).groupBy(_.label).map(_._2.take(ds.singlePoolSizeForTests / ds.nclasses)).toList.flatten
    val ts0 = ds.patterns.filter(!tr0.contains(_)).groupBy(_.label).map(_._2.take(ds.singlePoolSizeForTests / ds.nclasses)).toList.flatten
    val tr = if (tr0.size < ds.singlePoolSizeForTests) tr0 ++ ds.patterns.diff(tr0).take(ds.singlePoolSizeForTests - tr0.size) else tr0
    val ts = if (ts0.size < ds.nclasses) ts0 ++ tr.take(ds.nclasses) else ts0 //to avoid small/empty testing sets

    //reset ds
    ds.reset()

    strats(Seq()) foreach {
      strat =>
        println(s"$strat/${strat.learner} ...")

        //Ordena pool,testSet e aplica filtro se preciso.
        val needsFilter = (strat, strat.learner) match {
          case (_, _: ELM) => println(s"elm"); true
          case (DensityWeightedTrainingUtility(_, _, "maha", _, _, _), _) => true
          case (_: MahalaWeightedTrainingUtility, _) => true
          case _ => false
        }

        val binaf = if (needsFilter) Datasets.binarizeFilter(tr) else null
        lazy val binarizedTr = Datasets.applyFilter(binaf)(tr)
        lazy val binarizedTs = Datasets.applyFilter(binaf)(ts)

        val zscof = if (needsFilter) Datasets.zscoreFilter(binarizedTr) else null
        lazy val pool = if (!needsFilter) new Random(fold).shuffle(tr.sortBy(_.id))
        else {
          val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
          new Random(fold).shuffle(filteredTr.sortBy(_.id))
        }

        lazy val testSet = if (!needsFilter) new Random(fold).shuffle(ts.sortBy(_.id))
        else {
          val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
          new Random(fold).shuffle(filteredTs.sortBy(_.id))
        }

        if (!ds.isQCalculated) {
          //queries
          ds.writeQueries(pool, RandomSampling(pool), run, fold, Int.MaxValue)

          //hits
          val dsQueries = ds.queries(RandomSampling(pool), run, fold, binaf, zscof)

          //SpecTest needs mutex if parallelized.
          asserts.enqueue(() => s"$dataset rnd strat" should "write/read queries" in {
            assert(RandomSampling(pool).queries.map(_.toString).sameElements(dsQueries.map(_.toString)))
            assert(RandomSampling(pool).queries.sameElements(dsQueries))
          })

          val learners = Seq(NB(), KNNBatch(5, "eucl", pool, weighted = true), C45())
          learners foreach { learner =>
            ds.writeHits(pool, testSet, dsQueries.toVector, RandomSampling(pool), run, fold)(learner)
            val dsHits = ds.getCMs(RandomSampling(pool), learner, run, fold)
            var m = learner.build(dsQueries.take(ds.nclasses))
            val hitses = m.confusion(testSet) +: dsQueries.drop(ds.nclasses).map { pa =>
              m = learner.update(m, true)(pa)
              m.confusion(testSet)
            }
            asserts.enqueue(() =>
              it should s"write/read $learner hits" in {
                assertResult(hitses.flatten.flatten.toList)(dsHits.flatten.flatten.toList)
              })
          }

          ds.calculaQ(1, 1)
          println(s"Q: ${ds.Q}")
          asserts.enqueue(() => it should "calculate Q as |Y| <= Q <= |U|" in {
            assert(ds.Q <= pool.size)
            assert(ds.nclasses <= ds.Q)
          })
        }

        val strategy = strats(pool).find(x => x.id == strat.id && x.learner.id == strat.learner.id).get
        val queries = strategy.queries.take(ds.Q)
        ds.writeQueries(pool, strategy, run, fold, ds.Q)
        val dsQueries = ds.queries(strategy, run, fold, binaf, zscof)
        asserts.enqueue(() => s"${ds.nclasses} queries" should s"remain the same after written and read for $ds/$strategy/${strategy.learner}" in {
          assert(queries.map(x => (x.id, x, x.label)).sameElements(dsQueries.map(x => (x.id, x, x.label))))
        })
        //
        //        println("dsqs ================\n" + dsQueries.head.dataset().toString.lines.filter(_.contains("V4")).toList)
        //        println("ts ------------------\n" + testSet.head.dataset().toString.lines.filter(_.contains("V4")).toList)
        //        println(s"qs")
        //        dsQueries.sortBy(_.id).take(5).map(x => x.id + ":" + x.label + " " + x.value(3)) foreach println
        //        println(s"poo")
        //        pool.sortBy(_.id).take(5).map(x => x.id + ":" + x.label + " " + x.value(3)) foreach println
        //        println(s"ts")
        //        testSet.sortBy(_.id).take(5).map(x => x.id + ":" + x.label + " " + x.value(3)) foreach println
        //        if (dataset.startsWith("lung")) sys.exit(0)

        println("hits")
        var m = strategy.learner.build(queries.take(ds.nclasses))
        val hitses = m.confusion(testSet) +: queries.drop(ds.nclasses).map { pa =>
          m = strategy.learner.update(m, fast_mutable = true)(pa)
          m.confusion(testSet)
        }

        ds.writeHits(pool, testSet, dsQueries.toVector, strategy, run, fold)(strategy.learner)
        val dsHits = ds.getCMs(strategy, strategy.learner, run, fold)

        asserts.enqueue(() => s"${ds.nclasses} conf. mat.s" should s"remain the same after written and read for $ds/$strategy/${strategy.learner}" in {
          assert(hitses.flatten.flatten.sameElements(dsHits.flatten.flatten))
        })
    }
    ds.close()
  }
  asserts foreach (_.apply())
}