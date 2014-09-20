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

class TopSpec extends UnitSpec with Blob with Lock {
  lazy val datasets = Source.fromFile("a").getLines().toList
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
      ExpErrorReductionMargin(learner, pool, "gmeans+residual", sample = 3)
    )
  }.flatten

  val run = 0
  val fold = 0
  val learnerSeed = run * 10000 + fold
  //  datasets.filter(!Seq("digits2", "digits2-davi").contains(_)).dropWhile(_ != "autoUniv-au6-1000").filter(!Seq("lymphography", "statlog-german-credit").contains(_)) foreach { dataset =>
  datasets.filter(!Seq("digits2", "digits2-davi").contains(_)) foreach { dataset =>
    val ds = Ds(path, dataset)
    ds.open()
    ds.log(s"Processing ${ds.n} instances ...")
    val tr = new Random(0).shuffle(ds.patterns).groupBy(_.label).map(_._2.take(2)).toList.flatten
    val ts = ds.patterns.filter(!tr.contains(_)).groupBy(_.label).map(_._2.take(2)).toList.flatten

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
        println(s"${pool.head.numAttributes() - tr.head.numAttributes()} [[[[[[[[[[[[[[[[[[[[[")
        if (pool.head.numAttributes() - tr.head.numAttributes() != 0) {
          println("mudou qtd de atts")
          sys.exit(0)
        }
        println("ts")
        lazy val testSet = if (!needsFilter) new Random(fold).shuffle(ts.sortBy(_.id))
        else {
          val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
          new Random(fold).shuffle(filteredTs.sortBy(_.id))
        }

        println(s"Q test")
        if (!ds.isQCalculated) {
          //queries
          println(s"queries")
          ds.writeQueries(pool, RandomSampling(pool), run, fold, Int.MaxValue)

          //hits
          println("fetch queries")
          val dsQueries = ds.queries(RandomSampling(pool), run, fold, binaf, zscof)
          //SpecTest needs mutex.
          acquire()
          s"$dataset rnd stat" should "write/read queries" in {
            assert(RandomSampling(pool).queries.sameElements(dsQueries))
          }
          release()

          println("hits")
          val learners = Seq(NB(), KNNBatch(5, "eucl", pool, weighted = true), C45())
          learners foreach { learner =>
            ds.writeHits(pool, testSet, dsQueries, RandomSampling(pool), run, fold)(learner)
            val dsHits = ds.getCMs(RandomSampling(pool), learner, run, fold)
            val hits = learner.build(dsQueries).confusion(pool)
            acquire()
            it should s"write/read $learner hits" in {
              assert(hits.flatten.toList.sameElements(dsHits.flatten.toList))
            }
            release()
          }

          println(s"Q")
          ds.calculaQ(1, 1)
          println(s"Q: ${ds.Q}")
          acquire()
          it should "calculate Q as |U|" in {
            assertResult(pool.size)(ds.Q)
          }
          release()
        }

        println("sid find")
        val strategy = strats(pool).find(x => x.id == strat.id && x.learner.id == strat.learner.id).get
        println("queries")
        val queries = strategy.queries.take(ds.Q)
        println("write qs")
        ds.writeQueries(pool, strategy, run, fold, ds.Q)
        println("read qs")
        val dsQueries = ds.queries(strategy, run, fold, binaf, zscof)
        acquire()
        s"${ds.nclasses} queries" should s"remain the same after written and read for $ds/$strategy/${strategy.learner}" in {
          assert(queries.sameElements(dsQueries))
        }
        release()

        println("hits")
        val hits = strategy.learner.build(queries).confusion(pool)


        //        pool.map(x => x.id + ":" + x.label+ " ") foreach println
        println(s"qs")
        dsQueries.map(x => x.id + ":" + x.label + " ") foreach print
        println(s"ts")
        testSet.map(x => x.id + ":" + x.label + " ") foreach print


        ds.writeHits(pool, testSet, dsQueries, strategy, run, fold)(strategy.learner)
        val dsHits = ds.getCMs(strategy, strategy.learner, run, fold)

        acquire()
        s"${ds.nclasses} conf. mat.s" should s"remain the same after written and read for $ds/$strategy/${strategy.learner}" in {
          assert(hits.sameElements(dsHits))
        }
        release()
    }
    ds.close()
  }
}