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
import ml.classifiers.SVMLib
import svmal.SVMStrategymulti
import util.Datasets

import scala.util.Random

/**
 * One SVM per class; queries by round-robin.
 * @param pool
 * @param algorithm
 * @param debug
 */
case class SVMmulti(pool: Seq[Pattern], algorithm: String, debug: Boolean = false) extends Strategy {
  override val toString = s"SVMmulti ($algorithm)"
  val abr = "SVM" + algorithm.take(3).toLowerCase
  val learner = SVMLib() //just to visual tests and to be referenced in db

  protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    val labeledar = labeled.toArray
    val unlabeledar = unlabeled.toArray
    val svms = {
      for (c <- 0 until nclasses) yield {
        new SVMStrategymulti(algorithm, SVMStrategymulti.PatternsToInstances2(labeledar, c), SVMStrategymulti.PatternsToInstances2(unlabeledar, c))
      }
    }.toArray
    queries_rec(svms, unlabeled, labeled)
  }

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    val current_model = learner.build(labeled)
    plot.zera()
    for (p <- distinct_pool) plot.bola(p.x, p.y, current_model.predict(p).toInt, 9)
    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 6)
    if (selected != null) plot.bola(selected.x, selected.y, -1, 25)
    plot.mostra()
    Thread.sleep((delay * 1000).round.toInt)
  }

  private def queries_rec(svm: Seq[SVMStrategymulti], unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
    if (unlabeled.isEmpty) Stream.Empty
    else {
      val n = labeled.size
      val chosen = n % nclasses

      if (debug) visual_test(null, unlabeled, labeled)

      val id = svm(chosen).nextQuery()
      val ind = svm(chosen).lastQueriedInd
      svm.zipWithIndex.filter { case (_, i) => i != chosen}.foreach { case (s, _) => s.markAsQueried(ind)}
      val selected = pool.find(_.id == id) match {
        case Some(p) => p
        case None => println("Queried id not found!")
          sys.exit(1)
      }

      if (debug) visual_test(selected, unlabeled, labeled)
      selected #:: queries_rec(svm, unlabeled.diff(Seq(selected)), labeled :+ selected)
    }
  }
}

object SVMmultiTest extends App {
  lazy val source = Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci") _
  source("banana") match {
    case Right(patts) =>
      0 until 5 foreach { run =>
        Datasets.kfoldCV(new Random(run).shuffle(patts), 5, false) { case (tr0, ts0, fold, minSize) =>

          //z-score
          lazy val f = Datasets.zscoreFilter(tr0)
          lazy val pool = {
            val tr = Datasets.applyFilterChangingOrder(tr0, f)
            val res = new Random(run * 100 + fold).shuffle(tr)
            res
          }
          lazy val testSet = {
            val ts = Datasets.applyFilterChangingOrder(ts0, f)
            new Random(run * 100 + fold).shuffle(ts)
          }

          //          if (run == 4 && fold == 4) {
          val n = 14
          val s = SVMmulti(pool, "SIMPLE")
          //                              val s = SVMmulti(pool, "BALANCED_EE")
          //                    val s = SVMmulti(pool, "KFF")
          //          val s = SVMmulti(pool, "SELF_CONF")
          println(s.queries.take(n + 5).toList.map(_.id))

          val s2 = SVMmulti(pool, "SIMPLE")
          //                              val s2 = SVMmulti(pool, "BALANCED_EE")
          //                              val s2 = SVMmulti(pool, "KFF")
          //          val s2 = SVMmulti(pool, "SELF_CONF")
          val qs = s2.queries.take(n).toList
          println((qs ++ s.resume_queries(qs).take(5).toList).map(_.id))

          //                      val m = interaELM(5, 0).batchBuild(pool)
          //                      println(m.accuracy(testSet))
          //
          //                      var m2 = interaELM(5, 0).batchBuild(pool.take(3))
          //                      pool.drop(3).foreach(x => m2 = interaELM(50, 0).update(m2)(x))
          //                      println(m2.accuracy(testSet))
          ////          }
        }
      }
  }
}