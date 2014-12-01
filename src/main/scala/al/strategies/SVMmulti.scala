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
import ml.classifiers.{NB, SVMLib}
import svmal.SVMStrategymulti
import util.{ALDatasets, Datasets}

import scala.util.Random

/**
 * One SVM per class; queries by round-robin.
 * @param pool
 * @param algorithm0
 * @param debug
 */
case class SVMmulti(pool: Seq[Pattern], algorithm0: String, debug: Boolean = false) extends Strategy {
   override val toString = s"SVMmulti ($algorithm0)"
   val abr = "SVM" + algorithm0.take(3).toLowerCase

   def learner = SVMLib()

   //just to visual tests and to be referenced in db
   val algorithm = if (algorithm0 == "BALANCED_EEw") "BALANCED_EE" else algorithm0
   val id = algorithm0 match {
      case "SIMPLE" => 17
      case "SELF_CONF" => 18
      case "KFF" => 19
      case "BALANCED_EE" => 20
      case "BALANCED_EEw" => -20
   }

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

   private def hist(seq: Array[Pattern], n: Int) = {
      val ar = Array.fill(nclasses)(0)
      var i = 0
      while (i < n) {
         ar(seq(i).label.toInt) += 1
         i += 1
      }
      ar
   }

   private def fdp(hist: Array[Double], n: Int) = {
      val ar = Array.fill(nclasses)(0d)
      var c = 0
      ar(0) = hist(0)
      while (c + 1 < nclasses) {
         ar(c) = ar(c - 1) + hist(c)
         c += 1
      }
      ar
   }

   val rnd = new Random(123)

   private def queries_rec(svm: Seq[SVMStrategymulti], unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
      if (unlabeled.isEmpty) Stream.Empty
      else {
         val n = labeled.size
         val accps = (0 until nclasses).zip(fdp(hist(labeled.toArray, n) map (x => 1 - x / n.toDouble), n))
         val sorteio = rnd.nextFloat()
         val chosen = accps.dropWhile(_._2 < sorteio).head._1

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
