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

import clean.lib.{CM, Ds}
import ml.Pattern
import ml.classifiers.{RF, SVMLibDegree1}
import svmal.SVMStrategymulti

import scala.util.Random

/**
 * One SVM per class; queries by round-robin.
 * @param pool
 * @param algorithm
 * @param debug
 */
case class SVMmultiLinear(pool: Seq[Pattern], algorithm: String, debug: Boolean = false) extends Strategy {
   override val toString = s"SVMmultiL ($algorithm)"
   val abr = "SVML" + algorithm.take(3).toLowerCase

   //learner just to visual tests and to be referenced in db.
   //TROQUEI LEARNER PARA SVMLibDegree1
   // (aproveitando id/queries das estratégias SVMmulti),
   // demora mais que LibLinear, mas fica em linha com artigo do Tong!
//   def learner = LogReg()
   def learner = SVMLibDegree1()

   val id = algorithm match {
      //      case "SIMPLE" => 17
      //      case "SELF_CONF" => 18
      //      case "KFF" => 19
      //      case "BALANCED_EE" => 20
      case "SIMPLEw" => 966000
      case "SELF_CONFw" => 967000
      case "KFFw" => 968000
      case "BALANCED_EEw" => 969000
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
      var c = 1
      ar(0) = hist(0)
      while (c < nclasses) {
         ar(c) = ar(c - 1) + hist(c)
         c += 1
      }
      ar
   }

   val rnd = new Random(seed)

   private def queries_rec(svm: Seq[SVMStrategymulti], unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
      if (unlabeled.isEmpty) Stream.Empty
      else {
         val n = labeled.size
         val ps = hist(labeled.toArray, n) map (x => x / n.toDouble)
         val ps_1 = ps map (x => 1 - x)
         val pscompl = ps_1 map (_ / ps_1.sum)

         val fdpscompl = fdp(pscompl, n)
         //         val fdpscompl = fdp(ps, n)

         //         println(ps.mkString(" "))
         //         println(ps_1.mkString(" "))
         //         println(pscompl.mkString(" "))
         //         println(fdpscompl.mkString(" "))
         val sorteio = rnd.nextFloat()
         val chosen = fdpscompl.zipWithIndex.dropWhile(_._1 < sorteio).head._2
         //         println(s"\n ==== $chosen ====== $sorteio")
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

object SVMmultiTest extends App with CM {
   val context = "SVMmultiTest"
   val ds = Ds("abalone-3class", readOnly = true)
   val patts = new Random(6294).shuffle(ds.patterns)
   val (tr, ts) = patts.splitAt(patts.size / 4)
   val l = RF()
   val strats = Seq(
      //      RandomSampling(tr)
      DensityWeightedTrainingUtility(l, tr, "eucl")
      , AgDensityWeightedTrainingUtility(tr, "eucl")
      //      , SVMmulti(tr, "BALANCED_EEw")
      //      , SVMmulti(tr, "KFFw")
      //      , SVMmulti(tr, "SIMPLEw")
      //      , SVMmulti(tr, "SELF_CONFw")
   ).par
   val accs = strats.par.map { x =>
      val ques = x.queries.take(400)
      var m = l.build(ques.take(tr.head.nclasses))
      var old = m.predictionEntropy(tr)._1
      ques.map { q =>
         //         accBal(l.build(x.queries.take(qs)).confusion(ts))
         m = l.update(m)(q)
         val ne = m.predictionEntropy(tr)._2
         val r = old - ne
         old = ne
         r
      }
   }.transpose.map(_.mkString(" "))
   println(strats.map(_.abr).mkString(" "))
   accs foreach println
}