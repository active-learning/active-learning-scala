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

import clean.lib.{AppWithUsage, FilterTrait, Ds, CM}
import ml.Pattern
import ml.classifiers._
import ml.models.Model
import org.apache.commons.math3.stat.correlation.{PearsonsCorrelation, SpearmansCorrelation}
import util.Tempo

import scala.util.Random

case class GATUTests2(learner: Learner, pool: Seq[Pattern], distance_name: String, alpha: Double = 1, beta: Double = 1, debug: Boolean = false)
   extends StrategyWithLearnerAndMaps with MarginMeasure with EntropyMeasure {
   override val toString = "GATU a" + alpha + " b" + beta + " (" + distance_name + ")"
   val abr = "\\textbf{GATU" + distance_name.take(3) + "}"
   //+ beta
   val id = if (alpha == 1 && beta == 1 || alpha == 0.5 && beta == 0.5) distance_name match {
      case "eucl" => 4023306 + (100000 * (1 - alpha)).toInt
      case "cheb" => 4023308 + (100000 * (1 - alpha)).toInt
      case "maha" => 4023309 + (100000 * (1 - alpha)).toInt
      case "manh" => 4023307 + (100000 * (1 - alpha)).toInt
   } else throw new Error("Parametros inesperados para GATU.")

   protected def next(mapU: => Map[Pattern, Double], mapL: => Map[Pattern, Double], current_model: Model, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      val ls = labeled.size
      val us = unlabeled.size
      //      val hist = Array.fill(nclasses)(0d)
      //      labeled foreach { lab =>
      //         val cla = lab.label.toInt
      //         hist(cla) += 1
      //      }
      //      //      val des = desb(hist)
      //      val s = hist.sum
      //      val des = normalized_entropy(hist.map(_ / s))
      //
      //      val rnd = new Random(seed)
      //      1 to labeled.size foreach (_ => rnd.nextDouble())
      //      val sampled = rnd.shuffle(unlabeled).take(10000)
      //
      var (gnmax, agmax, umax, lmax, umin, lmin, gnmin, agmin) = (-1d, -1d, -1d, -1d, 1d, 1d, 1d, 1d)
      var xgnmax: Pattern = null
      var xagmax: Pattern = null
      var xumax: Pattern = null
      var xlmax: Pattern = null
      val list_i_id = unlabeled map { x =>
         val similarityU = mapU(x) / us
         val similarityL = mapL(x) / ls
         val gn = 1 - margin(current_model)(x)
         val u = math.pow(similarityU, beta)
         val l = math.pow(similarityL, alpha)
         val ag = u / l
         if (gn > gnmax) {
            gnmax = gn
            xgnmax = x
         }
         if (gn < gnmin) {
            gnmin = gn
            //                  ximin = x
         }
         if (ag > agmax) {
            agmax = ag
            xagmax = x
         }
         if (ag < agmin) {
            agmin = ag
            //                  xdmin = x
         }
         if (u > umax) {
            umax = u
            xumax = x
         }
         if (l > lmax) {
            lmax = l
            xlmax = x
         }
         if (u < umin) {
            umin = u
            //            xumin = x
         }
         if (l < lmin) {
            lmin = l
            //            xlmin = x
         }
         gn -> gn * ag
      }

      val xmixmax = unlabeled maxBy { x =>
         val similarityU = mapU(x) / us
         val similarityL = mapL(x) / ls
         val gn = 1 - margin(current_model)(x)
         val u = math.pow(similarityU, beta)
         val l = math.pow(similarityL, alpha)
         val ag = u / l
         gn * ag //((gn - gnmin) / (gnmax - gnmin)) * ((ag - agmin) / (agmax - agmin))
      }

      val (a, b) = list_i_id.unzip
      lazy val spear = new SpearmansCorrelation().correlation(a.toArray, b.toArray)
      //
      val sim_gn_mix = 1d / (1 + d(xgnmax, xmixmax))
      val sim_ag_mix = 1d / (1 + d(xagmax, xmixmax))
      //      //      //      val densObsoleta = spear > 0.99
      //      //      //      val densObsoleta = ximax == xidmax
      //      val densObsoleta = sim_gn_mix < sim_ag_mix
      //      //      print(s"${densObsoleta.compareTo(false)}")
      //      //      print(s"$spear")
      //      print(s"$lmin $lmax $umin ${10000 * umax} $des")

      //      if (!densObsoleta) xagmax //0.44 0.56
      //      if (agmin / agmax < 0.9) xagmax//0.49333333333333335 0.4533333333333333
      //      if (agmin / agmax < 0.5) xagmax//0.4666666666666667 0.5333333333333333
      //      if (agmin / agmax < 0.95) xagmax//0.05333333333333334 0.013333333333333334
      lazy val co = new PearsonsCorrelation().correlation(a.toArray, b.toArray)
      //      if (a.size>1 )println(s"$spear $co")
      //      if (a.size>1 && co < 0.999) xagmax //0.52 0.4266666666666667
      //      if (a.size>1 && co < 0.99) xagmax //0.29333333333333333 0.26666666666666666
      //      if (a.size>1 && spear < 0.99) xagmax //0.3466666666666667 0.64
      //      else {
      unlabeled maxBy { x =>
         val similarityU = mapU(x) / us
         val similarityL = mapL(x) / ls
         val gn = 1 - margin(current_model)(x)
         val u = math.pow(similarityU, beta)
         val l = math.pow(similarityL, alpha)
         val ag = u / l
         //((gn - gnmin) / (gnmax - gnmin)) * ((ag - agmin) / (agmax - agmin))
         //         math.pow(gn, sim_gn_mix) * ag //0.25333333333333335 0.25333333333333335
         //         gn * math.pow(ag, 1 - sim_gn_mix)//0.09333333333333334 0.12
         //         math.sqrt(math.pow(gn, sim_gn_mix) * math.pow(ag, 1 - sim_gn_mix)) //0.14666666666666667 0.17333333333333334
         //         math.sqrt(math.pow(gn, sim_gn_mix) * math.pow(ag, 1 - sim_gn_mix)) //0.13333333333333333 0.09333333333333334
         //         math.sqrt(math.pow(gn, sim_gn_mix) * math.pow(ag, sim_ag_mix)) //0.05333333333333334 0.06666666666666667
         math.sqrt(math.pow(gn, 0.3) * math.pow(ag, 0.7)) //0.2 0.22666666666666666
      }
      //      }

      //      val sim = 1d / (1 + d(xumax, xlmax))
      //      print(s"$sim")
   }
}


object GATUTest extends AppWithUsage with CM with FilterTrait {
   f

   def f {
      Tempo.start
      val fd = datasets.par.filter { d =>
         val ds = Ds(d, readOnly = true)
         ds.open()
         val U = ds.poolSize.toInt
         ds.close()
         U > 200
      }
      println(fd.size + " datasets")
      val accss = fd.take(32).par map { name =>
         val ds = Ds(name, readOnly = true)
         val patts = new Random(2985).shuffle(ds.patterns).take(20000)
         println(patts.size)
         val (tr, ts) = patts.splitAt(2 * patts.size / 3)
         val l = RF()
         Seq(
            GATU(l, tr, "manh"),
            GnoKNN(tr, "manh"),
            Sincretist(tr, "manh"),
            DensityWeightedTrainingUtility(l, tr, "manh")
         ).par map { s =>
            var m = l.build(s.queries.take(tr.head.nclasses))
            s.queries.drop(tr.head.nclasses).take(100).map { q =>
               m = l.update(m)(q)
               kappa(m.confusion(ts))
            }.sum / 200d
         }
      }
      Tempo.print_stop
      println(s"")
      for (ca <- 0 until accss.head.size; cb <- 0 until accss.head.size) {
         println(s"$ca > $cb: " + accss.count(x => x(ca) > x(cb)) / accss.size.toDouble + " ")
      }
   }

   val arguments: List[String] = superArguments
   val context: String = ""
}

/*
         Seq(Sincretist(tr, "manh"),
            DensityWeightedTrainingUtility(l, tr, "manh"),
            AgDensityWeightedTrainingUtility(tr, "manh"),
               Ag(2, l, tr, "manh"),
               Ag(3, l, tr, "manh"),
               Ag2(12, l, tr, "manh"),
               Ag2(18, l, tr, "manh"),
                        GATU(l, tr, "manh")
            //            GATUApErrado(l, tr, "eucl"),
            //            GATUAp(l, tr, "eucl"),
         ).par map { s =>

Elapsed 408754.0ms.

0 > 0: 0.0
0 > 1: 0.3466666666666667
0 > 2: 0.4266666666666667
0 > 3: 0.3333333333333333
0 > 4: 0.38666666666666666
0 > 5: 0.30666666666666664
0 > 6: 0.3466666666666667
0 > 7: 0.22666666666666666
1 > 0: 0.6533333333333333
1 > 1: 0.0
1 > 2: 0.6933333333333334
1 > 3: 0.5466666666666666
1 > 4: 0.6133333333333333
1 > 5: 0.5333333333333333
1 > 6: 0.5466666666666666
1 > 7: 0.44
2 > 0: 0.5066666666666667
2 > 1: 0.30666666666666664
2 > 2: 0.0
2 > 3: 0.30666666666666664
2 > 4: 0.32
2 > 5: 0.25333333333333335
2 > 6: 0.37333333333333335
2 > 7: 0.28
3 > 0: 0.6133333333333333
3 > 1: 0.4533333333333333
3 > 2: 0.64
3 > 3: 0.0
3 > 4: 0.52
3 > 5: 0.4666666666666667
3 > 6: 0.52
3 > 7: 0.4266666666666667
4 > 0: 0.56
4 > 1: 0.38666666666666666
4 > 2: 0.5866666666666667
4 > 3: 0.4266666666666667
4 > 4: 0.0
4 > 5: 0.3333333333333333
4 > 6: 0.41333333333333333
4 > 7: 0.3466666666666667
5 > 0: 0.6933333333333334
5 > 1: 0.4266666666666667
5 > 2: 0.7466666666666667
5 > 3: 0.49333333333333335
5 > 4: 0.6133333333333333
5 > 5: 0.0
5 > 6: 0.49333333333333335
5 > 7: 0.4266666666666667
6 > 0: 0.6533333333333333
6 > 1: 0.44
6 > 2: 0.6266666666666667
6 > 3: 0.48
6 > 4: 0.56
6 > 5: 0.49333333333333335
6 > 6: 0.0
6 > 7: 0.37333333333333335
7 > 0: 0.7733333333333333
7 > 1: 0.5466666666666666
7 > 2: 0.72
7 > 3: 0.5733333333333334
7 > 4: 0.64
7 > 5: 0.5733333333333334
7 > 6: 0.6266666666666667
7 > 7: 0.0
 */