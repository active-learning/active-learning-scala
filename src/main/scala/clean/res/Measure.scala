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

package clean.res

import al.strategies.Strategy
import clean.{Global, Blob, CM, Ds}
import ml.classifiers.Learner

trait Measure extends CM with Blob {
   val id: Int
   val ds: Ds
   val s: Strategy
   val l: Learner
   val r: Int
   val f: Int
   val value: Option[Double]
   val context = "MeaTrait"

   //   protected def qs2hs(qs: Int) = qs - ds.nclasses + 1
   //
   //   protected def t2qs(t: Int) = t + 1
   //
   //   protected def t2hs(t: Int) = qs2hs(t2qs(t))
   //
   //   protected def t2hi(t: Int) = t - ds.nclasses
}

sealed trait InstantMeasure extends Measure {
   val t: Int
   protected val fun: (Array[Array[Int]]) => Double
   protected lazy val CMs = {
      if (t < ds.nclasses - 1 || t >= ds.expectedPoolSizes(Global.folds).min)
         ds.error(s"tf $t fora dos limites t:[${ds.nclasses};${ds.expectedPoolSizes(Global.folds).min}]")
      ds.getCMs(s, l, r, f)(t, t)
   }
   lazy val value = if (CMs.isEmpty) None else Some(fun(CMs(t)))
}

sealed trait RangeMeasure extends Measure {
   val ti: Int
   val tf: Int
   protected val fun: (Array[Array[Int]]) => Double
   protected val rangeFun: (Seq[Array[Array[Int]]]) => (Array[Array[Int]] => Double) => Double
   protected lazy val CMs = {
      if (ti > tf || tf <= ds.nclasses || tf >= ds.expectedPoolSizes(Global.folds).min)
         ds.error(s"ti $ti ou tf $tf fora dos limites ti<=tf tf:]${ds.nclasses};${ds.expectedPoolSizes(Global.folds).min}[")
      ds.getCMs(s, l, r, f)(ti, tf)
   }
   protected lazy val calc = rangeFun(CMs.values.toSeq)
   lazy val value = if (CMs.size < tf - ti + 1) None else Some(calc(fun))
}

case class BalancedAcc(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val t: Int)
   extends InstantMeasure {
   val id = 10000000 + t
   protected val fun = accBal _
}

case class Kappa(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val t: Int)
   extends InstantMeasure {
   val id = 20000000 + t
   protected val fun = kappa _
}

case class ALCBalancedAcc(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val ti: Int, val tf: Int)
   extends RangeMeasure {
   val id = 30000000 + tf * 10000 + ti
   protected val fun = accBal _
   protected val rangeFun = ALC _
}

case class ALCKappa(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val ti: Int, val tf: Int)
   extends RangeMeasure {
   val id = 40000000 + tf * 10000 + ti
   protected val fun = kappa _
   protected val rangeFun = ALC _
}
