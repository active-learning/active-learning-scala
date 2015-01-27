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

package clean.lib

import al.strategies.{Passive, Strategy}
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
   protected val instantFun: (Array[Array[Int]]) => Double
   lazy val existia = {
      val tmp = ds.read(s"select count(0) from r where m=$id and p=$pid").head.head
      if (tmp > 1) ds.error(s"Mais do que um r!!!")
      tmp == 1
   }
   lazy val pid = ds.poolId(s, l, r, f).getOrElse {
      if (ds.readOnly) throw new Exception(s"readOnly: Could not create pid for ${(s, l, r, f)}.")
      ds.log(s"Tentando criar pool ${(s, l, r, f)}", 30)
      s match {
         case Passive(s.pool, false) =>
            ds.write(s"insert into p values (NULL, ${s.id}, ${l.id}, $r, $f)")
            ds.poolId(s, l, r, f).getOrElse(error(s"Could not create pid for ${(s, l, r, f)}."))
         case _ => throw new Error(s"No pid for ${(s, l, r, f)}.")
      }
   }

   def read(ds: Ds) = ds.read(s"select v from r where m=$id and p=$pid") match {
      case List(Vector(v)) => Some(v)
      case List() => None
      case x => ds.error(s"Mais de um valor? $x.")
   }

   def write(ds: Ds, cm: Array[Array[Int]] = null) {
      if (!existia) {
         if (cm != null) ds.write(s"insert into r values ($id, $pid, ${instantFun(cm)})")
         else value match {
            case Some(v) => ds.write(s"insert into r values ($id, $pid, $v)")
            case None => ds.log(s"Pool $r.$f incompleto. Impossivel calcular a medida $this.")
         }
      }
   }

   def sqlToWrite(ds: Ds, cm: Array[Array[Int]] = null) = {
      if (!existia) {
         if (cm != null) s"insert into r values ($id, $pid, ${instantFun(cm)})"
         else value match {
            case Some(v) =>
               try {
                  s"insert into r values ($id, $pid, $v)"
               } catch {
                  case exp: Exception => ds.log(exp.getMessage, 30); "select 1"
               }
            case None =>
               ds.log(s"Pool $r.$f incompleto. Impossivel calcular a medida $this.")
               "select 1"
         }
      } else "select 1"
   }

   //   protected def qs2hs(qs: Int) = qs - ds.nclasses + 1
   //
   //   protected def t2qs(t: Int) = t + 1
   //
   //   protected def t2hs(t: Int) = qs2hs(t2qs(t))
   //
   //   protected def t2hi(t: Int) = t - ds.nclasses
}

//object InstantMeasure {
//   def apply() = 0
//
//   def unapply() = 0
//}
//
//object RangeMeasure {
//   def apply() = 0
//
//   def unapply() = 0
//}

sealed trait InstantMeasure extends Measure {
   val t: Int
   protected lazy val cms = {
      if (t < ds.nclasses - 1 || t >= ds.expectedPoolSizes(Global.folds).min)
         ds.error(s"t $t fora dos limites t:[${ds.nclasses};${ds.expectedPoolSizes(Global.folds).min}]")
      ds.getCMs(pid)(t, t)
   }
   lazy val value = if (cms.isEmpty) None else Some(instantFun(cms(t)))
}

sealed trait RangeMeasure extends Measure {
   val ti: Int
   val tf: Int
   protected val rangeFun: (Seq[Array[Array[Int]]]) => (Array[Array[Int]] => Double) => Double
   protected lazy val cms = {
      if (ti > tf || tf <= ds.nclasses || tf >= ds.expectedPoolSizes(Global.folds).min)
         ds.error(s"ti $ti ou tf $tf fora dos limites ti<=tf tf:]${ds.nclasses};${ds.expectedPoolSizes(Global.folds).min}[")
      ds.getCMs(pid)(ti, tf)
   }
   protected lazy val calc = rangeFun(cms.values.toSeq)
   lazy val value = if (cms.size != tf - ti + 1) None else Some(calc(instantFun))
}

case class BalancedAcc(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val t: Int)
   extends InstantMeasure {
   val id = 100000000 + t * 10000
   protected val instantFun = accBal _
}

case class Kappa(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val t: Int)
   extends InstantMeasure {
   val id = 200000000 + t * 10000
   protected val instantFun = kappa _
}

case class ALCBalancedAcc(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val ti: Int, val tf: Int)
   extends RangeMeasure {
   val id = 300000000 + ti * 10000 + tf
   protected val instantFun = accBal _
   protected val rangeFun = ALC _
}

case class ALCKappa(ds: Ds, s: Strategy, l: Learner, r: Int, f: Int)(val ti: Int, val tf: Int)
   extends RangeMeasure {
   val id = 400000000 + ti * 10000 + tf
   protected val instantFun = kappa _
   protected val rangeFun = ALC _
}
