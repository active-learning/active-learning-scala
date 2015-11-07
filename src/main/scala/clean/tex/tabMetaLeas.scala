package clean.tex

import clean.lib._
import ml.classifiers.NoLearner
import util.StatTests

object tabMetaLeas extends App with StratsTrait with LearnerTrait with CM {
  val context = this.getClass.getName
  val ls = (args(0).split(",") map str2learner()).map(_.limp).toBuffer
  val mcs = List("RoF500", "PCTr-a", "RFw500", "ABoo500", "defr-a")
  val db = new Db("metanew", true)
  val sts = stratsTexForGraficoComplexo map (_(NoLearner()).limp)
  db.open()
  val tudo = Seq("f", "i").par map { fi =>
    sts.par map { st =>
      val nome = st + (if (fi == "f") " [|Y|;50]" else " [51;100]")
      val medidas3 = (mcs.par map { mc =>
        val m = ls.zipWithIndex.map { case (l, i) => l -> i }.toMap
        val cm = Array.fill(ls.size)(Array.fill(ls.size)(0))
        val sql = s"select esp,pre,count(0) from e where fs=90 and $fi='th' and st='$st' and leas='$ls' and mc='$mc' and run=-1 and fold=-1 group by esp,pre"
        //        println(s"${sql} <- sql")
        db.readString(sql) foreach {
          case Vector(a, b, v) => cm(m(a))(m(b)) = v.toInt
        }
        ((100 * acc(cm)).round / 100d, (100 * accBal(cm)).round / 100d, (100 * kappa(cm)).round / 100d)
        //      cm.map(_.toList) foreach println
      }).unzip3
      t3map(medidas3)(nome -> _.toList)
    }
  }
  val fla = tudo.flatten.toList
  val txt = t3map(fla.unzip3) { case nomesEmedidas =>
    val sorted = nomesEmedidas.sortBy(_._2.sum).reverse
    val header = Seq("estratégia", "orçamento") ++ mcs mkString " "
    println(header)
    sorted foreach { case (nome, meds) =>
      println(s"$nome " + meds.mkString(" "))
    }
  }
  db.close()

  //  val pairs = StatTests.friedmanNemenyi(tab, mcs.toVector)
  //  val fri = StatTests.pairTable(pairs, "stratsfriedpares", 2, "fried")
  //  println(s"${fri}")
  def t2map[A, B](as: (A, A))(f: A => B) = as match {
    case (a1, a2) => (f(a1), f(a2))
  }

  def t3map[A, B](as: (A, A, A))(f: A => B) = as match {
    case (a1, a2, a3) => (f(a1), f(a2), f(a3))
  }
}

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
