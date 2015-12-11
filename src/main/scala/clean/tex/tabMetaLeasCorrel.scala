package clean.tex

import clean.lib._
import ml.classifiers.NoLearner

object tabMetaLeasCorrel extends App with StratsTrait with LearnerTrait with CM {
  val context = this.getClass.getName
  val ls = (args(0).split(",") map str2learner()).map(_.limp).toBuffer
  val mcs = List("PCTr", "defr", "rndr")
  val db = new Db("metanew", true)
  val sts = stratsTexForGraficoComplexo map (_(NoLearner()).limp)
  db.open()
  val tudo = Seq("f", "i") map { fi =>
    sts map { st =>
      val nome = st + (if (fi == "f") "¹" else "²")
      val medidas = mcs map { mc =>
        val sql = s"select ats from r where $fi='th' and st='$st' and ra='ra' and mc='$mc' and ls='5nnw,nbb,c452,rbf' and fs=90 and nt=500 and cr=1 and rs=1 and dsminsize=100 and porPool='false' order by ats desc" // orderby inutil
        db.read(sql).head.head //já tirou a media antes de gravar no mysql
      }
      nome -> medidas.toList
    }
  }
  val fla = tudo.flatten.toList
  val txt = fla.sortBy(_._2.sum).reverse
  val header = Seq("estratégia") ++ mcs mkString " "
  println(header)
  txt foreach { case (nome, meds) =>
    println(s"$nome " + meds.mkString(" "))
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
