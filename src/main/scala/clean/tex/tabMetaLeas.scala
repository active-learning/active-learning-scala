package clean.tex

import clean.lib._
import ml.classifiers.NoLearner
import util.StatTests

object tabMetaLeas extends App with StratsTrait with LearnerTrait with CM {
  Global.debug = 5
  val context = this.getClass.getName
  val ls = (args(0).split(",") map str2learner()).map(_.limp).toBuffer
  //defr-a equivale a maj, com a vantagem de nunca dar zero no LOO;
  // como chu tá com bug, suponho que o mesmo acima valha para usar rnd-r no lugar dele.
  val db = new Db("metanew", true)
  val mcs = List("RoF500", "PCTr-a", "RFw500", "ABoo500", "defr-a", "rndr-a")
  val sts = stratsTexForGraficoComplexo map (_(NoLearner()).limp)
  db.open()
  val tudo = Seq("f", "i") map { fi =>
    sts map { st =>
      val nome = st + (if (fi == "f") "¹" else "²")
      val (cmss, medidas3) = (mcs map { mc =>
        val m = ls.zipWithIndex.map { case (l, i) => l -> i }.toMap
        val sql = s"select ds,esp,pre,count(0) from acc where tr='ts' and $fi='th' and st='$st' and ls='$ls' and mc='$mc' group by ds,esp,pre"
        val cms = db.readString(sql).groupBy(_.head).map(x => x._1 -> x._2.map(_.tail)) map { case (ds, list) =>
          val cm = Array.fill(ls.size)(Array.fill(ls.size)(0))
          list foreach { case Vector(esp, pre, v) => cm(m(esp))(m(pre)) = v.toInt }
          if (cm.flatten.sum < 25) justQuit(s"$fi $st $mc " + cm.flatten.sum.toString)
          cm.toList.map(_.toList)
        }
        if (cms.size != 90) justQuit(s"${cms.size} != 90 bases requerido\n $sql")
        val CM = cms.reduce((a, b) => a.flatten.zip(b.flatten).map(x => x._1 + x._2).grouped(cms.head.size).toList).toArray.map(_.toArray)
        cms.map(_.toArray.map(_.toArray)) ->((100 * acc(CM)).round / 100d, (100 * accBal(CM)).round / 100d, (100 * kappa(CM)).round / 100d)
      }).unzip

      //Fried
      val tab = cmss.map(_ map acc).dropRight(1).transpose
      println(s"${} <   - ")
      tab foreach (x => println(x.mkString(" ")))
      println(s"${} <- ")

      t3map(medidas3.unzip3)(nome -> _)
    }
  }
  val fla = tudo.flatten.toList
  val txt = t3map(fla.unzip3) { case nomesEmedidas =>
    val sorted = nomesEmedidas.sortBy(_._2.sum).reverse
    val header = Seq("estratégia") ++ mcs mkString " "
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
