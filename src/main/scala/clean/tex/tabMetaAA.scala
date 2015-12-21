package clean.tex

import al.strategies.RandomSampling
import clean.lib._
import ml.classifiers.Learner
import util.Stat

object tabMetaAA extends App with StratsTrait with LearnerTrait with CM {
  Global.debug = 50
  val context = this.getClass.getName
  val ls = args(0).split(",") map str2learner()
  //defr-a equivale a maj, com a vantagem de nunca dar zero no LOO;
  // como chu tá com bug, suponho que o mesmo acima valha para usar rnd-r no lugar dele.
  val db = new Db("metanew", true)
  val mcs = List("RoF500", "RFw500", "PCT", "ABoo500", "maj", "chu")
  val sts1 = stratsPMetaStrat
  val pares = for {s <- sts1:+((_: Learner) => RandomSampling(Seq())); l <- ls} yield s -> l
  val txts = pares.map(x => x._1(x._2).limp + "-" + x._2.limp)

  val combstrats = (1 to 1).flatMap(n => stratsTexForGraficoComplexoSemRnd.combinations(n).toList)
  val combleas = (1 to 1).flatMap(n => ls.combinations(n).toList)

  db.open()
  val tudo = for {
    fi <- Seq("f", "i").par
    sts1 <- combstrats
    les1 <- combleas
  } yield {
      val pares1 = (for {s <- sts1; l <- les1} yield s -> l) ++ (for {s <- Seq((_: Learner) => RandomSampling(Seq())); l <- les1} yield s -> l)
      val leas = pares1.map(x => x._1(x._2).limp + "-" + x._2.limp)
      val nome = leas.mkString(",") + (if (fi == "f") "¹" else "²")
      val medidas3 = mcs map { mc =>
        val m = txts.zipWithIndex.map { case (l, i) => l -> i }.toMap
        val runs = for (run <- 0 to 4) yield {
          val sql = s"select esp,pre,count(0) from tenfold where $fi='th' and st='aa' and run=$run and ls = '$leas' and mc='$mc' group by esp,pre"
          val cm = Array.fill(txts.size)(Array.fill(txts.size)(0))
          //          db.readString(sql) foreach println
          db.readString(sql) foreach { case Vector(esp, pre, v) => cm(m(esp))(m(pre)) = v.toInt }
          if (cm.flatten.sum != 90) {
            println(s"${sql}; <- sql")
            justQuit(s"$fi par $mc " + cm.flatten.sum.toString)
          }
          Vector(acc(cm), accBal(cm), kappa(cm))
        }
        runs.toVector.transpose map Stat.media_desvioPadrao
      }
      medidas3 map (nome -> _)
    }

  val P = "(.*)(500)".r
  val f = (x: String) => java.text.NumberFormat.getNumberInstance(new java.util.Locale("pt", "BR")).format(x.toDouble)
  for (med <- 2 to 2) {
    print("Pares disponíveis ")
    println(mcs map {
      case P(x, "500") => x
      case "maj" => "Maj"
      case "chu" => "Alea"
      case x => x
    } mkString "   ")
    tudo.toList.sortBy(x => x.map(_._2(med)._1).sum).reverse foreach { nomesEmedidas =>
      val nome = nomesEmedidas.head._1
      val i = if (nome.endsWith("¹")) "f" else "i"
      val medidas = nomesEmedidas.map(_._2(med)).map(x => f("%4.2f".format(x._1)) + " / " + f("%4.2f".format(x._2))).mkString(" ")
      println(s"${if (i == "f") "" else "___"}${nome.replace("w", "").replace("2", "w").replace("EERent-5NN,EERent-NB,EERent-C4.5w,EERent-SVM,HTUeuc-5NN,HTUeuc-NB,HTUeuc-C4.5w,HTUeuc-SVM,Clu-5NN,Clu-NB,Clu-C4.5w,Clu-SVM", "Todos_os_pares")} $medidas")
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
