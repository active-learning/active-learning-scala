package clean.tex

import java.io.FileWriter

import clean.lib._
import ml.classifiers.NoLearner
import util.{Stat, StatTests}

object tabMetaLeas extends App with StratsTrait with LearnerTrait with CM {
  Global.debug = 50
  val context = this.getClass.getName
  val ls = (args(0).split(",") map str2learner()).map(_.limp).toBuffer
  //defr-a equivale a maj, com a vantagem de nunca dar zero no LOO;
  // como chu tá com bug, suponho que o mesmo acima valha para usar rnd-r no lugar dele.
  val db = new Db("metanew", true)
  val mcs = List("RoF500", "PCT", "RFw500", "ABoo500", "maj", "chu")
  val sts = stratsTexForGraficoComplexo map (_(NoLearner()).limp)
  db.open()
  val tudo = for {
    fi <- Seq("f", "i")
    st <- sts
  } yield {
      val nome = st + (if (fi == "f") "¹" else "²")
      val medidas3 = mcs map { mc =>
        val m = ls.zipWithIndex.map { case (l, i) => l -> i }.toMap
        val runs = for (run <- 0 to 4) yield {
          val sql = s"select esp,pre,count(0) from tenfold where $fi='th' and st='$st' and run=$run and ls='$ls' and mc='$mc' group by esp,pre"
          val cm = Array.fill(ls.size)(Array.fill(ls.size)(0))
          db.readString(sql) foreach { case Vector(esp, pre, v) => cm(m(esp))(m(pre)) = v.toInt }
          if (cm.flatten.sum != 90) {
            println(s"${sql}; <- sql")
            justQuit(s"$fi $st $mc " + cm.flatten.sum.toString)
          }
          Vector(acc(cm), accBal(cm), kappa(cm))
        }
        runs.toVector.transpose map Stat.media_desvioPadrao
      }
      //
      //      //pro bonferroni:
      //      import scala.sys.process._
      //      val tab = cmss.map(_ map acc).dropRight(1).transpose
      //      val vals = tab.transpose.flatten.mkString(",")
      //      val fw = new FileWriter("/run/shm/asd")
      //      fw.write(s"friedman.test(Accuracy ~ algorithm|dataset, data=data.frame(dataset = rep(seq(90), 5), algorithm = rep(c(${mcs.dropRight(1).map(x => "\"" + x + "\"").mkString(",")}),each=90),Accuracy=c($vals)))")
      //      fw.close()
      //      val log = (Seq("Rscript", "--vanilla", "/run/shm/asd") !!).split("\n").toList
      //      println(s"${log} <- log)")
      //      val r = log.find(_.contains("p-value")).get.split(" +").last.toDouble
      //      println(s"${r} <- r")

      medidas3 map (nome -> _)
    }

  val P = "(.*)(500)".r
  val f = (x: String) => java.text.NumberFormat.getNumberInstance(new java.util.Locale("pt", "BR")).format(x.toDouble)
  for (med <- 0 to 2) {
    print("estratégia ")
    println(mcs map {
      case P(x, "500") => x
      case "maj" => "Maj"
      case "chu" => "Alea"
      case x => x
    } mkString "   ")
    tudo.sortBy(x => x.map(_._2(med)._1).sum).reverse foreach { nomesEmedidas =>
      val nome = nomesEmedidas.head._1
      val medidas = nomesEmedidas.map(_._2(med)).map(x => f("%4.2f".format(x._1)) + " / " + f("%4.2f".format(x._2))).mkString(" ")
      println(s"$nome $medidas")
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
