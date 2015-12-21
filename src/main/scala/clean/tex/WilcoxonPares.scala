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

package clean.tex

import java.io.FileWriter

import clean.lib._

import scala.sys.process._

object WilcoxonPares extends App with StratsTrait with LearnerTrait {
  val ls = args(0).split(",") map str2learner()
  val metads = Ds("metanew", readOnly = true)
  val combstrats = (2 to stratsPMetaStrat.size).flatMap(n => stratsPMetaStrat.combinations(n).toList)
  val combleas = (2 to ls.size).flatMap(n => ls.combinations(n).toList)
  val f = (x: Double) => {
    val r = java.text.NumberFormat.getNumberInstance(new java.util.Locale("pt", "BR")).format(x)
    r
  }

  val seq = for {
    i <- Seq("f", "i")
    sts1 <- combstrats
    les1 <- combleas
  } yield {
      val pares1 = for {s <- sts1; l <- les1} yield s -> l
      val leas = pares1.map(x => x._1(x._2).limp + "-" + x._2.limp)
      val nome = leas.mkString(",") + (if (i == "f") "¹" else "²")

      val sql = s"select a.spea,b.spea from rank a, rank b where a.ds=b.ds and a.ra=b.ra and a.cr=b.cr and a.i=b.i and a.f=b.f and a.st=b.st and a.ls=b.ls and a.rs=b.rs and a.fs=b.fs and a.nt=b.nt and a.porPool=b.porPool and a.mc='PCTr' and b.mc='defr' and a.st='par' and a.$i='th' and a.ls='${leas.mkString(";")}'"
      //      println(s"${sql}; ")
      val t = metads.read(sql)
      val (a, b) = t.map(x => x(0) -> x(1)).unzip
      val fw = new FileWriter("/run/shm/asd")
      fw.write("x=c(" + a.mkString(",") + ");y=c(" + b.mkString(",") + ");wilcox.test(x,y,paired=TRUE,exact=F)")
      fw.close()
      val log = (Seq("Rscript", "--vanilla", "/run/shm/asd") !!).split("\n").toList
      val r = log.find(_.contains("p-value")).get.split(" +")(5).toDouble
      (nome, r, i, a.sum / a.size, b.sum / b.size)
    }
  seq.sortBy { case (nome, r, i, a, b) => a + b }.reverse foreach { case (nome, r, i, a, b) =>
    println(s"${f(r)} ${(if (i == "f") "" else "___") + nome.replace("w", "").replace("2", "w").replace("EERent-5NN,EERent-NB,EERent-C4.5w,EERent-SVM,HTUeuc-5NN,HTUeuc-NB,HTUeuc-C4.5w,HTUeuc-SVM,Clu-5NN,Clu-NB,Clu-C4.5w,Clu-SVM","Todos os pares")} " + f(a) + " " + f(b))
  }
  metads.close()
}