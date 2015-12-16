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

import clean.lib.Ds

import scala.sys.process._

object WilcoxonStrats extends App {
  val metads = Ds("metanew", readOnly = true)
  val sts = "ATUeuc,ATUman,Clu,DWeuc,DWman,EERacc,EERent,HTUeuc,HTUman,Mar,Rnd,SGmulti,TUeuc,TUman".split(",")
  val seq = Seq(("5NNw", "ti"), ("C4.52", "ti"), ("NB", "ti"), ("SVM", "ti"), ("5NNw", "th"), ("C4.52", "th"), ("NB", "th"), ("SVM", "th"))
  val ls = "HTUeuc-EERent"
  metads.open()
  val logs = seq map { case (st, i) =>
    val sql = s"select a.spea,b.spea from rank a, rank b where a.ds=b.ds and a.ra=b.ra and a.cr=b.cr and a.i=b.i and a.f=b.f and a.st=b.st and a.ls=b.ls and a.rs=b.rs and a.fs=b.fs and a.nt=b.nt and a.porPool=b.porPool and a.mc='PCTr' and b.mc='defr' and a.st='$st' and a.i='$i' and a.ls='$ls'"
//    println(s"${sql} <- sql")
    val t = metads.read(sql)
    val (a, b) = t.map(x => x(0) -> x(1)).unzip
    val fw = new FileWriter("/run/shm/asd")
    fw.write("x=c(" + a.mkString(",") + ");y=c(" + b.mkString(",") + ");wilcox.test(x,y,paired=TRUE,exact=F)")
    fw.close()
    val log = (Seq("Rscript", "--vanilla", "/run/shm/asd") !!).split("\n").toList
    val r = log.find(_.contains("p-value")).get.split(" +")(5).toDouble
    println(s"${st.replace("w","").replace("2","w")} $i " + a.sum / a.size + " " + b.sum / b.size + " " + r)
    log
  }
  metads.close()
  println(s"${} <- ")
  logs foreach println
}