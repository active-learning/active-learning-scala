package clean.tex

import java.io.{File, FileWriter}

import clean.lib._
import ml.Pattern
import ml.classifiers._
import util.{StatTests, Datasets, Stat, Tempo}

import scala.io.Source

object tableMeta extends App {
  /*
+---------+
| mc      |
+---------+
| 5NN     |
| C4.55   |
| chu     |
| CIELM   |
| defr-a  |
| ELM     |
| ELMr-a  |
| maj     |
| PCT     |
| PCTr-a  |
| PE      |
| PEr-a   |
| RFw1000 |
| RFw250  |
| rndr-a  |
| SVM     |
+---------+
'ELM','PCT','RFw1000','C4.55','5NN','CIELM','SVM','maj'
   */
  val acc = args.head == "ac"
  val sortedMCs = if (acc) List("ELM", "PCT", "RFw", "ELM+\\newline PCT", "5NN", "maj") //maj tem que ser o último!
  else List("ELM", "PCT", "PCT+ ELM", "def")

  def ord2(l: List[Vector[String]]) = {
    val u = sortedMCs.zipWithIndex.sortBy(_._1)
    l.sortBy(_(1)).zip(u).sortBy(_._2._2).map(_._1)
  }

  val db = new Db("meta", true) //true=faster
  db.open()
  val sqla = "select st,mc,abts,dbts,htr from r where rs=10 AND fs=10 and nt=1000 and (sm like 'nosm%') and mc in (" + sortedMCs.map {
    case "ELM+\\newline PCT" => "'PE'"
    case "PCT" => "'PCT'"
    case "ELM" => "'ELM'"
    case "RFw" => "'RFw1000'"
    case "C4.5w" => "'C4.55'"
    case x => s"'$x'"
  }.mkString(",") + ") and fsel not like 'pca%'  and ra!='ac2' order by abts;"
  val sqlr = "select st,mc,ats,dts,htr from r where rs=10 AND fs=10 and nt=1000 and (sm like 'nosm%') and mc in (" + sortedMCs.map {
    case "PCT+ ELM" => "'PEr'"
    case "PCT" => "'PCTr'"
    case "ELM" => "'ELMr'"
    case "def" => "'defr'"
    case x => s"'$x'"
  }.mkString(",") + ") and fsel not like 'pca%'  and ra!='ac2' order by ats;"
  val sql = if (acc) sqla else sqlr
  println(s"${sql}")
  val dbrows = db.readString(sql)

  //  val tableheader = "\t& " + (sortedMCs ++ List("$\\delta$", "$\\frac{\\maj}{\\min}$")).mkString(" & ") + " \\\\ \\hline"
  val tableheader = "\t& " + (sortedMCs).mkString(" & ") + " \\\\ \\hline"
  val tablerows0 = dbrows.groupBy(_.head).values.toList.sortBy(_.head.head).map { case veclst =>
    val strat = veclst.head.head
    def f(nu: Double) = if (acc) (nu * 100).round.toString else "%4.3f".format((nu * 1000).round / 1000d)
    val vord = ord2(veclst) map { case Vector(st, metacla, acc, dev, _) => f(acc.toDouble) }
    val Min = vord.dropRight(1).minBy(_.toDouble)
    lazy val Maj = vord.last
    val Max = vord.dropRight(1).maxBy(_.toDouble)
    val strs = ord2(veclst).map(z => (z(0), z(1), f(z(2).toDouble), f(z(3).toDouble), z(4))) map {
      case (st, metacla, Maj, dev, _) => s"${Maj}/${dev}";
      case (st, metacla, Max, dev, _) => s"\\textcolor{blue}{\\textbf{${Max}}}/${dev}";
      case (st, metacla, Min, dev, _) => s"\\textcolor{red}{${Min}}/${dev}";
      case (st, metacla, ac, dev, _) => s"${ac}/${dev}";
      case x => db.error(s"${x} <- x ALERTA")
    } //++ List(s"${"%2d".format(((Min.toDouble - maj) * 100).round)}")
    //    val hx = veclst.head(4).split(";").head.split(" ").map(_.toDouble).max
    //    val hn = veclst.head(4).split(";").head.split(" ").map(_.toDouble).min
    strat + "\t& " + strs.mkString(" & ")
    //    + " & " + "%2d".format((100 * hx / hn).round) //max/min
  }
  //  tablerows0 foreach println
  val tablerows = tablerows0.sortBy(x => x.split("&").last.split("/").head.toDouble).reverse.mkString(" \\\\ \n")
  println(s"\\begin{table}[h]\n\\begin{center}\n\\begin{tabular}{l|l" + Seq.fill(tableheader.split("&").size - 2)("l").mkString + "}")
  println(s"${tableheader}")
  println(s"${tablerows}")
  println("\\end{tabular}\n\\end{center}\n\\end{table}")
  db.close()


  val tab = dbrows.groupBy(_.head).values.toList.sortBy(_.head.head).map { case veclst =>
    val strat = veclst.head.head
    val vals = ord2(veclst) map {
      case Vector(st, metacla, acc, dev, _) => acc.toDouble
      case x => db.error(s"${x} <- x ALERTA")
    }
    strat -> vals
  }
  val pairs = StatTests.friedmanNemenyi(tab, sortedMCs.toVector)
  val fri = StatTests.pairTable(pairs, "stratsfriedpares", 2, "fried")
  println(s"${fri}")
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
