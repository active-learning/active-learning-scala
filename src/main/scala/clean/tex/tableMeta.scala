package clean.tex

import java.io.{File, FileWriter}

import clean.lib._
import ml.Pattern
import ml.classifiers._
import util.{StatTests, Datasets, Stat, Tempo}

import scala.io.Source

object tableMeta extends App {
  val acc = args.head == "ac"
  //PE tá treinando!
  //    val sortedMCs= List("ELM","ELMr-a","PCT","PCTr-a","PE","PEr-a","RFw","C4.5","maj")
  val sortedMCs = if (acc) List("PCT+ ELM", "ELM", "PCT", "RFw", "C4.5w", "5NN", "maj") //maj tem que ser o último!
  else List("PCT+ ELM", "ELM", "PCT", "def")

  def ord2(l: List[Vector[String]]) = {
    val u = sortedMCs.zipWithIndex.sortBy(_._1)
    l.sortBy(_(1)).zip(u).sortBy(_._2._2).map(_._1)
  }

  val db = new Db("meta", true) //true=faster
  db.open()
  val dbrows = if (acc) db.readString("select st,mc,abts,dbts,htr from r where rs=10 AND fs=10 and nt=1000 and (sm like 'nosm%') and mc in (" + sortedMCs.map {
    case "PCT+ ELM" => "'PEr-a'"
    case "PCT" => "'PCTr-a'"
    case "ELM" => "'ELMr-a'"
    case "RFw" => "'RFw1000'"
    case "C4.5w" => "'C4.55'"
    case x => s"'$x'"
  }.mkString(",") + ") and fsel not like 'pca%' order by abts;")
  else db.readString("select st,mc,ats,dts,htr from r where rs=10 AND fs=10 and nt=1000 and (sm like 'nosm%') and mc in (" + sortedMCs.map {
    case "PCT+ ELM" => "'PEr'"
    case "PCT" => "'PCTr'"
    case "ELM" => "'ELMr'"
    case "def" => "'defr'"
    case x => s"'$x'"
  }.mkString(",") + ") and fsel not like 'pca%' order by abts;")

  //  val tableheader = "\t& " + (sortedMCs ++ List("$\\delta$", "$\\frac{\\maj}{\\min}$")).mkString(" & ") + " \\\\ \\hline"
  val tableheader = "\t& " + (sortedMCs).mkString(" & ") + " \\\\ \\hline"
  val tablerows0 = dbrows.groupBy(_.head).values.toList.sortBy(_.head.head).map { case veclst =>
    val strat = veclst.head.head
    val vord = ord2(veclst) map { case Vector(st, metacla, acc, dev, _) => acc }
    val Min = vord.dropRight(1).min //maj nao interessa
  lazy val maj = vord.last.toDouble
    val Max = (veclst map { case Vector(st, metacla, acc, dev, _) => acc }).max
    val strs = ord2(veclst) map {
      case Vector(st, metacla, Max, dev, _) => s"\\textcolor{blue}{\\textbf{${"%2d".format((Max.toDouble * 100).round)}}}/${"%2d".format((dev.toDouble * 100).round)}";
      case Vector(st, metacla, Min, dev, _) => s"\\textcolor{red}{${"%2d".format((Min.toDouble * 100).round)}}/${"%2d".format((dev.toDouble * 100).round)}";
      case Vector(st, metacla, acc, dev, _) => s"${"%2d".format((acc.toDouble * 100).round)}/${"%2d".format((dev.toDouble * 100).round)}";
      case x => db.error(s"${x} <- x ALERTA")
    } // ++ List(s"${"%2d".format(((Min.toDouble - maj) * 100).round)}")
    //    val hx = veclst.head(4).split(";").head.split(" ").map(_.toDouble).max
    //    val hn = veclst.head(4).split(";").head.split(" ").map(_.toDouble).min
    strat + "\t& " + strs.mkString(" & ")
    //    + " & " + "%2d".format((100 * hx / hn).round) //max/min
  }
  //  tablerows0 foreach println
  val tablerows = tablerows0.sortBy(_.split("&")(sortedMCs.size).split("/").head.toDouble).reverse.mkString(" \\\\ \n")
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
