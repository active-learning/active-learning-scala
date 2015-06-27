package clean.tex

import java.io.{File, FileWriter}

import clean.lib._
import ml.Pattern
import ml.classifiers._
import util.{Datasets, Stat, Tempo}

import scala.io.Source

object tableMeta extends App {
  //PE não aparece!
  //  val ord = List("ELM","ELMr-a","PCT","PCTr-a","PE","PEr-a","RFw1000","C4.55","maj")
  val ord = List("ELM", "ELMr-a", "PCT", "PCTr-a", "PEr-a", "RFw1000", "C4.55", "maj")

  def ord2(l: List[Vector[String]]) = {
    val u = ord.zipWithIndex.sortBy(_._1)
    l.sortBy(_(1)).zip(u).sortBy(_._2._2).map(_._1)
  }

  val db = new Db("meta", true)
  db.open()
  val dbrows = db.readString("select st,mc,abts,dbts,htr from r where rs=10 AND fs=10 and nt=1000 and (sm like 'nosm%') and mc in ('ELM','ELMr-a','PCT','PCTr-a','PE','PEr-a','RFw1000','C4.55','maj') and fsel not like 'pca%' order by abts;")
  //  val tableheader = "\t& " + ord(dbrows.groupBy(_.head).values.flatten.map(_(1)).toList.distinct).mkString(" & ") + " \\\\ \\hline"
  val tableheader = "\t& " + (ord ++ List("$\\delta$", "$\\frac{\\maj}{\\min}$")).mkString(" & ") + " \\\\ \\hline"
  val tablerows = dbrows.groupBy(_.head).values.toList.sortBy(_.head.head).map { case veclst =>
    val strat = veclst.head.head
    val Min = (veclst map { case Vector(st, metacla, acc, dev, _) => acc }).min
    val Max = (veclst map { case Vector(st, metacla, acc, dev, _) => acc }).max
    val strs = (ord2(veclst) map {
      case Vector(st, metacla, Max, dev, _) => s"\\textcolor{blue}{\\textbf{${"%2d".format((Max.toDouble * 100).round)}}}/${"%2d".format((dev.toDouble * 100).round)}";
      case Vector(st, metacla, Min, dev, _) => s"\\textcolor{red}{${"%2d".format((Min.toDouble * 100).round)}}/${"%2d".format((dev.toDouble * 100).round)}";
      case Vector(st, metacla, acc, dev, _) => s"${"%2d".format((acc.toDouble * 100).round)}/${"%2d".format((dev.toDouble * 100).round)}";
      case x => db.error(s"${x} <- x ALERTA")
    }) ++ List(s"${"%2d".format(((Max.toDouble - Min.toDouble) * 100).round)}")
    val hx = veclst.head(4).split(";").head.split(" ").map(_.toDouble).max
    val hn = veclst.head(4).split(";").head.split(" ").map(_.toDouble).min
    strat + "\t& " + strs.mkString(" & ") + " & " + "%2d".format((100 * hx / hn).round)
  }.sortBy(_.split("&")(ord.size).split("\\\\\\\\").head.toDouble).reverse.mkString(" \\\\ \n")
  println(s"\\begin{table}[h]\n\\begin{center}\n\\begin{tabular}{l|" + Seq.fill(tableheader.split("&").size - 1)("r").mkString + "}")
  println(s"${tableheader}")
  println(s"${tablerows}")
  println("\\end{tabular}\n\\end{center}\n\\end{table}")
  db.close()
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
