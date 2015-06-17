package clean.tex

import java.io.{File, FileWriter}

import clean.lib._
import ml.Pattern
import ml.classifiers._
import util.{Datasets, Stat, Tempo}

import scala.io.Source

object tableMeta extends App {
  val db = new Db("meta", false)
  db.open()
  val dbrows = db.readString("select st,metacla,accts,devts from resultadosmeta where rank='acc' and rs=10 and fs=10 and leas='5nna,rbf,rf,c45,nbb' order by metacla")
  val tableheader = "\t& " + dbrows.groupBy(_.head).values.flatten.map(_(1)).toList.distinct.sorted.mkString(" & ") + " \\\\ \\hline"
  val tablerows = dbrows.groupBy(_.head).values.toList.sortBy(_.head.head).map { case veclst =>
    val strat = veclst.head.head
    val Max = (veclst map { case Vector(st, metacla, acc, dev) => acc }).max
    val strs = veclst.sortBy(_(1)) map {
      case Vector(st, metacla, Max, dev) => s"\\textbf{${"%2d".format((Max.toDouble * 100).round)}}/${"%2d".format((dev.toDouble * 100).round)}";
      case Vector(st, metacla, acc, dev) => s"${"%2d".format((acc.toDouble * 100).round)}/${"%2d".format((dev.toDouble * 100).round)}";
      case x => db.error(s"${x} <- x ALERTA")
    }
    strat + "\t& " + strs.mkString(" & ")
  }.mkString(" \\\\ \n")
  println(s"\\begin{table}[h]\n\\begin{center}\n\\begin{tabular}{l|" + Seq.fill(tablerows.split("\n").size - 1)("r").mkString + "}")
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
