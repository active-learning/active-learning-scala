package clean.tex

import clean.lib._
import util.StatTests

object tableMetaComparaSMOTE extends App {
  val sortedMCs = List("ELM", "maj")
  //  val sortedMCs = List("ELM", "PCT", "RFw", "ELM+\\newline PCT", "maj")

  def ord2(l: List[Vector[String]]) = {
    val u = sormcs2.zipWithIndex.sortBy(_._1)
    l.sortBy(_(1)).zip(u).sortBy(_._2._2).map(_._1)
  }

  val db = new Db("meta", true) //true=faster
  db.open()
  val mcs = sortedMCs.map {
    case "ELM+\\newline PCT" => "'PE'"
    case "PCT" => "'PCT'"
    case "ELM" => "'ELM'"
    case "RFw" => "'RFw1000'"
    case "C4.5w" => "'C4.55'"
    case x => s"'$x'"
  }.mkString(",")
  val sqlsem = "select st,mc,abts,dbts,htr from r where rs=10 AND fs=10 and nt=1000 and (sm = 'nosm') and mc in (" + mcs + ") and fsel not like 'pca%'  and ra!='ac2' order by st,mc;"
  val sqlcom = sqlsem.replace("'nosm'", "'sm50'")
  val dbrows = db.readString(sqlsem) zip db.readString(sqlcom)
  if (!db.readString(sqlsem).map(_.take(2).mkString).sameElements(db.readString(sqlcom).map(_.take(2).mkString))) ???

  val sormcs2 = sortedMCs ++ sortedMCs.map(_ + "s")
  val tableheader = "\t& " + sormcs2.mkString(" & ") + " \\\\ \\hline"
  val tablerows0 = dbrows.groupBy(_._1.head).values.toList.sortBy(_.head._1.head).map { case veclst0 =>
    val veclst = veclst0.map(x => Seq(x._1, x._2)).flatten
    val strat = veclst.head.head
    def f(nu: Double) = (nu * 100).round.toString
    val vord = ord2(veclst) map { case Vector(st, metacla, acc, dev, _) => f(acc.toDouble) }
    val Min = vord.minBy(_.toDouble)
    lazy val Maj = vord.last
    val Max = vord.maxBy(_.toDouble)
    val strs = ord2(veclst).map(z => (z(0), z(1), f(z(2).toDouble), f(z(3).toDouble), z(4))) map {
      case (st, metacla, Maj, dev, _) => s"$Maj/$dev";
      case (st, metacla, Max, dev, _) => s"\\textcolor{blue}{\\textbf{$Max}}/$dev";
      case (st, metacla, Min, dev, _) => s"\\textcolor{red}{$Min}/$dev";
      case (st, metacla, ac, dev, _) => s"$ac/$dev";
      case x => db.error(s"$x <- x ALERTA")
    } //++ List(s"${"%2d".format(((Min.toDouble - maj) * 100).round)}")
    //    val hx = veclst.head(4).split(";").head.split(" ").map(_.toDouble).max
    //    val hn = veclst.head(4).split(";").head.split(" ").map(_.toDouble).min
    strat + "\t& " + strs.mkString(" & ")
    //    + " & " + "%2d".format((100 * hx / hn).round) //max/min
  }
  //  tablerows0 foreach println
  val tablerows = tablerows0.sortBy { x =>
    val vs = x.split("&")
    vs(3).split("/").head.replace("\\textcolor{", "").replace("\\textbf{", "").replace("{", "").replace("}", "").replace("red", "").replace("blue", "").toDouble - vs(1).split("/").head.replace("\\textcolor{", "").replace("\\textbf{", "").replace("}", "").replace("{", "").replace("red", "").replace("blue", "").toDouble
  }.reverse.mkString(" \\\\ \n")
  println(s"\\begin{table}[h]\n\\begin{center}\n\\begin{tabular}{l|l" + Seq.fill(tableheader.split("&").size - 2)("l").mkString + "}")
  println(s"$tableheader")
  println(s"$tablerows")
  println("\\end{tabular}\n\\end{center}\n\\end{table}")
  db.close()


  val tab = dbrows.groupBy(_._1.head).values.toList.sortBy(_.head._1.head).map { case veclst0 =>
    val veclst = veclst0.map(x => Seq(x._1, x._2)).flatten
    val strat = veclst.head.head
    val vals = ord2(veclst) map {
      case Vector(st, metacla, acc, dev, _) => acc.toDouble
      case x => db.error(s"${x} <- x ALERTA")
    }
    strat -> vals
  }
  val pairs = StatTests.friedmanNemenyi(tab, sormcs2.toVector)
  val fri = StatTests.pairTable(pairs, "stratsfriedpares", 2, "fried")
  println(s"$fri")
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
