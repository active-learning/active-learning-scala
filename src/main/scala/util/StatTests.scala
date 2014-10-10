package util

import scala.collection.immutable.ListMap
import scala.collection.mutable

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
object StatTests {
  def f(x: Double) = if (0 <= x && x <= 1) "%4.3f".format(x) else if (x == -1) "" else x.toInt.toString

  /**
   * Takes a map DatasetName -> strategyMeasuresTheHigherTheBetter
   * and returns a map strategyName -> Vector(wins strat 1?, wins strat 2?, ...)
   * 0: no win
   * 1: 0.9 confidence on win
   * 2: 0.95 confidence on win
   * @param measures
   */
  def friedmanNemenyi(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = strategies.zip(FriedmanTest.Friedman(measures.map(_._2.toArray).toArray, true)).map(x => x._1 -> x._2.toVector)

  /**
   * Winners per row (dataset or pool).
   */
  def winners(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
    val ranked = FriedmanTest.CD(measures.map(_._2.toArray).toArray, true)
    println(ranked)
    strategies.zipWithIndex.filter { case (s, i) =>
      ranked.contains(i)
    }.map(_._1)
  }

  def winsLossesTies(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = ???

  private def table(core: String, nstrats: Int, strategies: Vector[String], tableName: String, measure: String, language: String = "pt") {
    if (nstrats != strategies.size) {
      println(s"Inconsistency #measures-in-the-first-row != strategies.size: $nstrats != ${strategies.size}")
      sys.exit(1)
    }
    val caption = language match {
      case "pt" => s"$measure para os Q exemplos consultados. O maior valor de cada base está em \\textbf{negrito}."
      case "en" => s"$measure for the first Q queried instances. Highest value for each dataset is in \\textbf{bold} face."
    }
    println( """\begin{table}[h]
\caption{""" + caption + """}
\begin{center}
\begin{tabular}{l""" + Seq.fill(nstrats)("c").mkString("|") + "}\n & " + strategies.mkString(" & ") + """\\ \hline """)
    println(core)
    println( """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}""")
  }

  /**
   * prints a Latex table with all data,
   * rows: datasets
   * columns: strategies
   */
  def extensiveTable(measures: Seq[(String, Seq[Double])], strategies: Vector[String], tableName: String, measure: String, seps: Int = 4, language: String = "pt") {
    val nstrats = measures.head._2.length
    val core = measures.zipWithIndex.map { case ((d, l), i) =>
      val vals = l.map { xf =>
        val x = f(xf)
        if (xf == l.max) s"\\textbf{$x}" else x
      }.mkString(" & ")
      s"$d & $vals \\\\" + (if (i % seps == seps - 1) """ \hline""" else "")
    }.mkString("\n")
    table(core, nstrats, strategies, tableName, measure, language)
  }

  def extensiveTable2(measures: Seq[(String, Seq[(Double, Double)])], strategies: Vector[String], tableName: String, measure: String, seps: Int = 4, language: String = "pt"): Unit = {
    val nstrats = measures.head._2.length
    val core = measures.zipWithIndex.map { case ((d, l0), i) =>
      val l = l0 map (x => (x._1 * 1000).round / 1000d -> (x._2 * 1000).round / 1000d)
      val max = l.map(_._1).filter(x => x >= 0 && x <= 1).max
      val Dmax = l.map(_._2).filter(x => x >= 0 && x <= 1).max
      val Dmin = l.map(_._2).filter(x => x >= 0 && x <= 1).min
      val vals = l.map { case (x1f, x2f) =>
        val x1 = f(x1f)
        val x2 = f(x2f)
        val str = if (x1f == max) s"\\textcolor{blue}{\\textbf{$x1}}" else s"$x1" // \\usepackage[usenames,dvipsnames]{color}
        str + (if (x1f != -1 && x2f != -1) "/" else "") + (x2f match {
          case Dmin => s"\\textcolor{green}{\\textbf{$x2}}"
          case Dmax => s"\\textcolor{red}{\\textbf{$x2}}"
          case _ => s"$x2"
        })
      }.mkString(" & ")
      s"$d & $vals \\\\" + (if (i % seps == seps - 1) """ \hline""" else "")
    }.mkString("\n")
    table(core, nstrats, strategies, tableName, measure, language)
  }

  /**
   * prints a Latex table for pairwise comparisons
   */
  def pairTable(pairs: Vector[(String, Vector[Int])], tableName: String, measure: String, seps: Int = 2, language: String = "pt") {
    val caption = language match {
      case "pt" => s"Um contra um: cada asterisco indica quando a estratégia na linha tem melhor $measure que a estratégia na coluna com intervalo de confiança de 0.95."
      case "en" => s"Pairwise comparison: each asterisk indicates that the strategy at the row has better $measure than the strategy at the column within a confidence interval of 0.95."
    }
    println( """\begin{table}[h]
\caption{""" + caption + """}
\begin{center}
\begin{tabular}{l""" + Seq.fill(pairs.size)("c").grouped(seps).map(_.mkString).mkString("|") + "}\n \t\t& " + (1 to pairs.size).mkString(" & ") + """ \\""")

    pairs.zipWithIndex.foreach { case ((s, l), i) =>
      val nr = i + 1
      print(s"$nr - $s\t& " + l.map(x => if (x == 2) "*" else " ").mkString(" & ") + """ \\""")
      if (i % seps == seps - 1) println( """ \hline""") else println("")
    }

    println( """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}""")
  }
}

object FriedmanNemenyiTest extends App {
  val m0 = Seq(
    "d1" -> Seq(5d, 4d, 3d, 2d, 0d),
    "d2" -> Seq(5d, 4d, 3d, 2d, 0d),
    "d3" -> Seq(5d, 4d, 3d, 2d, 0d),
    "d4" -> Seq(5d, 4d, 3d, 22d, 0d),
    "d5" -> Seq(5d, 4d, 3d, 22d, 0d),
    "d6" -> Seq(5d, 4d, 3d, 22d, 0d),
    "d7" -> Seq(5d, 4d, 3d, 22d, 0d),
    "d8" -> Seq(5d, 4d, 3d, 22d, 0d),
    "d9" -> Seq(500d, 4000d, 30000d, 200000d, 200001d),
    "d10" -> Seq(50d, 40d, 3d, 2d, 0d),
    "d11" -> Seq(50d, 4d, 3d, 2d, 0d),
    "d12" -> Seq(50d, 4d, 3d, 2d, 0d),
    "d13" -> Seq(50d, 4d, 3d, 2d, 0d),
    "d14" -> Seq(50d, 4d, 3d, 2d, 1d),
    "d15" -> Seq(5d, 4d, 3d, 22d, 1d),
    "d16" -> Seq(5d, 4d, 3d, 22d, 1d)
  )
  val m = m0 //map (x=> x._1 -> (x._2 ++ x._2.map(_ -0.09)))
  //  StatTests.pairTable(StatTests.friedmanNemenyi(m, Vector("e1", "e2", "e3", "e4","e1", "e2", "e3", "e4")), "teste", "ALC")
  StatTests.pairTable(StatTests.friedmanNemenyi(m, Vector("e1", "e2", "e3", "e4", "e5")), "teste", "ALC")
  StatTests.winners(m, Vector("e1", "e2", "e3", "e4", "e5"))
  //  StatTests.extensiveTable(m, Vector("e1", "e2", "e3", "e4"), "teste", "ALC")
  m0 foreach (x => println(x._2.mkString(" ")))
}