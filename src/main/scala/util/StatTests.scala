package util

import scala.collection.immutable.ListMap

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
  /**
   * Takes a map DatasetName -> strategyMeasuresTheHigherTheBetter
   * and returns a map strategyName -> Vector(wins strat 1?, wins strat 2?, ...)
   * 0: no win
   * 1: 0.9 confidence on win
   * 2: 0.95 confidence on win
   * @param measures
   */
  def friedmanNemenyi(measures: List[(String, Array[Double])], strategies: Vector[String]) = strategies.zip(FriedmanTest.Friedman(measures.map(_._2).toArray, true)).map(x => x._1 -> x._2.toVector)

  /**
   * prints a Latex table with all data,
   * rows: datasets
   * columns: strategies
   */
  def extensiveTable(measures: List[(String, Array[Double])], strategies: Vector[String], tableName: String, measure: String, seps: Int = 4, language: String = "pt") {
    val nstrats = measures.head._2.length
    val core = measures.zipWithIndex.map { case ((d, l), i) =>
      val vals = l.map(x => if (x == l.max) s"\\textbf{$x}" else x).mkString(" & ")
      s"$d & $vals \\\\" + (if (i % seps == seps - 1) """ \hline""" else "")
    }.mkString("\n")
    table(core, nstrats, strategies, tableName, measure, language)
  }

  private def table(core: String, nstrats: Int, strategies: Vector[String], tableName: String, measure: String, language: String = "pt") {
    if (nstrats != strategies.size) {
      println(s"Inconsistency: $nstrats != ${strategies.size}")
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

  def extensiveTable2(measures: ListMap[String, Array[(Double, Double)]], strategies: Vector[String], tableName: String, measure: String, seps: Int = 4, language: String = "pt"): Unit = {
    val nstrats = measures.head._2.length
    val core = measures.zipWithIndex.map { case ((d, l), i) =>
      val vals = l.map { case (x1, x2) =>
        if (x1 == l.map(_._1).max) s"\\textbf{$x1/$x2}" else s"$x1/$x2"
      }.mkString(" & ")
      s"$d & $vals \\\\" + (if (i % seps == seps - 1) """ \hline""" else "")
    }.mkString("\n")
    table(core, nstrats, strategies, tableName, measure, language)
  }

  def winsLossesTies(measures: ListMap[String, Array[Double]], strategies: Vector[String]) = ???

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
  val m = List(
    "d1" -> Array(0.91, 0.02, 0.11, 0.3),
    "d2" -> Array(0.49, 0.01, 0.11, 0.12),
    "d3" -> Array(0.48, 0.0, 0.01, 0.13),
    "d4" -> Array(0.45, 0.0, 0.02, 0.13),
    "d5" -> Array(0.46, 0.0, 0.09, 0.13),
    "d6" -> Array(0.43, 0.0, 0.09, 0.13),
    "d7" -> Array(0.43, 0.0, 0.08, 0.13),
    "d8" -> Array(0.44, 0.0, 0.08, 0.13),
    "d9" -> Array(0.48, 0.0, 0.05, 0.13),
    "d10" -> Array(0.47, 0.002, 0.16, 0.192),
    "d11" -> Array(0.417, 0.0012, 0.116, 0.117),
    "d12" -> Array(0.427, 0.0022, 0.126, 0.127),
    "d13" -> Array(0.437, 0.0102, 0.136, 0.138),
    "d14" -> Array(0.437, 0.0102, 0.136, 0.138),
    "d15" -> Array(0.437, 0.0102, 0.136, 0.138),
    "d16" -> Array(0.437, 0.0102, 0.136, 0.138),
    "d17" -> Array(0.437, 0.0102, 0.136, 0.138),
    "d18" -> Array(0.437, 0.0102, 0.136, 0.138),
    "d19" -> Array(0.447, 0.0040, 0.146, 0.149),
    "d20" -> Array(0.447, 0.0040, 0.146, 0.149)
  )
  StatTests.pairTable(StatTests.friedmanNemenyi(m, Vector("e1", "e2", "e3", "e4")), "teste", "ALC")
  //  StatTests.extensiveTable(m, Vector("e1", "e2", "e3", "e4"), "teste", "ALC")
}