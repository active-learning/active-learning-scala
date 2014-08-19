package util

/*
elm-scala: an implementation of ELM in Scala using MTJ
Copyright (C) 2014 Davi Pereira dos Santos

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
object FriedmanNemenyi {
  /**
   * Takes a map DatasetName -> strategyMeasuresTheHigherTheBetter
   * and returns a map strategyName -> Vector(wins strat 1?, wins strat 2?, ...)
   * 0: no win
   * 1: 0.9 confidence on win
   * 2: 0.95 confidence on win
   * @param measures
   */
  def table(measures: Map[String, Array[Double]], strategies: Vector[String]) = {
    strategies.zip(FriedmanTest.Friedman(measures.values.toArray, true)).map(x => x._1 -> x._2.toVector).toMap
  }

  /*
   prints a Latex table with 0.95 pairwise comparisons.
    */
  def latex(pairs: Map[String, Vector[Int]], tableName: String, seps: Int = 2): Unit = {
    println( """\begin{table}[h]
\caption{Um contra um: cada asterisco indica quando a estratégia na linha é melhor que a estratégia na coluna com intervalo de confiança de 0.95.}
\begin{center}
\begin{tabular}{l""" + Seq.fill(pairs.size)("c").grouped(seps).map(_.mkString).mkString("|") + "}\n & " + (1 to pairs.size).mkString(" & ") + """\\""")

    pairs.zipWithIndex.foreach { case ((s, l), i) =>
      val nr = i + 1
      print(s"$nr - $s & " + l.map(x => if (x == 2) "*" else " ").mkString(" & ") + """ \\""")
      if (i % seps == seps - 1) println( """ \hline""") else println("")
    }

    println( """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}""")
  }
}

object FriedmanNemenyiTest extends App {
  val m = Map(
    "d1" -> Array(0.91, 0.02, 0.11, 0.13),
    "d2" -> Array(0.49, 0.019, 0.11, 0.12),
    "d3" -> Array(0.48, 0.002, 0.01, 0.13),
    "d4" -> Array(0.45, 0.002, 0.02, 0.13),
    "d5" -> Array(0.46, 0.003, 0.09, 0.13),
    "d6" -> Array(0.43, 0.002, 0.09, 0.13),
    "d7" -> Array(0.43, 0.001, 0.08, 0.13),
    "d8" -> Array(0.44, 0.002, 0.08, 0.13),
    "d9" -> Array(0.48, 0.004, 0.05, 0.13),
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
  FriedmanNemenyi.latex(FriedmanNemenyi.table(m, Vector("e1", "e2", "e3", "e4")), "teste")
}