package util

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
   def f(x: Double) = "%4.3f".format(x)

   def ff(precision: Double)(x: Double) = (x * precision).round / precision

   /**
    * Takes a map DatasetName -> strategyMeasuresTheHigherTheBetter
    * and returns a map strategyName -> Vector(wins strat 1?, wins strat 2?, ...)
    * 0: no win
    * 1: 0.95 confidence on win
    * 2: 0.99 confidence on win
    * @param measures
    */
   def friedmanNemenyi(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = strategies.zip(FriedmanTest.Friedman(measures.map(_._2.toArray).toArray, true)).map(x => x._1 -> x._2.toVector)

   /**
    * Winners per row (dataset or pool).
    */
   def winners(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
      val ranked = FriedmanTest.CD(measures.map(_._2.toArray).toArray, true)
      strategies.zipWithIndex.filter { case (s, i) =>
         ranked.contains(i)
      }.map(_._1)
   }

   /**
    * Losers per row (dataset or pool).
    */
   def losers(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
      val ranked = FriedmanTest.CD(measures.map(_._2.map(-_).toArray).toArray, true)
      strategies.zipWithIndex.filter { case (s, i) =>
         ranked.contains(i)
      }.map(_._1)
   }

   def winsLossesTies(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = ???

   private def table(core: String, nstrats: Int, strategies: Vector[String], tableName: String, measure: String, language: String = "pt") {
      if (nstrats != strategies.size) {
         println(s"Inconsistency #measures-in-the-first-row != strategies.size: $nstrats != ${strategies.size}: $strategies")
         sys.exit(1)
      }
      val caption = language match {
         case "pt" => s"$measure para os Q exemplos consultados. Os maiores valores de medida e desvio padrão de cada base está em \\textcolor{blue}{\\textbf{negrito azul}} e \\textcolor{red}{\\textbf{negrito vermelho}}." +
            s"Valores isolados estão sublinhados. Melhores valores de desvio padrão estão em \\textcolor{darkgreen}{verde}. Apenas negrito indica segundo melhor valor."
         case "en" => s"$measure for the first Q queried instances. Highest value(and std. deviation) for each dataset is in \\textcolor{blue}{\\textbf{blue bold}}(\\textcolor{red}{\\textbf{red bold}}) face. When unique, values are underlined. Best (lowest) std. values are in \\textcolor{darkgreen}{green}."
      }
      println( """\definecolor{darkgreen}{rgb}{0.0, 0.4, 0.0}
\begin{table}[h]
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

   def cor(ns0: Seq[Double], precision: Double) = {
      val ns = ns0 map ff(precision)
      val ns1 = ns.filter(_ > Double.MinValue).sorted.reverse
      val Mx = ns1.head
      val Snd = ns1.tail.head
      val Mn = ns1.last
      val Inf = ff(precision)(Double.MinValue)
      ns.zip(ns.map(x => f(x))).map {
         case (Inf, _) => ""
         case (Mn, s) if Mn == Mx => s"$s"
         case (Mn, s) => s"\\textcolor{red}{\\textbf{$s}}"
         case (Snd, s) => s"\\textbf{$s}"
         case (Mx, s) if Snd < Mx => s"\\underline{\\textcolor{blue}{\\textbf{$s}}}"
         case (Mx, s) => s"\\textcolor{blue}{\\textbf{$s}}"
         case (_, s) => s"$s"
      }
   }

   def extensiveTable2(precision: Double, measures: Seq[(String, Seq[(Double, Double)])], strategies: Vector[String], tableName: String, measure: String, seps: Int = 4, language: String = "pt"): Unit = {
      val nstrats = measures.head._2.length
      val core = measures.zipWithIndex.map { case ((d, l), i) =>
         val (vs, ds) = l.unzip
         val r = cor(vs, precision).zip(ds.map { case Double.MinValue => ""; case x => f(x)}) map { case ("", "") => ""; case x => x._1 + "/" + x._2}
         val vals = r.mkString(" & ")
         s"$d & $vals \\\\" + (if (i % seps == seps - 1) """ \hline""" else "")
      }.filter(_.nonEmpty).mkString("\n")
      if (core.nonEmpty) table(core, nstrats, strategies, tableName, measure, language)
   }

   /**
    * prints a Latex table for pairwise comparisons
    */
   def pairTable(pairs: Vector[(String, Vector[Int])], tableName: String, measure: String, seps: Int = 2, language: String = "pt") {
      val caption = language match {
         case "pt" => s"Um contra um: cada asterisco/cruz indica quando a estratégia na linha tem melhor $measure que a estratégia na coluna com intervalo de confiança de 0.99/0.95."
         case "en" => s"Pairwise comparison: each asterisk/cross indicates that the strategy at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95."
      }
      println( """\begin{table}[h]
\caption{""" + caption + """}
\begin{center}
\begin{tabular}{l""" + Seq.fill(pairs.size)("c").grouped(seps).map(_.mkString).mkString("|") + "}\n \t\t& " + (1 to pairs.size).mkString(" & ") + """ \\""")

      pairs.zipWithIndex.foreach { case ((s, l), i) =>
         val nr = i + 1
         print(s"$nr - $s\t& " + (l map {
            case 3 => "*"
            case 2 => "+"
            case 1 => "."
            case 0 => " "
         }).mkString(" & ") + """ \\""")
         if (i % seps == seps - 1) println( """ \hline""") else println("")
      }

      println( """\end{tabular}
\label{""" + tableName + """}
\end{center}
\end{table}""")
   }

   /**
    * Winners per row (dataset or pool).
    */
   def clearWinners(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
      ???
      val ranked = FriedmanTest.CD(measures.map(_._2.toArray).toArray, true)
      strategies.zipWithIndex.filter { case (s, i) =>
         ranked.contains(i)
      }.map(_._1)
   }
}

object FriedmanNemenyiTest extends App {
   val m0 = Seq(
      "d1" -> Seq(5d, 4d, 3d, 2d, 0d),
      "d2" -> Seq(5d, 4d, 3d, 2d, 0d),
      "d3" -> Seq(5d, 4d, 3d, 2d, 0d),
      "d4" -> Seq(5d, 4d, 3d, 22d, 10d),
      "d5" -> Seq(5d, 4d, 3d, 22d, 10d),
      "d6" -> Seq(5d, 4d, 3d, 22d, 10d),
      "d7" -> Seq(5d, 4d, 0d, 22d, 5d),
      "d8" -> Seq(5d, 4d, 0d, 22d, 0d),
      "d9" -> Seq(500d, 4000d, 30d, 200000d, 200001d),
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
   //  StatTests.pairTable(StatTests.friedmanNemenyi(m, Vector("e1", "e2", "e3", "e4", "e5")), "teste", "ALC")
   println("winners:" + StatTests.winners(m, Vector("e1", "e2", "e3", "e4", "e5")))
   println("losers:" + StatTests.losers(m, Vector("e1", "e2", "e3", "e4", "e5")))
   StatTests.pairTable(StatTests.friedmanNemenyi(m, Vector("e1", "e2", "e3", "e4", "e5")), "teste", "ALC")
   //  StatTests.extensiveTable(m, Vector("e1", "e2", "e3", "e4"), "teste", "ALC")
   //  m0 foreach (x => println(x._2.mkString(" ")))
}