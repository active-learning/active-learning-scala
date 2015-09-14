package util

import clean.lib.RoundFilter
import clean.tex.tabwinners

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
object StatTests extends RoundFilter {
   def f3(x: Double) = "%6.3f".format(x)

   def f2(x: Double) = "%6.2f".format(x)

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
    * A ordem retornada é a mesma original.
    */

   import scala.collection.JavaConversions._

   def winners(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
      val (lhm, cd) = FriedmanTest.CD(measures.map(_._2.toArray).toArray, true)
      val m = lhm.toList.sortBy(_._1).map(_._2._1)
      val ranked = m.dropRight(1)
      strategies.zipWithIndex.filter { case (s, i) =>
         ranked.contains(i)
      }.map(_._1)
   }

   /**
    * Losers per row (dataset or pool).
    * A ordem retornada é a mesma original.
    */
   def losers(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
      val (lhm, cd) = FriedmanTest.CD(measures.map(_._2.toArray).toArray, false)
      val m = lhm.toList.sortBy(_._1).map(_._2._1)
      val ranked = m.dropRight(1)
      strategies.zipWithIndex.filter { case (s, i) =>
         ranked.contains(i)
      }.map(_._1)
   }

   def winsLossesTies(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = ???

   private def table(capti: String, core: String, nstrats: Int, strategies: Vector[String], tableName: String, measure: String, language: String = "pt") = {
      val contemStdDev = if (core.contains("/")) " Os menores valores de desvio padrão estão em \\textcolor{darkgreen}{verde}." else ""
      if (nstrats != strategies.size) {
         println(s"Inconsistency #measures-in-the-first-row != strategies.size: $nstrats != ${strategies.size}: $strategies")
         sys.exit(1)
      }
      val caption = language match {
         case "pt" =>
            if (capti != "") capti
            else s"$measure: " + (if (contemStdDev.nonEmpty) "Os maiores valores da média e desvio padrão" else "O maior valor") + " de cada base está em \\textcolor{blue}{\\textbf{negrito azul}} e \\textcolor{red}{\\textbf{negrito vermelho}} respectivamente." +
               s" Valores isolados estão sublinhados.$contemStdDev Apenas negrito indica segundo melhor valor."
         case "en" => s"$measure: Highest average (std. deviation) for each dataset is in \\textcolor{blue}{\\textbf{blue bold}}(\\textcolor{red}{\\textbf{red bold}}) face. When unique, values are underlined." +
            s" Lowest std. values are in \\textcolor{darkgreen}{green}. Bold face only number shows second best value."
      }
      """\definecolor{darkgreen}{rgb}{0.0, 0.4, 0.0}
\begin{table}[h]
\caption{""" + caption + """}
\begin{center}""" + (if (strategies.size > 8) "\\scalebox{0.75}{" else "") +
         """\begin{tabular}{l""" + Seq.fill(nstrats)("c").mkString("|") + "}\n & " + strategies.mkString(" & ") + "\\\\\n" + """ \hline """ + "\n" +
         core +
         """\end{tabular}""" + "\n" + (if (strategies.size > 8) "}" else "") +
         """\label{""" + tableName + """}
\end{center}
\end{table}
                                     """
   }

   def cor(ns0: Seq[Double], precision: Double, alto: String, baixo: String, baixo2: String = "") = {
      val ns = ns0 map ff(precision)
      val ns1 = ns.filter(_ > Double.MinValue).sorted.reverse
      val Mx = ns1.head
      val Snd = ns1.tail.head
      val Sndmin = ns1.reverse.tail.head
      val Mn = ns1.last
      val Inf = ff(precision)(Double.MinValue)
      ns.zip(ns.map(x => f2(x))).map {
         case (Inf, _) => ""
         case (Mn, s) if Mn == Mx => s"$s"
         case (Sndmin, s) if baixo2.nonEmpty => s"\\textcolor{$baixo2}{\\textbf{$s}}"
         case (Mn, s) => s"\\textcolor{$baixo}{\\textbf{$s}}"
         case (Mx, s) if Snd == Mx && baixo2.isEmpty => s"\\textcolor{$alto}{\\textbf{$s}}"
         case (Snd, s) if baixo2.isEmpty => s"\\textcolor{black}{\\textbf{$s}}"
         case (Mx, s) if Snd < Mx && baixo2.isEmpty => s"\\underline{\\textcolor{$alto}{\\textbf{$s}}}"
         case (_, s) => s"$s"
      }
   }

   def extensiveTable2(capti: String, take11: Boolean, precision: Double, measures: Seq[(String, Seq[(Double, Double)])], strategies: Vector[String], tableName: String, measure: String, seps: Int = 4, language: String = "pt") = {
     val nnn=7
     val nstrats = if (take11) measures.head._2.take(nnn).length else measures.head._2.drop(nnn).length
      val core = measures.zipWithIndex.map { case ((d, l), i) =>
         val (vs, ds) = l.unzip
         val r = cor(vs, precision, "blue", "red").zip(cor(ds, precision, "black", "darkgreen", "black")) map {
            case ("", "") => ""
            case (x, y) if y.contains("-9") => x
            case (x, y) => x + "/" + y
         }
         val vals = (if (take11) r.take(nnn) else r.drop(nnn)).mkString(" & ")
         s"$d & $vals \\\\" + (if (i % seps == seps - 1) """ \hline""" else "")
      }.filter(_.nonEmpty).mkString("\n")
      if (core.nonEmpty) table(capti, core, nstrats, if (take11) strategies.take(nnn) else strategies.drop(nnn), tableName, measure, language) else ""
   }

   /**
    * prints a Latex table for pairwise comparisons
    */
   def pairTable(pairs: Vector[(String, Vector[Int])], tableName: String, seps: Int, caption: String = "CAPTION") = {
      """\begin{table}[h]
\caption{""" + caption + """}
\begin{center}""" +
         (if (pairs.size > 10 && pairs.size < 20) "\\scalebox{0.9}{" else "") +
         """\begin{tabular}{l""" + Seq.fill(pairs.size)("c").grouped(seps).map(_.mkString).mkString("|") + "}\n \t\t\t& " + (1 to pairs.size).mkString(" & ") + "\\\\\n" +
         pairs.zipWithIndex.map { case ((s, l), i) =>
            val I = i
            val nr = i + 1
            s"$nr - ${s.padTo(5, " ").mkString}\t& " + (l.zipWithIndex map {
               case (3, _) => "*"
               case (2, _) => "+"
               case (1, _) => "."
               case (0, I) => "-"
               case (0, _) => " "
            }).mkString(" & ") + """ \\""" +
               (if (i % seps == seps - 1) """ \hline""" else "")
         }.mkString("\n") + "\n" +
         """\end{tabular}""" + "\n" +
         (if (pairs.size > 10 && pairs.size < 20) "}" else "") +
         """
\label{""" + tableName + """}
\end{center}
\end{table}"""
   }

   /**
    * Winners per row (dataset or pool), eliminando quem estiver empatado com um não-vencedor.
    * A ordem retornada é a mesma original.
    */
   def clearWinners(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) =
      if (measures.head._2.size != strategies.size) {
         println(s"Erro: ${measures.head._2.size} != ${strategies.size}")
         sys.exit(1)
      } else {
         import scala.collection.JavaConversions._
         val (linkedHM, cd) = FriedmanTest.CD(measures.map(_._2.toArray).toArray, true)
         val m = linkedHM.toList.sortBy(_._1).map(_._2)
         if (m.last ==(-1, -1d)) {
            println(s"Todos empataram, logo, todos venceram.")
            strategies
         } else {
            val limit = m.last._2 - cd
            val ranked = m.takeWhile(_._2 < limit).map(_._1)
            strategies.zipWithIndex.filter { case (s, i) =>
               ranked.contains(i)
            }.map(_._1)
         }
      }

   /**
    * Loosers per row (dataset or pool), eliminando quem estiver empatado com um não-perdedor.
    * A ordem retornada é a mesma original.
    */
   def clearLosers(measures: Seq[(String, Seq[Double])], strategies: Vector[String]) = {
      import scala.collection.JavaConversions._
      val (lhm, cd) = FriedmanTest.CD(measures.map(_._2.toArray).toArray, false)
      val m = lhm.toList.sortBy(_._1).map(_._2)
      val limit = m.last._2 - cd
      val ranked = m.takeWhile(_._2 < limit).map(_._1)
      strategies.zipWithIndex.filter { case (s, i) =>
         ranked.contains(i)
      }.map(_._1)
   }

   /**
    * prints a Latex table for pairwise numerical comparisons
    */
   def distTable(pairs: List[(String, List[Double])], tableName: String, sujeitos: String, measure: String, seps: Int = 2, language: String = "pt") = {
      val caption = language match {
         case "pt" => s"Similaridade entre $sujeitos de acordo com a medida measure para as 94 bases de dados." +
            " O maior e o menor valor de cada linha está em \\textcolor{blue}{\\textbf{azul}} e \\textcolor{red}{\\textbf{vermelho}} respectivamente." +
            (if (pairs.size > 7) " Valores iguais ou acima de $0,50$ estão em negrito." else "")
         case "en" => s"escrever no scala a descricao em ingles!!."
      }
      val header = if (pairs.size > 10) pairs.map(x => "\\begin{sideways}" + x._1 + "\\end{sideways}") else pairs.map(_._1)
      """\begin{table}[h]
\caption{""" + caption + """}
\begin{center}""" + (if (header.head.contains("sideways")) if (pairs.size > 12) """\scalebox{0.8}{""" else """\scalebox{0.9}{""" else "") +
         """\begin{tabular}{l""" + Seq.fill(pairs.size)("c").grouped(seps).map(_.mkString).mkString("|") + "}\n \t\t\t\t& " + header.mkString(" & ") + """ \\""" + "\n" +
         pairs.zipWithIndex.map { case ((s, l), i) =>
            val Mx = l.filter(_ != 1).max
            val Mn = l.min
            s"$s\t& " + l.zipWithIndex.map {
               case (_, idx) if idx == i => "-"
               case (Mx, _) => "\\textcolor{blue}{\\textbf{" + f2(Mx) + "}}"
               case (Mn, _) => "\\textcolor{red}{\\textbf{" + f2(Mn) + "}}"
               case (ll, _) if ll >= 0.5 => "\\textbf{" + f2(ll) + "}"
               case (ll, _) => f2(ll)
            }.mkString(" & ") + """ \\""" + (if (i % seps == seps - 1) " \\hline" else "")
         }.mkString("\n") +
         """\end{tabular}""" + "\n" + (if (header.head.contains("sideways")) "}" else "") +
         """\label{""" + tableName + """}
\end{center}
\end{table}
                                     """
   }
}

object FriedmanNemenyiTest extends App {
   val m0 = Seq(
      "d1" -> Seq(5d, 4d, 30d, 2d, 0d),
      "d2" -> Seq(5d, 4d, 30d, 2d, 0d),
      "d3" -> Seq(5d, 4d, 30d, 2d, 0d),
      "d4" -> Seq(5d, 4d, 30d, 22d, 10d),
      "d5" -> Seq(5d, 4d, 30d, 22d, 10d),
      "d6" -> Seq(5d, 4d, 30d, 22d, 10d),
      "d7" -> Seq(5d, 4d, 30d, 22d, 5d),
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
   println(StatTests.pairTable(StatTests.friedmanNemenyi(m, Vector("e1", "e2", "e3", "e4", "e5")), "teste", 2, "sdfsdf"))
   //  StatTests.extensiveTable(m, Vector("e1", "e2", "e3", "e4"), "teste", "ALC")
   //  m0 foreach (x => println(x._2.mkString(" ")))
   println("clearwinners:" + StatTests.clearWinners(m, Vector("e1", "e2", "e3", "e4", "e5")))
   println("clearlosers:" + StatTests.clearLosers(m, Vector("e1", "e2", "e3", "e4", "e5")))
}