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

package clean.tex

import java.io.PrintWriter

import al.strategies._
import clean.lib._
import ml.classifiers._
import util.{Stat, StatTests}

object fried_tabela_datasets_Pares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator {
  lazy val arguments = superArguments
  val context = "friedParestex"
  val measure = ALCKappa
  val risco = false
  run()

  override def run() = {
    super.run()
    val caption = language match {
      case "pt" => s"Pares um contra um (STR). Medida: $measure. \\textit{Legenda na Tabela \\ref{tab:friedClassif}.}"
      case "en" => s"Pairwise comparison: each asterisk/cross/dot indicates that the algorithm at the row has better $measure than the strategy at the column within a confidence interval of 0.99/0.95/0.90."
    }
    val fakePool = Seq()
    val strats = Seq(RF() -> MarginFixo(RF(), fakePool),
      SVMLibRBF() -> ExpErrorReductionMarginFixo(SVMLibRBF(), fakePool, "entropy"),
      C45() -> ExpErrorReductionMarginFixo(C45(), fakePool, "entropy"),
      NBBatch() -> ExpErrorReductionMarginFixo(NBBatch(), fakePool, "entropy"),
      KNNBatcha(5, "eucl", fakePool, true) -> AgDensityWeightedTrainingUtility(fakePool, "eucl"),
      RF() -> AgDensityWeightedTrainingUtility(fakePool, "maha"),
      SVMLibRBF() -> RandomSampling(fakePool),
      C45() -> RandomSampling(fakePool),
      NBBatch() -> RandomSampling(fakePool),
      KNNBatcha(5, "eucl", fakePool, true) -> ExpErrorReductionMarginFixo(KNNBatcha(5, "eucl", fakePool, true), fakePool, "balacc"))
    //      val strats = (for {l <- ls; s <- stratsTexRedux("all").map(_(l))} yield s).distinct

    val (sls, res0) = (for {
      dataset <- datasets.toList.filter { dataset =>
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        val r = ds.poolSize >= 200
        ds.close()
        r
      }
    } yield {
        val ds = Ds(dataset, readOnly = true)
        ds.open()
        val (ti, th, tf, tpass) = ranges(ds)
        val (sts, sres) = (for {
          (lear, stra) <- strats
        } yield {
            val vs = (for {
              r <- 0 until runs
              f <- 0 until folds
            } yield {
                val classif = lear
                classif.limpa -> measure(ds, stra, classif, r, f)(ti, tf).read(ds).getOrElse {
                  println((ds, stra, stra.learner, classif, r, f) + ": medida não encontrada")
                  sys.exit(0) //NA
                }
              }) map (_._2)
            stra.abrev -> (if (!risco) Stat.media_desvioPadrao(vs.toVector) else Stat.media_desvioPadrao(vs.toVector)._2 -> NA)
          }).unzip
        ds.close()
        sts -> (renomeia(ds.dataset) -> sres)
      }).unzip
    val sl = sls.head

    val sortedFiltered = res0.filter(!_._2.contains(NA, NA)).sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
    val sorted = res0.toList.sortBy(_._1).zipWithIndex.map(x => ((x._2 + 1).toString + "-" + x._1._1) -> x._1._2)
    sorted foreach (x => println(x._2.map(_._1).mkString(" ")))

    println(s"\t${sortedFiltered.size} datasets completos")
    val pairs = if (!risco) StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(_._1)), sl.toVector)
    else StatTests.friedmanNemenyi(sortedFiltered.map(x => x._1 -> x._2.map(1 - _._2).drop(1)), sl.toVector.drop(1))
    val arq2 = s"/home/davi/wcs/artigos/revista-comparacao/tempstratsfriedPares$measure" + (if (risco) "Risco" else "") + ".tex"
    println(s"")
    println(arq2)
    val fw2 = new PrintWriter(arq2, "ISO-8859-1")
    fw2.write(StatTests.pairTable(pairs, "stratsfriedpares" + (if (risco) "Risco" else ""), 2, caption))
    fw2.close()
  }
}
