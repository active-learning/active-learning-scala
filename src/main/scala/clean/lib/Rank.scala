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

package clean.lib

import ml.Pattern
import ml.classifiers.Learner
import org.apache.commons.math3.stat.correlation.SpearmansCorrelation
import util.{Stat, Datasets}

import scala.util.Random

trait Rank extends FilterTrait with RoundFilter {
  def cv10x10fold(bagsFromFile: Vector[Vector[Pattern]], leas: Vector[Pattern] => Vector[Learner]) = (1 to 10).par map { run =>
    val shuffledbagsFromFile = new Random(run).shuffle(bagsFromFile)
    Datasets.kfoldCV2(shuffledbagsFromFile, 10, parallel = true) { (trbags, tsbags, fold, minSize) =>
      val tr = trbags.flatten.toVector
      val ts = tsbags.flatten.toVector
      lazy val (trf, binaf, zscof) = criaFiltro(tr, fold)
      lazy val tsf = aplicaFiltro(ts, fold, binaf, zscof)
      if (leas(tr).isEmpty) {
        //        val learnerSeed = 0
        //        val (fpool, binaf, zscof) = criaFiltro(tr, 1)
        //        val ftestSet = aplicaFiltro(ts, 1, binaf, zscof)
        //        val l = NinteraELM(learnerSeed)
        //        var m = l.batchBuild(fpool).asInstanceOf[ELMModel]
        //        m = l.modelSelectionFull(m)
        //        val rankMedio = media(tr.toSeq map (p => p.nominalSplit))
        //        val twoSpears = ftestSet map { p =>
        //          try {
        //            val spear = new SpearmansCorrelation().correlation(m.output(p), p.nominalSplit)
        //            val spearMaj = new SpearmansCorrelation().correlation(rankMedio, p.nominalSplit)
        //            //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio.zipWithIndex.map(_._2.toDouble), p.nominalSplit)
        //            (spear, spearMaj)
        //          } catch {
        //            case x: Throwable => error("\n " + m.output(p).toList + "\n " + rankMedio.toList + "\n " + p.nominalSplit.toList + " \n" + x)
        //          }
        //        }
        //        val (spearELM, spearMaj) = twoSpears.unzip
        //        Vector(Stat.media_desvioPadrao(spearELM)._1, Stat.media_desvioPadrao(spearMaj)._1)

        val rankMedio = media(tr.toSeq map (p => p.nominalSplit))
        val twoSpears = ts map { p =>
          val rank = Array(0d)
          try {
            val spear = new SpearmansCorrelation().correlation(rank, p.nominalSplit)
            val spearMaj = new SpearmansCorrelation().correlation(rankMedio, p.nominalSplit)
            //                        val spearMaj = new SpearmansCorrelation().correlation(rankMedio.zipWithIndex.map(_._2.toDouble), p.nominalSplit)
            (spear, spearMaj)
          } catch {
            case x: Throwable => sys.error("\n " + rank.toList + "\n " + rankMedio.toList + "\n " + p.nominalSplit.toList + " \n" + x)
          }
        }
        val (spearELM, spearMaj) = twoSpears.unzip
        Vector(Stat.media_desvioPadrao(spearELM)._1, Stat.media_desvioPadrao(spearMaj)._1)
      } else leas(tr) flatMap { le =>
        val (trtestbags, tstestbags, m) = if (le.querFiltro) (trf.groupBy(x => x.vector), tsf.groupBy(x => x.vector), le.build(trf))
        else (tr.groupBy(x => x.vector), ts.groupBy(x => x.vector), le.build(tr))
        Seq(trtestbags, tstestbags) map (bags => (bags.map(_._2) map { tsbag =>
          //m.accuracy(tsbag)
          tsbag.map(_.label).contains(m.predict(tsbag.head))
          //}).sum / bags.size.toDouble)
        }).count(_ == true) / bags.size.toDouble)
      }
    }
  }

  /**
   * apenas troca valor de acuracia por posicionamento
   * @param s
   * @return
   */
   def ranqueia(s: Seq[Double]) = s.zipWithIndex.sortBy(_._1).reverse.zipWithIndex.groupBy {
      case ((v, idx), ra) => ff(1000)(v)
   }.toList.map { case (k, g) =>
      val gsize = g.size
      val avrRa = g.map { case ((v, idx), ra) => ra}.sum.toDouble / gsize + 1 // +1 pra corrigir o índice zero
      g.map { case ((v, idx), ra) => idx -> avrRa}
   }.flatten.sortBy(_._1).map(_._2)

   def media(ss: Seq[Array[Double]]) = {
      val cs = ss.head.size
      val n = ss.size
      ss.foldLeft(Array.fill(cs)(0d))((b, a) => b.zip(a).map { case (x, y) => x + y}).map(_ / n)
   }

   def res0ToPlot0(res0: List[Seq[Seq[Double]]],tipoSumariz:String) = {
      val cols = res0.head.head.size
      val qs = res0.head.size
      tipoSumariz match {
         case "media" =>
            res0.foldLeft(Seq.fill(cols * qs)(0d)) { (l, m) =>
               m.flatten.zip(l).map(x => x._1 + x._2)
            }.grouped(cols).toList.map(_.toList)
         case "mediana" => res0.map(_.flatten).transpose.map(x => x.sorted.toList(x.size / 2)).grouped(cols).toList.map(_.toList)
      }
   }

  /**
   * pega n melhores e todas as que empatarem com a n melhor
   */
  def pegaMelhores[T](s: Seq[T], n: Int)(f: T => Double) = {
    var t = 0
    val (bef, aft) = s.groupBy(f).toList.sortBy(_._1).reverse.span { case (k, vs) =>
      t += vs.size
      t < n
    }
    (bef :+ aft.head).map(_._2).flatten
  }

  /**
   * dispensa n melhores e todas as que empatarem com a n melhor
   */
  def dispensaMelhores[T](s: Seq[T], n: Int)(f: T => Double) = {
    var t = 0
    s.groupBy(f).toList.sortBy(_._1).reverse.dropWhile { case (k, vs) =>
      t += vs.size
      t < n
    }.tail.map(_._2).flatten
  }
}
