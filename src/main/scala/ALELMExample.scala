import al.strategies.{DensityWeightedTrainingUtility, ClusterBased}
import ml.classifiers._
import util.Datasets

import scala.util.Random

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
object ALELMExample extends App {
  //  val patts0 = new Random(0).shuffle(Datasets.patternsFromSQLite("/home/davi/wcs/ucipp/uci")("gas-drift").right.get.take(1000000))
  //  val patts0 = new Random(0).shuffle(Datasets.arff(true)("/home/davi/wcs/ucipp/uci/iris.arff").right.get.take(200000))
  val patts0 = new Random(650).shuffle(Datasets.arff("/home/davi/wcs/ucipp/uci/abalone-11class.arff").right.get.take(2000))
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilter(filter)(patts0)

  val n = patts.length / 2
  val initialN = patts.head.nclasses
  // ClusterBased(patts.take(n))
  val tri = DensityWeightedTrainingUtility(IELM(), patts.take(n), "eucl").queries
  val tris = DensityWeightedTrainingUtility(IELMScratch(), patts.take(n), "eucl").queries
  val trei = DensityWeightedTrainingUtility(EIELM(), patts.take(n), "eucl").queries
  val trie = DensityWeightedTrainingUtility(IELMEnsemble(10), patts.take(n), "eucl").queries
  val trci = DensityWeightedTrainingUtility(CIELM(), patts.take(n), "eucl").queries
  val treci = DensityWeightedTrainingUtility(ECIELM(), patts.take(n), "eucl").queries
  val ts = patts.drop(n)

  val li = IELM()
  val lis = IELMScratch()
  val lei = EIELM()
  val lie = IELMEnsemble(10)
  val lci = CIELM()
  val leci = ECIELM()

  var mi = li.build(tri.take(initialN))
  var mis = lis.build(tris.take(initialN))
  var mei = lei.build(trei.take(initialN))
  var mie = lie.build(trie.take(initialN))
  var mci = lci.build(trci.take(initialN))
  var meci = leci.build(treci.take(initialN))

  val res = tri.zip(tris).zip(trei).zip(trie).zip(trci).zip(treci).drop(initialN).map { case (((((xi, xei), xis), xie), xci), xeci) =>
    mi = li.update(mi)(xi)
    mis = lis.update(mis)(xis)
    mei = lei.update(mei)(xie)
    mie = lie.update(mie)(xie)
    mci = lci.update(mci)(xci)
    meci = leci.update(meci)(xeci)
    (mi.accuracy(ts), mis.accuracy(ts), mei.accuracy(ts), mie.accuracy(ts), mci.accuracy(ts), meci.accuracy(ts))
  }
  res foreach (x => println(s"${x._1} ${x._2} ${x._3} ${x._4} ${x._5} ${x._6}"))
}