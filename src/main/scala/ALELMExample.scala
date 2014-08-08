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
  val patts0 = new Random(650).shuffle(Datasets.arff(true)("/home/davi/wcs/ucipp/uci/abalone-3class.arff").right.get.take(2000))
  val filter = Datasets.zscoreFilter(patts0)
  val patts = Datasets.applyFilterChangingOrder(patts0, filter)

  val n = patts.length / 2
  val initialN = patts.head.nclasses
  val tr = patts.take(n)
  val ts = patts.drop(n)

  val li = IELM()
  val lis = IELMScratch()
  val lei = EIELM()
  val lie = IELMEnsemble(10)
  val lci = CIELM()
  var mi = li.build(tr.take(initialN))
  var mis = lis.build(tr.take(initialN))
  var mei = lei.build(tr.take(initialN))
  var mie = lie.build(tr.take(initialN))
  var mci = lci.build(tr.take(initialN))
  tr.drop(initialN).foreach { x =>
    mi = li.update(mi)(x)
    mis = lis.update(mis)(x)
    mei = lei.update(mei)(x)
    mie = lie.update(mie)(x)
    mci = lci.update(mci)(x)
    println(s"${mi.accuracy(ts)} ${mis.accuracy(ts)} ${mei.accuracy(ts)} ${mie.accuracy(ts)} ${mci.accuracy(ts)}")
  }
}