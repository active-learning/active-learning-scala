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
import util.Datasets
import weka.filters.Filter
import weka.filters.unsupervised.attribute.{RemoveUseless, Standardize, NominalToBinary, ReplaceMissingValues}

import scala.util.Random

trait FilterTrait {
  def criaFiltro(tr: Seq[Pattern], fold: Int) = {
    //bina
    val binaf = Datasets.binarizeFilter(tr)
    val binarizedTr = Datasets.applyFilter(binaf)(tr)

    //tr
    val zscof = Datasets.zscoreFilter(binarizedTr)
    val pool = {
      val filteredTr = Datasets.applyFilter(zscof)(binarizedTr)
      new Random(fold).shuffle(filteredTr.sortBy(_.id))
    }

    (pool, binaf, zscof)
  }

  def aplicaFiltro(ts: Seq[Pattern], fold: Int, binaf: Filter, zscof: Filter) = {
    val binarizedTs = Datasets.applyFilter(binaf)(ts)
    val testSet = {
      val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
      new Random(fold).shuffle(filteredTs.sortBy(_.id))
    }
    testSet
  }

  def criaFiltroReplaceMissing(tr0: Seq[Pattern], ts0: Seq[Pattern]) = {
    val instances = Datasets.patterns2instances(tr0)
    val instancess = Datasets.patterns2instances(ts0)

    val rmvf = new ReplaceMissingValues
    rmvf.setInputFormat(instances)
    val instancesrmv = Filter.useFilter(instances, rmvf)
    val instancessrmv = Filter.useFilter(instancess, rmvf)

    val bin = new NominalToBinary
    bin.setInputFormat(instancesrmv)
    val instancesbin = Filter.useFilter(instancesrmv, bin)
    val instancessbin = Filter.useFilter(instancessrmv, bin)

    val rmUseless_filter = new RemoveUseless
    rmUseless_filter.setInputFormat(instancesbin)
    val instancesuse = Filter.useFilter(instancesbin, rmUseless_filter)
    val instancessuse = Filter.useFilter(instancessbin, rmUseless_filter)

    //    val std = new Standardize
    //    std.setInputFormat(instancesuse)
    //    std.setIgnoreClass(false)//vai normalizar todos os targets; ok, pois mantém
    //    val instancesstd = Filter.useFilter(instancesuse, std)
    //    val instancessstd = Filter.useFilter(instancessuse, std)

    println(s"${instances} <- instances")
    println(s"${} <- ")
    println(s"${} <- ")
    println(s"${} <- ")
    println(s"${instancessuse} <- instancessuse")

    Datasets.instances2patterns(instancesuse) -> Datasets.instances2patterns(instancessuse)
    //    Datasets.instances2patterns(instancesstd) -> Datasets.instances2patterns(instancessstd)
  }

  def aplicaFiltroReplaceMissing(ts0: Seq[Pattern], fold: Int, rmvf: Filter, binaf: Filter, zscof: Filter) = {
    val ts = Datasets.applyFilter(rmvf)(ts0)
    val binarizedTs = Datasets.applyFilter(binaf)(ts)
    val testSet = {
      val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
      new Random(fold).shuffle(filteredTs.sortBy(_.id))
    }
    testSet
  }
}
