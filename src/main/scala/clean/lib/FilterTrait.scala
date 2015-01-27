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

import scala.util.Random

trait FilterTrait {
   def filterTr(tr: Seq[Pattern], fold: Int) = {
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

   def filterTs(ts: Seq[Pattern], fold: Int, binaf: Filter, zscof: Filter) = {
      //ts
      val binarizedTs = Datasets.applyFilter(binaf)(ts)
      val testSet = {
         val filteredTs = Datasets.applyFilter(zscof)(binarizedTs)
         new Random(fold).shuffle(filteredTs.sortBy(_.id))
      }
      testSet
   }
}
