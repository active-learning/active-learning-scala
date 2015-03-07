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

package al.strategies

import ml.Pattern
import ml.classifiers.RF
import weka.core.Utils

case class QBC(pool: Seq[Pattern], debug: Boolean = false)
   extends StrategyAgnostic {
   override val toString = "QBC"
   val abr = "QBC"
   val id = 3292212
   //acrescentei 1 na frente porque a estrat estava erradamente como filterdependent; troquei por 3 pq o JS tava equivalente a Entropy
   val learner = RF(seed)

   def next(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
      val current_model = RF(seed, 10, 0, math.max(Utils.log2(labeled.size).toInt - 1, 1)).build(labeled)
      val selected = unlabeled maxBy current_model.JS
      selected
   }

   protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = ???
}

