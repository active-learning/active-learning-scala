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
import util.Graphics
import Graphics.Plot

trait StrategyAgnostic extends Strategy {
   protected def resume_queries_impl(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = queries_rec(unlabeled, labeled)

   private def queries_rec(unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Stream[Pattern] = {
      if (unlabeled.isEmpty) Stream.Empty
      else {
         if (debug) visual_test(null, unlabeled, labeled)
         val selected = next(unlabeled, labeled)
         if (debug) visual_test(selected, unlabeled, labeled)
         selected #:: queries_rec(unlabeled.diff(Seq(selected)), labeled :+ selected)
      }
   }

   protected def next(unlabeled: Seq[Pattern], labeled: Seq[Pattern]): Pattern
}