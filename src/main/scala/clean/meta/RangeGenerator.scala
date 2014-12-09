package clean.meta

import clean.{Global, Ds}

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
trait RangeGenerator {
   def ranges(ds: Ds) = {
      val min = ds.nclasses - 1
      val max = math.min(ds.expectedPoolSizes(Global.folds).min, 200)
      val delta = max - min
      val step = delta / 10
      (min until max by step).map(x => (x, x + step)) take 9
   }

   def maxRange(ds: Ds) = {
      val rs = ranges(ds)
      rs.head._1 -> rs.last._2
   }
}

