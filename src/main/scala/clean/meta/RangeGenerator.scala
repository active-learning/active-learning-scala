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
package clean.meta

import clean.{Global, Ds}

trait RangeGenerator {
   def ranges(ds: Ds, n: Int = 10, qmax: Int = 200) = {
      val tmin = ds.nclasses - 1
      val tmax = math.min(ds.expectedPoolSizes(Global.folds).min - 1, qmax - 1)
      val delta = tmax - tmin
      val step = delta / n
      val rs = (tmin until tmax by step).map(x => (x, x + step - 1)) take (n - 1)
      rs ++ Seq(rs.last._2 -> tmax)
   }

   def maxRange(ds: Ds) = {
      val rs = ranges(ds)
      rs.head._1 -> rs.last._2
   }
}

