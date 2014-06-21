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

trait JSMeasure extends EntropyMeasure {
   def JSdivergence(distributions: Seq[Array[Double]]) = {
      val mean_distribution = distributions.reduce((a, b) => a.zip(b).map(p => p._1 + p._2)) map (_ / distributions.length.toDouble)
      val mean_entropy = distributions.map(entropy).sum / distributions.length.toDouble
      entropy(mean_distribution) - mean_entropy
   }
}
