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

trait Rank {
   def fff(precision: Double)(x: Double) = (x * precision).round / precision

   def ranqueia(s: Seq[Double]) = s.zipWithIndex.sortBy(_._1).reverse.zipWithIndex.groupBy {
      case ((v, idx), ra) => fff(1000)(v)
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
}