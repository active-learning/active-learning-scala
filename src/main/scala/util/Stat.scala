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

package util

object Stat {
   def media_desvioPadrao(items: Vector[Double]) = {
      val s = items.sum
      val l = items.length.toDouble
      val m = s / l
      val v0 = (items map {
         x =>
            val di = x - m
            di * di
      }).sum / (l - 1)
      val v = if (v0.isNaN) 0 else v0
      val d = math.sqrt(v)
      (m, d)
   }

   def media_desvioPadraoi(items: Vector[Int]) = {
      val s = items.sum
      val l = items.length.toDouble
      val m = s / l
      val v0 = (items map {
         x =>
            val di = x - m
            di * di
      }).sum / (l - 1)
      val v = if (v0.isNaN) 0 else v0
      val d = math.sqrt(v)
      (m, d)
   }

   def media_intervalo_confianca90(items: Vector[Double]) = {
      val s = items.sum
      val l = items.length.toDouble
      val m = s / l
      val v0 = (items map {
         x =>
            val di = x - m
            di * di
      }).sum / (l - 1)
      val v = if (v0.isNaN) 0 else v0
      val d = math.sqrt(v)
      (m, 1.64 * d / math.sqrt(l)) //1.64  1.96
   }

   def media_intervalo_confianca95(items: Vector[Double]) = {
      val s = items.sum
      val l = items.length.toDouble
      val m = s / l
      val v0 = (items map {
         x =>
            val di = x - m
            di * di
      }).sum / (l - 1)
      val v = if (v0.isNaN) 0 else v0
      val d = math.sqrt(v)
      (m, 1.96 * d / math.sqrt(l)) //1.64  1.96
   }

   def media_std_intervalo_confianca99(items: Vector[Double]) = {
      val s = items.sum
      val l = items.length.toDouble
      val m = s / l
      val v0 = (items map {
         x =>
            val di = x - m
            di * di
      }).sum / (l - 1)
      val v = if (v0.isNaN) 0 else v0
      val d = math.sqrt(v)
      (m, d, 2.58 * d / math.sqrt(l)) //1.64  1.96  2.58
   }
}
