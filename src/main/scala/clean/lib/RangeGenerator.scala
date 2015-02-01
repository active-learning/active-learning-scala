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

trait RangeGenerator {
   def ranges(ds: Ds) = {
      val tmin = ds.nclasses - 1
      val tmax = math.min(ds.expectedPoolSizes(Global.folds).min, 200) - 1 //maximiza utilidade do experimento pra calcular ALC
      val thalf = tmax / 2
      val tpass = math.min(ds.expectedPoolSizes(Global.folds).min / 2, 199) //maximiza utilidade do experimento pra comparar com passiva
      (tmin, thalf, tmax, tpass)
   }
}

