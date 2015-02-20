package clean.tex

import clean.lib.StratsTrait
import util.Datasets

import scala.util.Random

/*
elm-scala: an implementation of ELM in Scala using MTJ
Copyright (C) 2014 Davi Pereira dos Santos

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
object Toy extends App with StratsTrait {
   val rnd = new Random()

   def p(x0: Double, y0: Double, r: Double, primeiraMetade: Boolean, c: Int) {
      1l to (30 * r).round map { _ =>
         val (dx, dy) = 2 * r * 2 * (rnd.nextDouble() - 0.5) -> 2 * r * 2 * (rnd.nextDouble() - 0.5)
         val x = x0 + dx
         val y = y0 + dy
         if (math.sqrt(dx * dx + dy * dy) < r) {
            if (primeiraMetade) {
               if (x < x0) println(s"$x $y $c")
            } else {
               if (x > x0) println(s"$x $y $c")
            }
         }
      }
   }

   //   p(6, 6, 5, true, 0)
   //   p(16, 16, 1.5, false, 0)
   p(6, 6, 5, false, 1)
   p(16, 16, 1.5, true, 1)
   p(22, 22, 3, true, 1)
   p(22, 22, 3, false, 1)

   val arff = "/home/davi/unversioned/experimentos/fourclusters.arff"
   val data = Random.shuffle(Datasets.arff(arff).right.get)
   allStrats()
   //
   //   val fw = new PrintWriter(s"/home/davi/wcs/tese/toy$s.plot", "ISO-8859-1")
   //fw.close()
}

