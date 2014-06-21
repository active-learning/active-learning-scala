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

trait EntropyMeasure {
   protected def log2(x: Double) = if (x == 0) 0d else math.log(x) / math.log(2d)

   protected def log(x: Double) = if (x == 0) 0d else math.log(x)

   protected def entropy(P: Array[Double]) = -P.map(x => x * log2(x)).sum

   /*
   normalized_entropy:
   @article{ l e w i n 2 0 0 4 q u a n t i t a t i v e,
  title={Quantitative DNA methylation analysis based on four-dye trace data from direct sequencing of PCR amplificates},
  author={Lewin, J{\"o}rn and Schmitt, Armin O and Adorj{\'a}n, P{\'e}ter and Hildmann, Thomas and Piepenbrock, Christian},
  journal={Bioinformatics},
  volume={20},
  number={17},
  pages={3005--3012},
  year={2004},
  publisher={Oxford Univ Press}
  }*/
   protected def normalized_entropy(P: Array[Double]) = -P.map(x => x * log(x)).sum / log(P.length)
}
