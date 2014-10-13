package clean

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

trait CM {
  def printConfusion(m: Array[Array[Int]]) {
    m foreach { r =>
      r.foreach(c => print(s"$c "))
      println
    }
    println
  }

  def contaAcertos(m: Array[Array[Int]]) = {
    val n = m.size
    var i = 0
    var s = 0
    while (i < n) {
      s += m(i)(i)
      i += 1
    }
    s
  }

  def contaTotal(m: Array[Array[Int]]) = {
    val n = m.size
    var i = 0
    var j = 0
    var s = 0
    while (i < n) {
      j = 0
      while (j < n) {
        s += m(i)(j)
        j += 1
      }
      i += 1
    }
    s
  }

  def accPorClasse(m: Array[Array[Int]]) = {
    m.zipWithIndex map { case (li, idx) =>
      li(idx).toDouble / li.sum
    }
  }
}