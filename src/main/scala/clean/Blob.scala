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

trait Blob {
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

  def confusionToBlob(m: Array[Array[Int]]) = shrinkToBytes(m.flatten)

  def shrinkToBytes(numbers: Seq[Int], bits: Int = 12) = {
    val binary = numbers flatMap { num =>
      val str32bit = Integer.toBinaryString(num)
      val str12bit = str32bit.reverse.padTo(12, 0).reverse
      str12bit
    }
    val n = binary.size
    val pad8bit = binary.reverse.padTo(n + (8 - (n % 8)), 0).reverse
    val r = BaseConverter.fromBinary(pad8bit.mkString)
    r
  }

  def blobToConfusion(b: Array[Byte], nclasses: Int) = stretchFromBytes(b).grouped(nclasses).map(_.toArray).toArray

  def stretchFromBytes(bytes: Array[Byte], bits: Int = 12) = {
    val a = BaseConverter.toBinary(bytes)
    val b = a.drop(a.size % 12)
    val c = b.grouped(12).map(_.mkString).toList
    val d = c.map(Integer.parseInt(_, 2))
    d
  }

  def printConfusion(m: Array[Array[Int]]) {
    m foreach { r =>
      r.foreach(c => print(s"$c "))
      println
    }
    println
  }
}