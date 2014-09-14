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
  def shrinkToBytes(numbers: Seq[Int], bits: Int = 12) = {
    println(numbers)
    val binary = numbers flatMap (n => Integer.toBinaryString(n).takeRight(bits).reverse.padTo(12, 0).reverse)
    val n = binary.size
    val pad32bit = binary.reverse.padTo(n + (8 - n % 8), 0).reverse
   Converter.fromBinary(pad32bit.mkString)
  }

  def stretchFromBytes(bytes: Array[Byte], bits: Int = 12) = {
    val a = Converter.toBinary(bytes)
    val b = a.drop(a.size % 12).grouped(12).map(_.mkString).toList
   b.map(Integer.parseInt(_, 2))
  }
}