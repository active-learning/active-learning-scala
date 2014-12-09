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
   /**
    * limits for values: [-4; 4]
    * @param m
    * @return
    */
   def doublesTobdString(m: Seq[Double]) = if (m.exists(x => x < -4 || x > 4)) {
      println(s"${m.toList}\nfora do intervalo [-4;4] na conversão de valores double para string!")
      sys.exit(1)
   } else m.map(x => "%5.4f".format(x + 4).replace(".", "")).mkString

   def bdstringToDoubles(s: String) = s.grouped(5).map(x => (x.head + "." + x.tail).toDouble - 4).toVector

   /**
    * lossless
    * @param numbers
    * @param bits
    * @return
    */
   def shrinkToBytes(numbers: Seq[Int], bits: Int = 12) = {
      val binary = numbers flatMap { num =>
         val str32bit = Integer.toBinaryString(num)
         val str12bit = str32bit.reverse.padTo(bits, 0).reverse
         str12bit
      }
      val n = binary.size
      val pad8bit = binary.reverse.padTo(n + (8 - (n % 8)), 0).reverse
      val r = BaseConverter.fromBinary(pad8bit.mkString)
      r
   }

   def confusionToBlob(m: Array[Array[Int]]) = shrinkToBytes(m.flatten)

   def blobToConfusion(b: Array[Byte], nclasses: Int) = stretchFromBytes(b).grouped(nclasses).map(_.toArray).toArray

   def stretchFromBytes(bytes: Array[Byte], bits: Int = 12) = {
      val a = BaseConverter.toBinary(bytes)
      val b = a.drop(a.size % bits)
      val c = b.grouped(bits).map(_.mkString)
      val d = c.map(Integer.parseInt(_, 2))
      d
   }
}

object BlobTest extends App with Blob {
   val ar = Array(-1.3210, 0.6754, 0, -1, 1, -4, 4, 4)
   val str = doublesTobdString(ar)
   println(str)
   println(s"${ar.toList} == ${bdstringToDoubles(str)}")
}