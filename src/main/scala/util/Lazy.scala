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

/**
 * http://blog.adamdklein.com/?p=689
 */
object Lazy {
   class LazyCache[T](v: => T) {
      private var state: Option[T] = None

      def value: T = if (state.isDefined) state.get else {
         state = Some(v)
         state.get
      }

      def reset() {
         state = None
      }
   }

   object LazyCache {
      import scala.language.implicitConversions
      def apply[T](v: => T) = new LazyCache[T](v)
      implicit def unwrap[T](v: LazyCache[T]): T = v.value
   }
}
