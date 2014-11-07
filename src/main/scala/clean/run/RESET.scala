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

package clean.run

import clean.{AppWithUsage, Ds}

object RESET extends AppWithUsage {
  val arguments = superArguments.dropRight(1)
  val context = "RESETapp"
  run()
  core()

  def core() {
    datasets foreach { dataset =>
      val ds = Ds(dataset)
      ds.open()
      ds.reset()
      ds.log(s"$dataset resetado.", 20)
      ds.close()
    }
    log("Datasets zerados.")
  }
}
