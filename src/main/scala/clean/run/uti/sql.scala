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

package clean.run.uti

import clean.{AppWithUsage, Ds}

object sql extends AppWithUsage {
  lazy val arguments = superArguments ++ List("sql")
  val context = "sql"
  run()

  override def run() = {
    super.run()
    datasets foreach { dataset =>
      val ds = Ds(path, dataset)
      ds.open()
      if (sql.startsWith("insert") || sql.startsWith("update") || sql.startsWith("delete") || sql.startsWith("vacuum") || sql.startsWith(".pragma")) ds.write(sql)
      else println(s"${ds.read(sql)}")
      ds.close()
    }
    justQuit("Datasets prontos.")
  }
}
