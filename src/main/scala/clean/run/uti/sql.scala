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
      (if (parallelDatasets) datasets.par else datasets) foreach { dataset =>
         val ds = Ds(dataset, readOnly = false)
         ds.open()
         sql.split(";").foreach { s =>
            val sq = s.trim.toLowerCase
            if (sq.startsWith("alter") || sq.startsWith("insert") || sq.startsWith("update") || sq.startsWith("delete") || sq.startsWith("vacuum") || sq.startsWith(".pragma")) ds.write(sq)
            else println(s"${ds.read(sq)}")
         }
         ds.close()
      }
      justQuit("Datasets prontos.\n" + args.toList)
   }
}
