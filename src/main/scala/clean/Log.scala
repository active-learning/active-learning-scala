package clean

import java.util.Calendar

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

trait Log {
  val context: String

  def log(msg: String, level: Int = Global.debug) {
    if (Global.debug > 0) level match {
      case x if x >= Global.debug => println(s"$context : $msg    ${Calendar.getInstance().getTime}")
      case x =>
    }
  }

  def error(msg: String) = {
    Global.running = false
    //    Thread.sleep(100)
    throw new Error(s"Error: $msg")
  }

  def justQuit(msg: String) = {
    log(s"Quiting: $msg", 20)
    Global.running = false
    //    Thread.sleep(100)
    sys.exit(1)
  }
}