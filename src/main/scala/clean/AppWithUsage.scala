package clean

import java.security.SecureRandom
import java.util.UUID

import util.XSRandom

import scala.util.Random

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

trait AppWithUsage extends App with Log with ArgParser {
   //  Class.forName("org.sqlite.JDBC")
   val superArguments = List("debug-verbosity:-1,0,1,2,...,30", "files-with-dataset-names-or-dataset-names:file1,file2|#d1,d2,d3", "paralleliz(runs folds):r|f|rf|d", "maxtimesteps")
   val arguments: List[String]
   lazy val runs = Global.runs
   lazy val folds = Global.folds
   lazy val debugIntensity = if (args.isEmpty) 20 else args(0).toInt
   lazy val maxQueries0 = args(3).toInt
   lazy val sql = args(4)
   lazy val normalizar = args(4).contains("y")
   lazy val path = args(4) + "/"
   lazy val trulyrnd = new SecureRandom()
   lazy val seed = trulyrnd.nextInt() + System.nanoTime() + System.currentTimeMillis() + UUID.randomUUID().toString.map(_.toByte).map(_.toInt).sum
   lazy val xsrnd = {
      val tmp = new XSRandom()
      tmp.setSeed(seed)
      tmp
   }
   lazy val rnd = new Random(xsrnd.nextInt())
   lazy val datasets = if (args(1).startsWith("@")) datasetsFromFiles(args(1).drop(1)).toList
   else rnd.shuffle(if (args(1).startsWith("#")) args(1).drop(1).split(',').toList else datasetsFromFiles(args(1)).toList)
   //  lazy val datasets = if (args(1).startsWith("#")) args(1).drop(1).split(',') else datasetsFromFiles(args(1))
   lazy val parallelRuns = args(2).contains("r")
   lazy val parallelFolds = args(2).contains("f")
   lazy val parallelDatasets = args(2).contains("d")
   lazy val learnerStr = if (args.size < 5) "learner-undefined" else args(4)
   lazy val learnersStr = if (args.size < 5) Array("learners-undefined") else args(4).split(",")
   //  lazy val measure = args.last match {
   //    case "alca" => ALCacc()
   //    case "alcg" => ALCgmeans()
   //    case "aatq" => accAtQ()
   //    case "gatq" => gmeansAtQ()
   //    case "pa" => passiveAcc()
   //    case "pg" => passiveGme()
   //  }
   lazy val memlimit = Global.memlimit

   def maxQueries(ds: Ds) = math.max(ds.nclasses, math.min(ds.expectedPoolSizes(folds).min, maxQueries0))

   def memoryMonitor() = {
      Global.running = true
      new Thread(new Runnable() {
         def run() {
            while (Global.running) {
               //60s
               1 to 300 takeWhile { _ =>
                  Thread.sleep(200)
                  Global.running
               }
               log(s"Memory usage: ${Runtime.getRuntime.totalMemory() / 1000000d}MB.", 30)
               //          if (Runtime.getRuntime.totalMemory() / 1000000d > memlimit) {
               //            Global.running = false
               //            error(s"Limite de $memlimit MB de memoria atingido.")
               //          }
            }
            log("Saiu do monitoramento de memória.", 30)
         }
      }).start()
   }

   def run() {
      try {
         Global.debug = debugIntensity
         println(args.mkString(" "))
         if (args.size != arguments.size) {
            println(s"Usage: java -cp your-path/als-version.jar ${this.getClass.getCanonicalName.dropRight(1)} ${arguments.mkString(" ")}")
            sys.exit(1)
         }
      } catch {
         case ex: Throwable => Global.running = false
            ex.printStackTrace()
            justQuit("Erro: " + ex.getMessage)
      }
   }
}
