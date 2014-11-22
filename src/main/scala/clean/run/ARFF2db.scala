package clean.run

import java.io.File

import clean.{Global, AppWithUsage}
import util.Datasets
import weka.core.Instances
import weka.core.converters.DatabaseSaverCustomized

import scala.util.{Failure, Success, Try}

object ARFF2db extends AppWithUsage {
  val context = "ARFF2db"
  val arguments = superArguments ++ Seq("path")
  run()

  override def run() = {
    super.run()
    (if (parallelDatasets) datasets.toList.par else datasets.toList) foreach { name =>
      val instances = Datasets.arff(path + name + ".arff") match {
        case Left(str) => println(str + "\nSkipping dataset '" + name + "'."); None
        case Right(data) =>
          println("Processing '" + name + "':")
          val insts = new Instances(data.head.dataset(), data.length)
          data foreach { p =>
            //Using weight temporarily as clipboard for id.
            if (p.weight() != 1) throw new Error(s"Weight ${p.weight()} differs from 1! That info would be lost.")
            p.setWeight(p.id)
            insts.add(p)
          }
          Some(insts)
      }

      val save = new DatabaseSaverCustomized
       val url = s"jdbc:mysql://${Global.mysqlHost(readOnly = false)}:${Global.mysqlPort(readOnly = false)}/"
      save.name = name
      save.setUrl(url)
      save.setInstances(instances.get)
      save.setUser("davi")
       save.setPassword(Global.mysqlPass(readOnly = false))
      save.setRelationForTableName(false)
      save.setTableName("i")
      save.setAutoKeyGeneration(true)
      save.connectToDatabase()
      Try(save.writeBatchExcep()) match {
        case Success(_) =>
          //The weights were used as clipboard, but weight info is not written to DB. That's good.
          // instances variable cannot be used after this unless weights are properly recovered.
          println(" Finished: '" + name + "'.")
        case Failure(ex) =>
          println(ex + "\nSkipping dataset '" + name + "'.")
      }
      println(s"")
    }
  }
}
