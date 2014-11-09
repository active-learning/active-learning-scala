package clean.run

import clean.AppWithUsage
import util.Datasets
import weka.core.Instances
import weka.core.converters.DatabaseSaverCustomized

class ARFF2db extends AppWithUsage {
  val context = "ARFF2db"
  val arguments = superArguments ++ Seq("path")

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
    save.setUrl("jdbc:sqlite:////" + path + name + ".db")
    save.setInstances(instances.get)
    save.setRelationForTableName(false)
    save.setTableName("i")
    save.setAutoKeyGeneration(true)
    save.connectToDatabase()
  }
}
