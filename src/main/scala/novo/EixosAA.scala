package novo

import java.io.FileWriter

import al.strategies.{DistanceMeasure, EntropyMeasure}
import ml.Pattern
import ml.classifiers._
import util.Datasets

import scala.util.Random

object EixosAA extends App with DistanceMeasure with EntropyMeasure {
  val fast = !true
  val (distance_name, context, debug, learner) = ("eucl", "", false, NoLearner())
  val ds = Seq("unlabeled", "repr", "ign", "labeled", "fundo", "ent") map { arq =>
    Datasets.arff(s"/home/davi/Dropbox/git/als/$arq.arff", false, false) match {
      case Right(x) => x
      case Left(str) => sys.error(str)
    }
  }
  val (pool, dsr, dsi, dsl, fundo, dse) = (ds.head, ds(1), ds(2), ds(3), ds(4), ds(5))

//  val l = SVMLibRBF()
//    val l = RF(42, 20, threads = 8)
//    val l = RoF()
    val l = KNNBatchb(1, "eucl", fundo, weighted = true)

  val (mol, mor, moi, moe) = if (fast) (l.build(dsl), l.build(dsl), l.build(dsi), l.build(dse)) else (l.build(dsl), l.build(dsr), l.build(dsi), l.build(dse))

  val infs = pool map { ex =>
    val (x, y) = ex.vector.head -> ex.vector(1)
    val (diu, dir, dii, die) = (mol.distribution(ex), mor.distribution(ex), moi.distribution(ex), moe.distribution(ex))
    val m = diu.sorted.reverse(0)
    val mr = dir.sorted.reverse(0) - dir.sorted.reverse(1)
    val mi = dii.sorted.reverse(0) - dii.sorted.reverse(1)
    val mig = die.dropRight(1).sorted.reverse(0) - die.dropRight(1).sorted.reverse(1)
    val den = 1.8 * math.sqrt(dens(ex, pool ++ dsl))
    (x, y) ->(40*math.pow(m, 4.5d) / 0.85, 3 * math.pow(dir(6), 6d), math.pow(dii(5), 8d), den, math.pow((1 - m) * dir(6) * 5 * dii(5), 4), math.sqrt((1 - m) * den * 5 * dii(5)), mig, 1 - mr, 1 - mi)
  }

  val infs2 = fundo map { ex =>
    //  val infs = new Random(345).shuffle(pool) map { ex =>
    val (x, y) = ex.vector.head -> ex.vector(1)
    val (diu, dir, dii, die) = (mol.distribution(ex), mor.distribution(ex), moi.distribution(ex), moe.distribution(ex))
    val m = diu.sorted.reverse(0) - diu.sorted.reverse(1)
    val mr = dir.sorted.reverse(0) - dir.sorted.reverse(1)
    val mi = dii.sorted.reverse(0) - dii.sorted.reverse(1)
    // dom kno
    (x, y) ->(math.pow(1 - m, 1d), math.pow(1 - mr, 3), math.pow(1 - mi, 1 / 2d))
  }

  val infs3 = fundo map { ex =>
    val (x, y) = ex.vector.head -> ex.vector(1)
    val (diu, dir, dii, die) = (mol.distribution(ex), mor.distribution(ex), moi.distribution(ex), moe.distribution(ex))
    val m = diu.sorted.reverse(0)
    (x, y) ->(math.pow(m, 4.5d) / 0.85, 3 * math.pow(dir(6), 6d), math.pow(dii(5), 8d))
  }

  def dens(e: Pattern, p: Seq[Pattern]) = {
    val p2 = p.diff(Seq(e))
    val vals = p2.map { x => d(e, x) }.sorted take 50
    val md = vals.sum / vals.length
    math.pow(1d / (1 + md), 20d)
  }

  ((if (fast) Seq() else Seq("r")) ++ Seq("u", "i", "d", "e", "uir", "uid", "mig", "ub", "db", "kb", "ub2", "db2", "kb2")) foreach { t =>
    println(t)
    val fw = new FileWriter(s"/home/davi/$t.csv")
    fw.write("x y z\n")
    val inf = t match {
      case "u" => infs.map { case ((x, y), in) => (x, y) -> in._1 }.sortBy(_._2)
      case "r" => infs.map { case ((x, y), in) => (x, y) -> in._2 }.sortBy(_._2)
      case "i" => infs.map { case ((x, y), in) => (x, y) -> in._3 }.sortBy(_._2)
      case "d" => infs.map { case ((x, y), in) => (x, y) -> in._4 }.sortBy(_._2)
      case "uir" => infs.map { case ((x, y), in) => (x, y) -> in._5 }.sortBy(_._2)
      case "uid" => infs.map { case ((x, y), in) => (x, y) -> in._6 }.sortBy(_._2)
      case "mig" => infs.map { case ((x, y), in) => (x, y) -> in._7 }.sortBy(_._2)
      case "ub" => infs2.map { case ((x, y), in) => (x, y) -> in._1 }.sortBy(_._2)
      case "db" => infs2.map { case ((x, y), in) => (x, y) -> in._2 }.sortBy(_._2)
      case "kb" => infs2.map { case ((x, y), in) => (x, y) -> in._3 }.sortBy(_._2)
      case "ub2" => infs3.map { case ((x, y), in) => (x, y) -> in._1 }.sortBy(_._2)
      case "db2" => infs3.map { case ((x, y), in) => (x, y) -> in._2 }.sortBy(_._2)
      case "kb2" => infs3.map { case ((x, y), in) => (x, y) -> in._3 }.sortBy(_._2)
      case "e" => (pool ++ dsl).map { ex =>
        val (x, y) = ex.vector.head -> ex.vector(1)
        (x, y) -> (if (ex.label < 3) 1 else 0)
      }
    }
    inf foreach { case ((x, y), in) => fw.write(s"$x $y $in\n") }
    fw.close()
  }
}
