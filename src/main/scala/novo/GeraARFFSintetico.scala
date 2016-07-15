package novo

import java.io.FileWriter

import scala.util.Random

object GeraARFFSintetico extends App {
  val rnd = new Random(1324)

  def printuple[X](c: Int, w: Double)(tup: (X, X)) = tup._1 + ", " + tup._2 + ", " + c + s", {$w}\n"

  def disturb(s: Double)(x: Double, y: Double) = (s * rnd.nextDouble() + x - s / 2, s * rnd.nextDouble() + y - s / 2)

  def espiralAleatoria(x: Double, y: Double, n: Int) = {
    val ids = (1 to 10 * n by 5) map (x => math.sqrt(x.toDouble))
    val angs = ids map (_ * 1)
    val raios = ids map (_ * 0.1)
    angs.zip(raios) map { case (a, r) => disturb(math.sqrt(r) * 2)(x + r * math.cos(a), y + r * math.sin(a)) }
  }

  def uniforme(n: Int) = for {x <- -1000 to 1000 by n; y <- -1000 to 1000 by n} yield (x / 100d, y / 100d)

  val labeled = Seq((6, 6, 0), (4, 0, 1), (5, 4, 0), (-6, -4, 1),  (-5, -5, 1), (-2, -3, 2), (-2, -6, 1),
    (6.2, 6.1, 0), (4.1, -0.2, 1), (5.2, 3.9, 0), (-5.8, -4.1, 1),  (-5.05, -5.1, 1), (-2.1, -2.9, 2), (-2.2, -5.1, 1),
    (6.3, 6.4, 0), (4.3, -0.4, 1), (5.3, 3.4, 0))
  val unlabeled = Seq[(Double, Double, Int)]((5, 5, 15), (7, 7, 15), (-3, 0, 20), (6, 0, 20), (-3.8, 3.5, 10), (-4.2, 2.5, 10), (3, 0, 30), (6, -4, 20), (3, 3, 30), (-6, -4, 30), (0, -4, 20), (0, 0, 30))

  Seq("unlabeled", "labeled", "ign", "repr", "fundo", "ent") foreach { set =>
    val fw = new FileWriter(s"/home/davi/Dropbox/git/als/$set.arff")
    fw.write(s"@relation synthetic-$set\n")
    fw.write("@attribute x numeric\n")
    fw.write("@attribute y numeric\n")
    fw.write("@attribute c {0,1,2,9,10,19,24,29,39,40,41,42,49}\n")
    fw.write("@data\n")


    set match {
      case "labeled" =>
        labeled foreach { case (x, y, c) => fw.write(printuple(c, 1)(x, y)) }
      case "unlabeled" =>
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(9, 1)(x))) }
      case "ign" =>
        labeled foreach { case (x, y, c) => fw.write(printuple(10, 1)(x, y)) }
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(19, 0.00005)(x))) }
      case "repr" =>
        labeled foreach { case (x, y, c) => fw.write(printuple(24, 1)(x, y)) }
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(24, 1)(x))) }
        uniforme(5) foreach (x => fw.write(printuple(29, 0.00005)(x)))
      case "fundo" =>
        uniforme(5) foreach (x => fw.write(printuple(39, 1)(x)))
      case "ent" => //mig
        labeled foreach { case (x, y, c) => fw.write(printuple(40 + c, 1)(x, y)) }
        unlabeled foreach { case (x, y, n) => espiralAleatoria(x, y, n) foreach (x => fw.write(printuple(49, 0.001)(x))) }
    }
    fw.close()
  }
}
