package clean.tex

import java.io.File

import clean.lib._
import ml.Pattern
import ml.classifiers._
import util.{Datasets, Stat, Tempo}

object metaParesByPoolEscolhePares extends AppWithUsage with LearnerTrait with StratsTrait with RangeGenerator with Rank with MetaTrait {
  lazy val arguments = superArguments ++ List("learners:nb,5nn,c45,vfdt,ci,...|eci|i|ei|in|svm", "rank", "ntrees", "qtosPorBase", "vencedorOuPerdedor(use1):1|-1", "runs", "folds", "ini", "fim", "FS", "smote", "smotePropor", "suav", "semr")
  val context = this.getClass.getName.split('.').last.dropRight(1)
  val dedup = false
  //se mudar medida, precisa verficar mais dois lugares: dsminSize e no código. ALC é mais fácil.
  val measure = ALCKappa
  //1 100 200 (Kappa exige 200)
  val dsminSize = 1
  //n=2 estraga stats
  val n = 1
  run()

  override def run() = {
    super.run()
    val ls = learners(learnersStr)
    val metaclassifs = (patts: Vector[Pattern]) => if (porRank) Vector()
    else Vector(//NB não funciona porque quebra na discretização
      PCT(),
      NinteraELM(),
      PCTELM(),
      CIELMBatch(),
      //      SVMLibRBF(),
      C45(false, 5),
      KNNBatcha(5, "eucl", patts),
      RF(42, ntrees),
      Chute(),
      Maj()
    )
    //    stratsTex("all").drop(8) foreach { strat => //drop rnd,clu,atus,qbcrf,svms
    println(s"Escolher poucos leas!")
    val todospares = for {strat <- stratsTexPermuta("eucl"); l <- ls} yield strat -> l
    val permutacoes = todospares.combinations(2)
    println(s"${permutacoes.size} <- permutacoes")
    println(s"${todospares.size} <- todospares")

    permutacoes foreach { permuta =>
      val hash = permuta.map { case (s, l) => s(l).limpa }.mkString("-")
      Tempo.start
      //    $rus.$ks
      val arq = s"/home/davi/wcs/arff/$semR${if (suav) "suav" else ""}$context-n${if (porRank) 1 else n}best${criterio}m$measure-$ini.$fim-${"pares" + (if (porRank) "Rank" else "")}-$hash-p$dsminSize$featureSel.arff"
      val txt = s"/home/davi/results/$semR${if (suav) "suav" else ""}trAccCorrigida-$rus*$ks-fold-$context-n${if (porRank) 1 else n}best${criterio}m$measure-$ini.$fim-${"pares" + (if (porRank) "Rank" else "")}-$hash-p$dsminSize${metaclassifs(Vector()).map(_.limpa).mkString("-")}${if (apenasUmPorBase) "-umPBase" else ""}$featureSel${if (smote) "-SMOTE" else ""}-${ntrees}trees.txt"
      //      val arq = s"/home/davi/wcs/arff/$context-n${if (porRank) 1 else n}best${melhor}m$measure-$ini.$fim-${pares.map { case (s, l) => s(l).id }.mkString("-").hashCode.toLong.abs + (if (porRank) "Rank" else "")}-${learnerStr.replace(" ", ".")}-p$dsminSize.arff"
      val labels = permuta.map { case (s, le) => s(le).limpa }

      //cada dataset produz um bag de metaexemplos (|bag| >= 25)
      def bagsNaN = DsByMinSize(datasets, dsminSize).par map { d =>
        val ds = Ds(d, readOnly = true)
        ds.open()
        val (ti0, th0, tf0, tpass) = ranges(ds)
        val ti = if (ini == "ti") ti0 else th0
        val tf = if (fim == "th") th0 else tf0
        val res = for {
          r <- 0 until runs
          f <- 0 until folds
        } yield {
            //descobre vencedores deste pool
            val accs = permuta map { case (s, le) =>

              //            p -> measure(ds, Passive(Seq()), ds.bestPassiveLearner, r, f)(ti,tf).read(ds).getOrElse(error("sem medida"))

              (s(NoLearner()).limpa, le.limpa) -> measure(ds, s(le), le, r, f)(ti, tf).read(ds).getOrElse(error("sem medida"))

              //            val classif = BestClassifCV100_10foldReadOnlyKappa(ds, r, f, s(l))
              //            s(l) -> measure(ds, s(l), classif, r, f)(-2).read(ds).getOrElse(error("sem medida"))

            }
            //gera metaexemplos
            //            "\"#classes\",\"#atributos\",\"#exemplos\"," +
            //              "\"#exemplos/#atributos\",\"%nominais\",\"log(#exs)\",\"log(#exs/#atrs)\"," +
            //              "skewnessesmin,skewavg,skewnessesmax,skewnessesminByskewnessesmax," +
            //              "kurtosesmin,kurtavg,kurtosesmax,kurtosesminBykurtosesmax," +
            //              "nominalValuesCountmin,nominalValuesCountAvg,nominalValuesCountmax,nominalValuesCountminBynominalValuesCountmax," +
            //              "mediasmin,mediasavg,mediasmax,mediasminBymediasmax," +
            //              "desviosmin,desviosavg,desviosmax,desviosminBydesviosmax," +
            //              "entropiasmin,entropiasavg,entropiasmax,entropiasminByentropiasmax," +
            //              "correlsmin,correlsavg,correlsmax,correlsminBycorrelsmax,correleucmah,correleucman,correlmanmah","AH-conect.-Y", "AH-Dunn-Y", "AH-silhueta-Y", "AH-conect.-1.5Y", "AH-Dunn-1.5Y", "AH-silhueta-1.5Y",
            //            "AH-conect.-2Y", "AH-Dunn-2Y", "AH-silhueta-2Y", "kM-conect.-Y", "kM-Dunn-Y", "kM-silhueta-Y", "kM-conect.-1.5Y", "kM-Dunn-1.5Y",
            //            "kM-silhueta-1.5Y", "kM-conect.-2Y", "kM-Dunn-2Y", "kM-silhueta-2Y"
            //FS não ajudou, mesmo robando assim:
            val selecionados = Seq("nominalValuesCountmin", "AH-conect.-1.5Y", "AH-silhueta-1.5Y", "\"#atributos\"", "kurtosesmin", "skewnessesmin", "\"#classes\"", "correlsavg", "mediasavg", "AH-silhueta-Y")
            val metaatts00 = (if (semR == "sor") Seq() else ds.metaAttsrf(r, f, suav).map(x => (x._1, x._2.toString, x._3))) ++ (if (semR == "semr") Seq() else ds.metaAttsFromR(r, f).map(x => (x._1, x._2.toString, x._3)))
            val metaatts0 = if (featureSel == "fs") metaatts00.filter(x => selecionados.contains(x._1)) else metaatts00
            val metaatts = ("\"bag_" + permuta.size + "\"", ds.dataset, "string") +: metaatts0
            if (porRank) {
              //rank legivel por clus e ELM
              List(metaatts ++ ranqueia(accs.map(_._2)).zipWithIndex.map { case (x, i) => (s"class$i", x.toString, "numeric") } -> "")
            } else {
              //Acc
              val melhores = pegaMelhores(accs, n)(_._2 * criterio).map(_._1)
              melhores map (m => metaatts -> (m._1 + "-" + m._2))
            }
          }
        ds.close()
        res.flatten
      }
      def bags = bagsNaN
      if (!new File(arq).exists()) grava(arq, arff(labels.mkString(","), bags.toList.flatten, print = true, context, porRank))

      val patterns = Datasets.arff(arq, dedup, rmuseless = false) match {
        case Right(x) => if (apenasUmPorBase) {

          //não consegui descobrir como aumentar qtd de atributos no weka (vai ficar sem atts desvio.
          //          val dat = new Instances(x.head.dataset())
          //          println(s"${dat.numAttributes()} <- dat.numAttributes()")
          //          def at(nr: Int) = new Attribute("desviopad" + nr.toString)
          //          ((if (porRank) x.head.ndescs else x.head.numAttributes() - 1) to 1) foreach (nr => dat.insertAttributeAt(at(nr), 1))
          //          println(s"${dat.numAttributes()} <- dat.numAttributes()")
          //          val parent=PatternParent(dat)
          //          val ps = (x.groupBy(_.base).map(_._2) map meanPatternComDesvios(porRank, parent)).toVector

          val ps = (x.groupBy(_.base).map(_._2) map meanPattern(porRank)).toVector

          patts2file(ps, arq + "umPorBase.arff")
          //          out(s"Apenas um por base = ${ps.size}! Apenas um por base!")
          //          out("umPorBase" + s"$arq")
          ps
        } else x
        case Left(m) => error(s"${m} <- m")
      }

      //      if (!new File(txt).exists) {


      if (porRank) print(s"${permuta} Spearman correl. $rus*$ks-fold CV. ${if (smote) "-SMOTE" else ""}" + " " + arq + " ")
      else print(s"${permuta} Accuracy. $rus*$ks-fold CV. $featureSel ${if (smote) "-SMOTE" else ""}" + " " + arq + " ")

      val sm = (if (semR == "all") "" else semR) + (if (suav) "suav" else "") + (if (smote) s"sm$smotePropor" else "nosm")
      val fsel = featureSel
      val ra = if (porRank) "ra" else "ac"
      val metads = new Db("meta", readOnly = false)
      metads.open()
      metads.readString(s"select mc from r where ls='$hash' and st='${permuta}' and sm='$sm' and nt=$ntrees and fsel='$fsel' and ra='$ra' and rs=$rus and fs=$ks") match {
        case x: List[Vector[String]] if x.map(_.head).intersect(metaclassifs(Vector()).map(_.limp)).size == 0 =>
          val porMetaLea = cv({
            "pares"
          }, smotePropor, smote, ntrees, featureSel, patterns, metaclassifs, porRank, rus, ks).toVector.flatten.flatten.groupBy(_.metalearner)
          def fo(x: Double) = "%2.3f".format(x)

          porMetaLea foreach { case (nome, resultados) =>
            val accTr = Stat.media_desvioPadrao(resultados.map(_.accTr))
            val accTs = Stat.media_desvioPadrao(resultados.map(_.accTs))
            val accBalTr = Stat.media_desvioPadrao(resultados.map(_.accBalTr))
            val accBalTs = Stat.media_desvioPadrao(resultados.map(_.accBalTs))
            val r = resultados reduce (_ ++ _)
            val (resumoTr, resumoTs) = r.resumoTr -> r.resumoTs
            //ls: permuta de leas ou strats (pra marcar experimento)
            //st: item atual dentro da permuta  (cada item é uma execução do programa todo.)
            metads.write(s"insert  into r values ('$ra', '$sm', $criterio, '$ini', '$fim', '${permuta}', '$hash', $rus, $ks, '$nome', $ntrees, '$fsel', $dsminSize, ${accTr._1}, ${accTr._2}, ${accTs._1}, ${accTs._2}, ${accBalTr._1}, ${accBalTr._2}, ${accBalTs._1}, ${accBalTs._2}, '$resumoTr', '$resumoTs')")
            (nome, accTs) -> s"${nome.padTo(8, " ").mkString}:\t${fo(accTr._1)}/${fo(accTr._2)}\t${fo(accTs._1)}/${fo(accTs._2)}"
          }
        case x: List[Vector[String]] => println(s"${x} <- rows already stored")
        //      outp.toList.sortBy(_._1._2).reverseMap(_._2) foreach out

        //      out("histogramas ===========================")
        //      out(s"${pares.map { case (s, l) => l.limpa }.mkString(" ")}")
        //      porMetaLea foreach { case (nome, resultados) =>
        //        val r = resultados reduce (_ ++ _)
        //        //          out(s"$nome: ------------")
        //        r.histTr.padTo(6, "   ").zip(r.histTrPred.padTo(6, "   ")).map(x => x._1 + "\t\t" + x._2).take(ls.size).map(x => s"metale:$nome tr " + x) foreach out
        //        r.histTs.padTo(6, "   ").zip(r.histTsPred.padTo(6, "   ")).map(x => x._1 + "\t\t" + x._2).take(ls.size).map(x => s"  metale:$nome ts " + x) foreach out
        //        out("")
        //      }

        //      out(Tempo.stop + "s")
        //      val fw = new FileWriter(txt)
        //      fw.write(tx1.split('\n').map(x => s"st:$stratName " + x).mkString("\n"))
        //      //      fw.write(tx2)
        //      fw.close()
      }
      metads.close()
      println()
      //      } else {
      //        val arq = Source.fromFile(txt)
      //        val str = arq.getLines().toList.mkString("\n")
      //        arq.close()
      //        println(str)
      //      }

      //      if (!porRank) Datasets.arff(arq, dedup, rmuseless = false) match {
      //        case Right(x) => if (apenasUmPorBase) {
      //          val ps = (x.groupBy(_.base).map(_._2) map meanPattern(porRank)).toVector
      //          patts2file(ps, arq + "umPorBase")
      //          C45(laplace = false, 5, 1).tree(arq + "umPorBase.arff", arq + "umPorBase" + ".tex")
      //          println(s"${arq} <- arq")
      //        }
      //      }
    }
  }
}

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
