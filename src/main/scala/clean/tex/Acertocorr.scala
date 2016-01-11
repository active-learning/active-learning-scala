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

package clean.tex

import clean.lib.Ds

object Acertocorr extends App {
  val metads = Ds("metanew", readOnly = true)
  val dominio = Map("abalone-3class" -> 0, "artificial-characters" -> 2, "autoUniv-au1-1000" -> 3, "autoUniv-au6-cd1-400" -> 3, "autoUniv-au7-300-drift-au7-cpd1-800" -> 3, "autoUniv-au7-700" -> 3, "autoUniv-au7-cpd1-500" -> 3, "balance-scale" -> 4, "banana" -> 5, "banknote-authentication" -> 6, "bupa" -> 7, "car-evaluation" -> 8, "cardiotocography-3class" -> 9, "climate-simulation-craches" -> 10, "connectionist-mines-vs-rocks" -> 11,
    "connectionist-vowel" -> 11, "ecoli" -> 12, "eeg-eye-state" -> 13, "first-order-theorem" -> 14, "flare" -> 15, "glass" -> 16, "habermans-survival" -> 17, "heart-disease-processed-cleveland" -> 18, "heart-disease-processed-hungarian" -> 18, "heart-disease-processed-va" -> 18, "hepatitis" -> 19, "hill-valley-without-noise" -> 20, "horse-colic-surgical" -> 21, "indian-liver-patient" -> 22, "ionosphere" -> 23,
    "iris" -> 24, "kr-vs-kp" -> 25, "leaf" -> 26, "lymphography" -> 27, "magic" -> 28, "mammographic-mass" -> 29, "mfeat-fourier" -> 30, "molecular-splice-junction" -> 31, "monks1" -> 32, "monks3" -> 32, "movement-libras" -> 33, "mushroom" -> 34, "musk" -> 35, "nursery" -> 36, "optdigits" -> 37, "ozone-eighthr" -> 38, "page-blocks" -> 39, "parkinsons" -> 40, "pendigits" -> 41,
    "phoneme" -> 42, "pima-indians-diabetes" -> 43, "qsar-biodegradation" -> 44, "ringnorm" -> 45, "robot-failure-lp5" -> 46, "robot-nav-sensor-readings-2" -> 46, "saheart" -> 47, "seeds" -> 48, "spambase" -> 49, "spect-heart" -> 18, "statlog-australian-credit" -> 50, "statlog-german-credit" -> 50, "statlog-heart" -> 50, "statlog-image-segmentation" -> 50, "statlog-vehicle-silhouettes" -> 50, "steel-plates-faults" -> 51,
    "systhetic-control" -> 52, "texture" -> 53, "thyroid-ann" -> 54, "thyroid-hypothyroid" -> 54, "thyroid-newthyroid" -> 54, "thyroid-sick-euthyroid" -> 54, "tic-tac-toe" -> 55, "turkiye-student" -> 56, "twonorm" -> 57, "user-knowledge" -> 58, "vertebra-column-2c" -> 59, "vertebra-column-3c" -> 59, "volcanoes-a3" -> 60, "volcanoes-b5" -> 60, "volcanoes-d1" -> 60, "volcanoes-e1" -> 60, "voting" -> 61, "waveform-v2" -> 62,
    "wdbc" -> 63, "wholesale-channel" -> 64, "wilt" -> 65, "wine" -> 66, "wine-quality-red" -> 66, "wine-quality-white-5class" -> 66, "yeast-4class" -> 67)

  case class registro(ds: String, pctr: Double, defr: Double, dif: Double)

  val i = "th"
  val t = metads.readString(s"select a.ds,a.spea,b.spea,a.spea-b.spea from rank a, rank b where a.ds=b.ds and a.ra=b.ra and a.cr=b.cr and a.i=b.i and a.f=b.f and a.st=b.st and a.ls=b.ls and a.rs=b.rs and a.fs=b.fs and a.nt=b.nt and a.porPool=b.porPool and a.mc='PCTr' and b.mc='defr' and a.st='HTUeuc' and a.i='$i'; ")
  val r = t map { case Vector(ds, pctr, defr, dif) => registro(ds, pctr.toDouble, defr.toDouble, dif.toDouble) }

  def qtosRoubos(ds: String) = dominio.values.count(_ == dominio(ds))-1

  def f(x: Double) = {
    val nf = java.text.NumberFormat.getNumberInstance(new java.util.Locale("pt", "BR"))
    nf.setMinimumFractionDigits(2)
    nf.setMaximumFractionDigits(2)
    nf.format(x)
  }

  val ds2reg = r.map(x => x.ds -> x).toMap
  val dss = dominio.keys.toList
  val res = (dss map qtosRoubos).zip(dss map ds2reg).sortBy(_._2.ds).sortBy(-_._2.dif).sortBy(-_._1).zipWithIndex foreach { case ((rou, registro(ds, pctr, defr, dif)), idx) =>
    println(s"${idx + 1}-$ds ${f(rou)} ${f(dif)} $pctr $defr")
  }
  metads.close()
}