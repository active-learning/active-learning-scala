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

package clean.run.oth

import al.strategies.SVMmulti
import clean.nonQ
import ml.Pattern

/**
 * SVM strats precisam ser separadas porque só vão aceitar um learner: SVMLib().
 * Ele atrapalharia fast() quando fosse desejado NB() para as outras strats.
 */
object svm extends nonQ {
  override val arguments = superArguments
  val context = "SVMapp"
  override lazy val learnerStr = "svm"
  run()

  def strats(pool: Seq[Pattern], learnerSeed: Int) = List(
    SVMmulti(pool, "SELF_CONF"),
    SVMmulti(pool, "KFF"),
    SVMmulti(pool, "BALANCED_EE"),
    SVMmulti(pool, "SIMPLE")
  )
}
