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

package al.strategies

import ml.Pattern
import ml.classifiers.Maj

case class Majoritary(pool: Seq[Pattern], debug: Boolean = false)
  extends StrategyAgnostic {
  override val toString = "Majoritary"
  val abr = "Maj"
  val id = 21

  def learner = Maj()

  protected def next(unlabeled: Seq[Pattern], labeled: Seq[Pattern]) = {
    unlabeled.head
  }

  protected def visual_test(selected: Pattern, unlabeled: Seq[Pattern], labeled: Seq[Pattern]) {
    plot.zera()
    for (p <- distinct_pool) plot.bola(p.x, p.y, p.label.toInt, 9)
    for (p <- labeled) plot.bola(p.x, p.y, p.label.toInt + 5, 9)
    if (selected != null) plot.bola(selected.x, selected.y, -1, 15)
    plot.mostra()
    Thread.sleep((delay * 1000).round.toInt)
  }
}

