/*************************************************************************
 * Clus - Software for Predictive Clustering                             *
 * Copyright (C) 2007                                                    *
 *    Katholieke Universiteit Leuven, Leuven, Belgium                    *
 *    Jozef Stefan Institute, Ljubljana, Slovenia                        *
 *                                                                       *
 * This program is free software: you can redistribute it and/or modify  *
 * it under the terms of the GNU General Public License as published by  *
 * the Free Software Foundation, either version 3 of the License, or     *
 * (at your option) any later version.                                   *
 *                                                                       *
 * This program is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 * GNU General Public License for more details.                          *
 *                                                                       *
 * You should have received a copy of the GNU General Public License     *
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *
 *                                                                       *
 * Contact information: <http://www.cs.kuleuven.be/~dtai/clus/>.         *
 *************************************************************************/

package clus.algo.kNN;

/**
 * This class stores some useful statistics for a Numeric Attribute
 * of certain data.
 */
public class NumericStatistic extends AttributeStatistic{

	private double $mean,$variance,$min,$max;

	public NumericStatistic(){
	}

	public double mean(){
		return $mean;
	}
	public void setMean(double m){
		$mean = m;
	}
	public double variance(){
		return $variance;
	}
	public void setVariance(double v){
		$variance = v;
	}
	public double min(){
		return $min;
	}
	public void setMin(double m){
		$min = m;
	}
	public double max(){
		return $max;
	}
	public void setMax(double m){
		$max = m;
	}
}
