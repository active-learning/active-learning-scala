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

import clus.data.rows.DataTuple;
import clus.data.type.ClusAttrType;

/**
 * This class represents Euclidian distances between DataTuples.
 */

public class EuclidianDistance extends VectorDistance {

	public EuclidianDistance(ClusAttrType[] attrs,double[] weights){
		super(attrs,weights);
	}

	/**
	 * Returns the (Euclidian) distance between the 2 given tuples
	 */
	public double getDistance(DataTuple t1,DataTuple t2){
		double dist = 0;
		double curDist;
		for (int i = 0; i < amountAttribs();i++){
			//calculate distance for current attribute
			curDist = getAttrib(i).getBasicDistance(t1,t2);
			//add to total
			//if(){
				dist += getWeight(i) * Math.pow(curDist,2);
			//}
			//dist += Math.pow(curDist,2);
		}
		return Math.sqrt(dist);
	}

	public String toString(){
		return "Euclidian";
	}
}
