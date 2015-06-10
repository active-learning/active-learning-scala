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

import clus.main.*;
import clus.algo.tdidt.ClusNode;
import clus.data.rows.*;
import clus.statistic.ClusStatistic;
import java.util.Vector;
//import jeans.util.MyArray;
import jeans.tree.MyNode;

/**
 * This class represents a node of an extended decision tree.
 * It uses kNN to predict the class of a tuple when in a leaf node
 */

public class KNNTree extends ClusNode {

	public final static long serialVersionUID = Settings.SERIAL_VERSION_ID;

	public ClusStatManager m_SMgr;

	private VectorDistance $vectDist;//used for calculating distances in leafs
	private Vector $decisionData;//Tuples that are used in this node of the tree to decide target values.

	public KNNTree(ClusRun clRun,VectorDistance vDist){
		super();
		m_SMgr = clRun.getStatManager();

		$vectDist = vDist;

		//initialize decision data
		$decisionData = new Vector(1,10);

	}
	public KNNTree(KNNTree toClone){
		super();
		m_SMgr = toClone.m_SMgr;
		m_Test = toClone.m_Test;
		m_ClusteringStat = toClone.m_ClusteringStat;
		$vectDist = toClone.getVectorDistance();
		$decisionData = new Vector(1,10);
	}

	public final ClusStatistic predictWeighted(DataTuple tuple) {
		if (atBottomLevel()) {
			//return getTotalStat();

			return predictLeaf(tuple,getDecisionData());

		} else {
			int n_idx = m_Test.predictWeighted(tuple);
			if (n_idx != -1) {
				ClusNode info = (ClusNode)getChild(n_idx);
				return info.predictWeighted(tuple);
			} else {
				int nb_c = getNbChildren();
				ClusStatistic stat = m_ClusteringStat.cloneSimple();
				for (int i = 0; i < nb_c; i++) {
					ClusNode node = (ClusNode)getChild(i);
					ClusStatistic nodes = node.predictWeighted(tuple);
					stat.addPrediction(nodes, m_Test.getProportion(i));
				}
				return stat;
			}
		}
	}
	// Preforms kNN in a leafnode with given data to decide
	private ClusStatistic predictLeaf(DataTuple tuple,Vector dec_data){

		if (tuple == null) System.err.println("tuple == null");
		if (dec_data == null) System.err.println("dec_data == null");

		ClusStatistic stat = m_SMgr.createClusteringStat();
		//find out how much neighbours necessary (via settings)
		int amountNBS = Settings.kNNT_k.getValue();
		//find out if Distance-Weighted kNN used
		boolean distWeighted = Settings.kNNT_distWeighted.getValue();

		//make a priorityqueue of size amountNBS to find nearest neighbours
		//but when fewer elements for prediction then amountNBS then use only those
		if (amountDecisionData(dec_data)<amountNBS) amountNBS = amountDecisionData(dec_data);
		PriorityQueue q = new PriorityQueue(amountNBS);

		//Manier vinden om aan de datatuples te raken die bij dit blad horen

		int nbr = amountDecisionData(dec_data);

		DataTuple curTup;
		double dist;
		//find the nearest neighbours
		for (int i = 0; i <nbr;i++){
			//System.out.print(i);
			curTup = getTuple(dec_data,i);

			if (curTup == null) System.out.println("curTup == null");

			dist = calcDistance(tuple,curTup);
			q.addElement(curTup,dist);
		}
		//add the kNN's to the statistic

		//weights all the same for now: changed when needed
		double weight = 1.0;
		for (int i=0;i<amountNBS;i++){
			//Change weights when distance-weighted kNN is wanted
			if (distWeighted) weight = 1.0 / Math.pow(q.getValue(i),2);
			stat.updateWeighted((DataTuple)q.getElement(i),weight);
		}

		stat.calcMean();
		return stat;
	}

	/**
	 * This method transforms an ordinary ClusNode tree into a KNNTree.
	 */
	public static KNNTree makeTree(ClusRun cr ,ClusNode source,VectorDistance vd){
		KNNTree node = new KNNTree(cr,vd);
		node.m_Test = source.m_Test;
		node.m_ClusteringStat = source.m_ClusteringStat;

		int arity = source.getNbChildren();
		node.setNbChildren(arity);

		for (int i = 0; i < arity; i++) {
			ClusNode child = (ClusNode) source.getChild(i);
			node.setChild(KNNTree.makeTree(cr,child,vd), i);
		}
		return node;
	}

	/**
	 * Adds given DataTuple to the DataTuples used for prediction in this node.
	 */
	public void addTuple(DataTuple t){
		//if (atBottom()) { ??? misschien voor zorgen dat het echt alleen werkt als in leaf?
		$decisionData.addElement(t);
		//}
	}
	/**
	 * Returns the i'th DataTuple in the given decision data.
	 *
	 * Required
	 *		i < amountDecisionData(dec_data)
	 */
	public DataTuple getTuple(Vector dec_data,int i){
		//if (atBottomLevel())
		return (DataTuple) dec_data.elementAt(i);
		//else return null;
	}
	/**
	 * Returns the amount of DataTuples available for decision making.
	 */
	public int amountDecisionData(Vector dec_data){
		return dec_data.size();
	}

	// Calculates distance between 2 tuples
	private double calcDistance(DataTuple t1,DataTuple t2){
		if ($vectDist == null) System.out.println("$vectDist == null");

		return $vectDist.getDistance(t1,t2);
	}

	/*
	 * Returns a vector containing all decision data for this node,
	 * when not a leaf it returns the concatenated vector of all decisiondata
	 * from it's children.
	 */
	public Vector getDecisionData(){
		if (atBottomLevel()) return $decisionData;
		else {
			Vector dec_data = new Vector(5,5);
			KNNTree child;
			int arity = getNbChildren();
			for (int i = 0; i < arity; i++) {
				child = (KNNTree) getChild(i);
				dec_data.addAll(child.getDecisionData());
			}
			return dec_data;
		}
	}
	/**
	 * Sets teh decisiondata to given data.
	 * Required
	 *		dec_data may only contain DataTuple objects !!!!
	 */
	public void setDecisionData(Vector dec_data){
		$decisionData = dec_data;
	}

	public VectorDistance getVectorDistance(){
		return $vectDist;
	}

	/**
	 * This method makes the current node in the tree a leaf, thereby deleting
	 * it's children and adding there training examples to there own.
	 */
	public void makeLeaf(){
		if (!atBottomLevel()){
			//get decisiondata from children and make it own;
			$decisionData = getDecisionData();
			m_Test = null;
			cleanup();
			//remove the children
			removeAllChildren();
		}
	}
	/**
	 * Returns the value for the given tuple based on neirest neighbour but
	 * using the decision data from whole the subtree.
	 * (thus also usable for internal nodes)
	 */
	public ClusStatistic predictWeightedLeaf(DataTuple tuple) {
		//if (tuple == null) System.err.println("tuple == null");
		return predictLeaf(tuple,getDecisionData());
	}

	/**
	 * Returns a clone of this node
	 * required
	 * 		this is a leafnode : atBottomLevel() == True
	 */
	public MyNode cloneNode() {
		KNNTree clone = new KNNTree(this);
		if (atBottomLevel()) clone.setDecisionData(getDecisionData());
		return clone;
	}

}
