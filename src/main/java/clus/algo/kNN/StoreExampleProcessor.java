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

import clus.model.ClusModel;
import clus.model.processor.ClusModelProcessor;
import clus.data.type.*;
import clus.data.rows.DataTuple;

import java.io.IOException;

/**
 * This class represents a ModelProcessor that is used for storing tuples in
 * the nodes of a KNNTree which are used for prediction of target values.
 */
public class StoreExampleProcessor extends ClusModelProcessor {

	public void initialize(ClusModel model, ClusSchema schema) throws IOException{
		//does nothing
	}

	public void terminate(ClusModel model) throws IOException{
		//does nothing
	}

	/**
	 * This method adds the given tuple to the given KNNTree.
	 * is used in the correct way by the method applyModelProcessors in ClusNode
	 * Required
	 *		model: instance of KNNTree
	 */
	public void modelUpdate(DataTuple tuple, ClusModel model) throws IOException {
		//this fails when model isn't a KNNTree
		KNNTree tree_node = (KNNTree) model;
		//add the tuple to the node (leaf).
		tree_node.addTuple(tuple);
	}

}
