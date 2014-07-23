/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package svmal;

import java.io.BufferedReader;
import java.io.FileReader;

import svmal.ml.active.BALANCED_EE;
import svmal.ml.active.KFF;
import svmal.ml.active.SELF_CONF;
import svmal.ml.active.SIMPLE;
import svmal.weka.active.ActiveLearner;
import svmal.libsvm.SvmLib;
import svmal.weka.core.DataContainer;
import svmal.weka.core.Instance2;
import svmal.weka.core.InstanceContainer;
import weka.core.*;

public class SVMStrategy {

    ActiveLearner learner;
    DataContainer labeled = null;
    DataContainer unlabeled = null;
    private SvmLib svm = new SvmLib();

//    public SVMStrategy(String strategy, weka.core.Instances firstOfEachClass, Instances rest) throws Exception {
//    }

    /**
     * mutates both sets of instances!
     *
     * @param strategy
     * @param firstOfEachClass
     * @param rest
     * @throws Exception
     */
    public SVMStrategy(String strategy, Instances firstOfEachClass, Instances rest) throws Exception {
        unlabeled = svm.convertData(rest);
        labeled = svm.convertData(firstOfEachClass, unlabeled);
        switch (strategy) {
            case "SIMPLE":
                /*
                    Tong
                 */
                learner = new SIMPLE();
                break;
            case "SELF_CONF":
                /*
                 * This implementation is an SVM-based variant of SELF-CONF.
                 * For SVM, probabilistic estimates are obtained in a standard way, using logistic regression.
                 * The code uses random subsampling: On each trial we estimate the `self estimated expected log-loss' for only a random subset of U.
                 * The subsample in the first active session trial contains 100 points; on each subsequent trial we decrement the subsample size by one point
                 * until we reach a minimum of 10 points, which we keep for the remaining trials.

                 * N. Roy and A. McCallum.
                 * Toward optimal active learning through sampling estimation of error reduction.
                 * Precedings of ICML-2001 pages 441�448, 2001.

                    @article{baram2004online,
                      title={Online choice of active learning algorithms},
                      author={Baram, Yoram and El-Yaniv, Ran and Luz, Kobi},
                      journal={The Journal of Machine Learning Research},
                      volume={5},
                      pages={255--291},
                      year={2004},
                      publisher={JMLR. org}
                    }
                 */
                learner = new SELF_CONF();
                break;
            case "KFF":
                /*
                    A simple active-learning heuristic based on “farthest-first” traversal sequences
                    in kernel space. Farthest-first (FF) sequences have been previously used for computing provably
                    approximate optimal clustering for k-center problems (Hochbaum and Shmoys, 1985). The FF
                    traversal of the points in a data set is defined as follows. Start with any point x and find the farthest
                    point from x. Then find the farthest point from the first two (where the distance of a point from a set
                    is defined to be the minimum distance to a point in the set), etc. In any metric space, FF traversals
                    can be used for computing 2-approximation solutions to the k-center clustering problem in which
                    one seeks an optimal k-clustering of the data and optimality is measured by the maximum diameter
                    of the clusters. In particular, by taking the first k elements in the FF traversal as “centroids” and then
                    assigning each other point to its closest “centroid”, one obtains a k-clustering whose cost is within
                    a factor 2 of the optimal (Hochbaum and Shmoys, 1985).
                    @article{baram2004online,
                      title={Online choice of active learning algorithms},
                      author={Baram, Yoram and El-Yaniv, Ran and Luz, Kobi},
                      journal={The Journal of Machine Learning Research},
                      volume={5},
                      pages={255--291},
                      year={2004},
                      publisher={JMLR. org}
                    }
                    */
                learner = new KFF();
                break;
            case "BALANCED_EE":
                /*
                 * Parece que é o sucessor do MAB3.
                 * The idea is similar to COMB (by Luz et. al.). We use two learners:
                 * SIMPLE and KFF. the latter exploits and the former explores the data.
                 * the context switch between the two methods is done randomly using a biased coin.
                 * the bias is dynamically chosen and reflects the `effectivness' of exploration,
                 * which is measured by a distance function between two hypothesis.
                 *
                 * the implemention corresponds the paper:
                 * Thomas Osugi, Deng Kun, and Stephen Scott.
                 * Balancing Exploration and Exploitation: A New Algorithm for Active Machine Learning.
                 * In Proceedings of the Fifth IEEE International Conference on Data Mining. November 2005.
                 */
                learner = new BALANCED_EE();
                break;
        }
    }

    public static Instances InstancesToInstances2(Instances insts) {
        Instances result = new Instances(insts, 0, 0);
        for (int i = 0; i < insts.numInstances(); i++) {
            Instance orig = insts.get(i);
            Instance2 inst2 = new Instance2(orig.weight(), orig.toDoubleArray());
            inst2.setDataset(result);
            result.add(inst2);
        }
        return result;
    }

    public static void main(String[] args) throws Exception {
        System.out.println("teste");
        BufferedReader dataReader = new BufferedReader(new FileReader("/home/davi/wcs/ucipp/uci/banana.arff"));
//        BufferedReader dataReader = new BufferedReader(new FileReader("/home/davi/wcs/ucipp/uci/iris.arff"));
        Instances data = InstancesToInstances2(new Instances(dataReader));
        dataReader.close();
        data.setClassIndex(data.numAttributes() - 1);

        Instances lab = new Instances(data, 0, 500);
        Instances unl = new Instances(data, 500, 500);

//        SVMStrategy s = new SVMStrategy("KFF", lab, unl);
//        SVMStrategy s = new SVMStrategy("SIMPLE", lab, unl);
        SVMStrategy s = new SVMStrategy("SELF_CONF", lab, unl);
//        SVMStrategy s = new SVMStrategy("BALANCED_EE", lab, unl);
        for (int i = 0; i < 100; i++) {
//            System.out.println(
            s.nextQuery();//);
        }
    }

    public Instance2 nextQuery() throws Exception {
        learner.buildClassifier(labeled);
        int ind = learner.instanceToQuery(unlabeled);
        InstanceContainer ins = unlabeled.getInstance(ind);
        unlabeled.deleteInstance(ind);
        System.out.println("idx " + ind + " class:" + ins.classValue());
//        System.out.println("");
        labeled.addInstance(ins);
        return null;
    }
}
