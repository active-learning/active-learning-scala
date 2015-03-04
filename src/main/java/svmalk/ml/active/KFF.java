package svmalk.ml.active;

import svmalk.weka.core.DataContainer;
import svmalk.weka.core.InstanceContainer;
import weka.core.Utils;


/**
 * <p>Title: Active Learning</p>
 * <p/>
 * <p>Description: </p>
 * <p/>
 * <p>Copyright: Copyright (c) 2005</p>
 * <p/>
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
public class KFF extends XSvmActiveLearner {

    public static final String ID = "KFF";

    private DataContainer svmData;

    public KFF() {
        svmData = null;
    }

    /**
     * instanceToQuery
     *
     * @param pool DataContainer
     * @return int
     * @throws Exception
     * @todo Implement this ActiveLearner method
     */
    public int instanceToQuery(DataContainer pool) throws Exception {
        int index = -1;
        InstanceContainer poolInst = null;
        double distFromSet, currentMaxDist = Double.MIN_VALUE;

        for (int pInd = 0, pSize = pool.size(); pInd < pSize; ++pInd) {
            poolInst = pool.getInstance(pInd);

            distFromSet = distFromSet(poolInst, svmData);
            if (distFromSet > currentMaxDist) {
                index = pInd;
                currentMaxDist = distFromSet;
            }
        }

        if (index == -1) {
            throw new Exception("KFF query failed");
        }

        return index;
    }


    /**
     * @param data DataContainer
     * @throws Exception
     * @see XSvmActiveLearner
     */
    public void buildClassifier(DataContainer data) throws Exception {
        super.buildClassifier(data);
        svmData = data;
    }

    protected DataContainer getSvmData() {
        return svmData;
    }

    private double distFromSet(InstanceContainer ins,
                               DataContainer setOfInst) throws Exception {
        double minDist = Double.MAX_VALUE;
        double currDist;
        InstanceContainer instInSet;

        for (int i = 0; i < setOfInst.size(); ++i) {
            instInSet = setOfInst.getInstance(i);
            currDist = getSvm().kernelDistance(ins, instInSet);

            if (Utils.sm(currDist, minDist)) {
                minDist = currDist;
            }
        }
        return minDist;
    }

}
