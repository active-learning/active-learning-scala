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
public class SIMPLE extends XSvmActiveLearner {
    public static final String ID = "SIMPLE";

    public SIMPLE() {
        super();
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
        int i, closest = -1;
        int size = pool.size();
        InstanceContainer ins;
        double minConf = Double.MAX_VALUE, currConf;

        // find the instance with lowest confidence
        for (i = 0; i < size; i++) {
            ins = pool.getInstance(i);
            currConf = Math.abs(getSvm().confindenceOnInstance(ins));
            if (Utils.smOrEq(currConf, minConf)) {
                minConf = currConf;
                closest = i;
            }
        }
        return closest;
    }
}
