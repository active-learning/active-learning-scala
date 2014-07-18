package svm.ml.active;

import weka.active.ActiveLearner;
import weka.core.DataContainer;
import weka.core.Instances;


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
public abstract class XActiveLearner extends ActiveLearner {
//    abstract public DataContainer create(Instances2 instances) throws Exception;

    /* This is a quick-and-dirty solution
    * the original imp. took the sacling info (min max values) from former
    * a better solution would perform a scalling on the whole original data
    * before splitting it to CV folds
    */
    abstract public DataContainer create(Instances data, DataContainer former) throws
            Exception;

    /**
     * initiate this XActiveLearner with the initialData
     *
     * @param initialData DataContainer should contain at least one
     *                    instance per class
     * @throws Exception
     */
    abstract public void init(DataContainer initialData) throws Exception;

    abstract public DataContainer create(String fileName);

    abstract public DataContainer create(double[][] attributes, double[] labels) throws
            Exception;
}
