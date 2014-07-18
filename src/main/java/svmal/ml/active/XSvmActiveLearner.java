package svmal.ml.active;

import svmal.libsvm.SvmLib;
import svmal.weka.core.DataContainer;
import svmal.weka.core.InstanceContainer;
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
abstract public class XSvmActiveLearner extends XActiveLearner {

    private SvmLib svm;

    public XSvmActiveLearner() {
        svm = new SvmLib();
    }

    /**
     * buildClassifier
     *
     * @param data DataContainer
     * @throws Exception
     * @todo Implement this ActiveLearner method
     */
    public void buildClassifier(DataContainer data) throws Exception {
        svm.buildClassifier(data);
    }

    /**
     * classifyInstance
     *
     * @param instance InstanceContainer
     * @return double
     * @throws Exception
     * @todo Implement this ActiveLearner method
     */
    public double classifyInstance(InstanceContainer instance) throws Exception {
        return svm.classifyInstance(instance);
    }

    /**
     * setDataAndPool
     *
     * @param data Instances
     * @param pool DataContainer
     * @throws Exception
     * @todo Implement this ActiveLearner method
     */
    public void setDataAndPool(Instances data, DataContainer pool) throws
            Exception {
        throw new RuntimeException("method not supported");
    }

    /**
     * startActiveLearningSession
     *
     * @throws Exception
     * @todo Implement this ActiveLearner method
     */
    public void startActiveLearningSession() throws Exception {
        throw new RuntimeException("method not supported");
    }

//        public void startActiveLearningSession() throws Exception
//    {
//        this.m_classifierExist = false;
//        this.m_firstClassSeen = NO_CLASS;
//
//        this.m_data = null;
//        this.m_wasQueried = null;
//        this.m_trainingSet = null;
//        this.m_fullTrainingSet = null;
//        this.m_pool = null;
//        this.m_holdOut = new FastVector();
//        // random
//        if (this.m_rndm == null) {
//            this.m_rndm = new Random(m_seed);
//        }
//        // progress
//        if (m_selfTestInfo == null) {
//            m_selfTestInfo = new FastVector();
//        }
//        if (this.m_mode == MODE_CEM_PROOF) {
//            if (m_selfTestPosRatio == null) {
//                m_selfTestPosRatio = new FastVector();
//            }
//            if (m_selfTestActualPosRatio == null) {
//                m_selfTestActualPosRatio = new FastVector();
//            }
//            if (m_selfTestSVM == null) {
//                m_selfTestSVM = new FastVector();
//            }
//        }
//        this.m_lastEntropyRes = 0;
//        this.m_previousEntropyRes = 0;
//        this.m_lastFullEntropyRes = 0;
//        this.m_previousFullEntropyRes = 0;
//    }

    public void init(DataContainer initialData) throws Exception {
        buildClassifier(initialData);
    }

    /**
     * creates DataContainer corresponding `data'. the scaling of the values
     * corresponds the min-max values of `former'
     *
     * @param data   Instances for DataContainer
     * @param former DataContainer determines the scaling min-max values
     * @return DataContainer
     * @throws Exception
     */
    public DataContainer create(Instances data, DataContainer former) throws
            Exception {
        return svm.convertData(data, former);
    }


    /**
     * Creates a DataContainer corresponding the data in file `fileName'.
     *
     * @param fileName String
     * @return DataContainer
     */
    public DataContainer create(String fileName) {
//        Instances instances = new Instances2(fileName);
        return null; //svm.convertData(instances);
    }

    /**
     * Creates a DataContainer corresponding the data in file `fileName'.
     *
     * @param attributes double[][]
     * @param labels     double[]
     * @return DataContainer
     * @throws Exception
     */
    public DataContainer create(double[][] attributes, double[] labels) throws
            Exception {
        return svm.convertData(attributes, labels);
    }

    protected SvmLib getSvm() {
        return this.svm;
    }


}
