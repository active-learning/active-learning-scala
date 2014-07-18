package svmal.ml.active;

import svmal.weka.core.DataContainer;
import weka.core.FastVector;
import svmal.weka.core.InstanceContainer;
import weka.core.Utils;

import java.util.Random;

/**
 * <p>Title: Active Learning</p>
 * <p/>
 * <p>Description: </p>
 * This implimentation is an SVM-based variant of SELF-CONF.
 * The original SELF-CONF algorithm proposed by Roy and McCallum (2001)
 * bases its probability estimates (and classification)
 * on Naive Bayes calculations.
 * <p/>
 * Probabilistic estimates are obtained in a standard way,
 * using logistic regression.
 * <p/>
 * The code uses random subsampling: On each trial we estimate the
 * `self estimated expected log-loss' for only a random subset of U.
 * The subsample in the first active session trial contains 100 points;
 * on each subsequent trial we decrement the subsample size by one point
 * until we reach a minimum of 10 points, which we keep for the remaining trials.
 * <p/>
 * N. Roy and A. McCallum.
 * Toward optimal active learning through sampling estimation of error reduction.
 * Precedings of ICML-2001 pages 441�448, 2001.
 * <p/>
 * <p/>
 * <p>Copyright: Copyright (c) 2005</p>
 * <p/>
 * <p>Company: Technion </p>
 *
 * @author Ron (based on Kobi's code)
 * @version 1.0
 */
public class SELF_CONF extends XSvmActiveLearner {

    public static final String ID = "SELF_CONF";

    private static final int MIN_SAMPLE_SIZE_DEFAULT = 10;
    private int m_minSampleSize = MIN_SAMPLE_SIZE_DEFAULT;
    private static final int MAX_SAMPLE_SIZE_DEFAULT = 100;
    private int m_maxSampleSize = MAX_SAMPLE_SIZE_DEFAULT;
    private static final int DEC_DEFAULT = 1;
    private int m_dec = DEC_DEFAULT;
    private int m_currSampleSize;
    private long randomSeed = 1234567890;
    private Random m_rndm;


    private DataContainer svmData;

    public SELF_CONF() {
        svmData = null;
        m_currSampleSize = m_maxSampleSize;
        m_rndm = new Random(randomSeed);
    }


    @Override
    public DataContainer create(String fileName) {
        return null;
    }

    /**
     * builds the classifier of this SELF_CONF active learner.
     *
     * @param data DataContainer contains the training instances
     * @throws Exception if failed to build classifier
     */
    public void buildClassifier(DataContainer data) throws Exception {
        super.buildClassifier(data);
        svmData = data;
    }


    /**
     * quering for the class of an instance from the (unlabelled) pool
     *
     * @param pool DataContainer of unlabelled instances
     * @return int the index of instance to query
     * @throws Exception when fails
     */
    public int instanceToQuery(DataContainer pool) throws Exception {

        int i, sampleSz, poolSz = pool.size();
        DataContainer sampledPool;
        int[] indexes;

        if (m_currSampleSize <= m_minSampleSize) {
            sampleSz = m_minSampleSize;
            m_currSampleSize = m_minSampleSize;
        } else {
            sampleSz = m_currSampleSize;
            m_currSampleSize -= m_dec;
        }
        if (poolSz < sampleSz) {
            sampleSz = poolSz;
            sampledPool = pool;
            indexes = new int[sampleSz];
            for (i = 0; i < sampleSz; i++) {
                indexes[i] = i;
            }
        } else {
            indexes = new int[sampleSz];
            sampledPool = this.sampleFromPool(pool,
                    sampleSz,
                    this.m_rndm,
                    indexes);
        }
        System.out.println("Sample size=" + sampleSz);

        int res = this.instanceToQueryErrorReductionMode(sampledPool);
        if (res != -1) {
            return indexes[res];
        }
        throw new Exception("SELF_CONF Failure");
    }

    private int instanceToQueryErrorReductionMode(DataContainer pool) throws
            Exception {
        int i, best = -1, currCls, poolSz = pool.size();
        InstanceContainer ins;
        double currConf, minLoss = Double.MAX_VALUE, currLoss;

        // find the instance with lowest confidence
        for (i = 0; i < poolSz; i++) {
            currLoss = 0.0;
            ins = pool.getInstance(i);
            currConf = Math.abs(getSvm().confindenceOnInstance(ins));
            currConf = this.logisticRegression(currConf);
            currCls = (int) (getSvm().classifyInstance(ins));
            currLoss += (this.expectedLogLoss(ins, currCls, pool) * currConf);
            currCls = ((currCls == 0) ? 1 : 0);
            currConf = 1.0 - currConf;
            currLoss += (this.expectedLogLoss(ins, currCls, pool) * currConf);
            if (Utils.sm(currLoss, minLoss)) {
                minLoss = currLoss;
                best = i;
            }
        }
        return best;
    }

    public double[] distributionToQuery(DataContainer pool) throws Exception {
        int i, newIndx, currCls;
        int sampleSz, poolSz = pool.size();
        InstanceContainer ins;
        DataContainer sampledPool;
        double currConf, currLoss;
        double sum,
                lowestLoss = Double.MAX_VALUE, highestLoss = -Double.MAX_VALUE;
        double[] prob = new double[poolSz];
        int[] indexes;

        System.out.println("error-reduction dist");
        for (i = 0; i < poolSz; i++) {
            prob[i] = 0.0;
        }
        // sample
        if (m_currSampleSize <= m_minSampleSize) {
            sampleSz = m_minSampleSize;
            m_currSampleSize = m_minSampleSize;
        } else {
            sampleSz = m_currSampleSize;
            m_currSampleSize -= m_dec;
        }
        if (poolSz < sampleSz) {
            sampleSz = poolSz;
            sampledPool = pool;
            indexes = new int[sampleSz];
            for (i = 0; i < sampleSz; i++) {
                indexes[i] = i;
            }
        } else {
            indexes = new int[sampleSz];
            sampledPool = this.sampleFromPool(pool,
                    sampleSz,
                    this.m_rndm,
                    indexes);
        }
        System.out.println("Sample size=" + sampleSz);

        // find the instance with lowest confidence
        for (i = 0; i < sampleSz; i++) {
            currLoss = 0.0;
            ins = sampledPool.getInstance(i);
            currConf = Math.abs(getSvm().confindenceOnInstance(ins));
            currConf = this.logisticRegression(currConf);
            currCls = (int) (getSvm().classifyInstance(ins));
            currLoss +=
                    (this.expectedLogLoss(ins, currCls, sampledPool) * currConf);
            currCls = ((currCls == 0) ? 1 : 0);
            currConf = 1.0 - currConf;
            currLoss +=
                    (this.expectedLogLoss(ins, currCls, sampledPool) * currConf);

            newIndx = indexes[i];
            prob[newIndx] = currLoss;
            if (Utils.sm(prob[newIndx], lowestLoss)) {
                lowestLoss = prob[newIndx];
            }
            if (Utils.gr(prob[newIndx], highestLoss)) {
                highestLoss = prob[newIndx];
            }
        }
        for (i = 0; i < sampleSz; i++) {
            newIndx = indexes[i];
            if (Utils.eq(lowestLoss, highestLoss)) {
                prob[newIndx] = 0.5;
            } else {
                prob[newIndx] = (prob[newIndx] - lowestLoss) /
                        (highestLoss - lowestLoss);
                if (Utils.sm(prob[newIndx], 0.0)) {
                    prob[newIndx] = 0.0;
                }
                if (Utils.gr(prob[newIndx], 1.0)) {
                    prob[newIndx] = 1.0;
                }
            }
        }
        // scale up
        double scale = 100;
        for (i = 0; i < sampleSz; i++) {
            newIndx = indexes[i];
            prob[newIndx] *= scale;
        }
        // exponential scale
        sum = 0.0;
        for (i = 0; i < sampleSz; i++) {
            newIndx = indexes[i];
            prob[newIndx] = Math.exp(-1.0 * prob[newIndx]);
            sum += prob[newIndx];
        }
        for (i = 0; i < sampleSz; i++) {
            newIndx = indexes[i];
            prob[newIndx] = prob[newIndx] / sum;
        }
        for (i = 0; i < sampleSz; i++) {
            newIndx = indexes[i];
            if (Utils.grOrEq(prob[newIndx], 0.01)) {
                System.out.println("prob[" + newIndx + "]=" + prob[newIndx]);
            }
        }
        // check
        sum = 0.0;
        for (i = 0; i < poolSz; i++) {
            sum += prob[i];
        }
        if (!Utils.eq(sum, 1.0)) {
            throw new Exception("sum probs is not 1.0 - sum=" + sum);
        }
        return prob;
    }

    private double expectedLogLoss(InstanceContainer insToAdd, int cls,
                                   DataContainer pool) throws Exception {
        int i, poolSz = pool.size();
        double currConf, sum;
        InstanceContainer ins;

        insToAdd.setClassValue(cls);
        svmData.addInstance(insToAdd);
        getSvm().buildClassifier(svmData);
        sum = 0.0;
        for (i = 0; i < poolSz; i++) {
            ins = pool.getInstance(i);
            currConf = Math.abs(getSvm().confindenceOnInstance(ins));
            currConf = this.logisticRegression(currConf);
            if (!Utils.eq(currConf, 0)) {
                sum += (currConf * Math.log(currConf));
            }
            currConf = 1.0 - currConf;
            if (!Utils.eq(currConf, 0)) {
                sum += (currConf * Math.log(currConf));
            }
            if (Utils.gr(sum, 0.0)) {
                throw new Exception("sum is positive");
            }
        }
        sum /= ((double) poolSz);
        sum = Math.abs(sum);
        svmData.deleteInstance(svmData.size() - 1);
        getSvm().buildClassifier(svmData);
        return sum;
    }

    private double logisticRegression(double conf) throws Exception {
        if (Utils.sm(conf, 0.0)) {
            throw new Exception("conf is negative" + conf);
        }
        double res = (1.0 / (1.0 + Math.exp(-1.0 * conf)));
        if (Utils.sm(res, 0.5) || Utils.gr(res, 1.0)) {
            throw new Exception("logic reg is not in [0.5, 1]");
        }
        return res;
    }

    private DataContainer sampleFromPool(DataContainer pool,
                                         int sampleSize,
                                         Random rnd,
                                         int[] indexes) {
        int i, indx1, indx2, poolSz = pool.size();
        DataContainer sample = pool.makeEmptyCopy();
        InstanceContainer ins;
        FastVector chooseFrom = new FastVector();

        for (i = 0; i < poolSz; i++) {
            chooseFrom.addElement(new Integer(i));
        }
        for (i = 0; i < sampleSize; i++) {
            indx1 = (int) (Math.ceil(chooseFrom.size() * rnd.nextDouble())) - 1;
            indx2 = ((Integer) (chooseFrom.elementAt(indx1))).intValue();
            chooseFrom.removeElementAt(indx1);
            ins = pool.getInstance(indx2);
            sample.addInstance(ins);
            if (indexes != null) {
                indexes[i] = indx2;
            }
        }
        return sample;
    }
}
