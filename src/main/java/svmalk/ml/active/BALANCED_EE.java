package svmalk.ml.active;

import svmalk.weka.core.DataContainer;
import svmalk.weka.core.InstanceContainer;
import weka.core.Utils;

import java.util.Random;

/**
 * <p>Title: Active Learning</p>
 * <p/>
 * <p>Description:
 * <p/>
 * balancing between exploration and exploitation. The idea
 * is similar to COMB (by Luz et. al.). We use two learners:
 * SIMPLE and KFF. the latter exploits and the former explores the data.
 * the context switch between the two methods is done
 * randomly using a biased coin. the biase
 * is dynamically chosen and reflects the `effectivness'
 * of exploration, which is measured by a distance function between two
 * hypothesis.
 * <p/>
 * the implemention corresponds the paper:
 * Thomas Osugi, Deng Kun, and Stephen Scott.
 * Balancing Exploration and Exploitation: A New Algorithm for Active Machine Learning.
 * In Proceedings of the Fifth IEEE International Conference on Data Mining. November 2005.
 * <p/>
 * </p>
 * <p/>
 * <p>Copyright: Copyright (c) 2005</p>
 * <p/>
 * <p>Company: </p>
 *
 * @author not attributable
 * @version 1.0
 */
/*
 * For efficiency this BALACED_EE extends KFF and provides an
 * inline implemention of the query of SIMPLE
 */
public class BALANCED_EE extends KFF {
    public static final String ID = "BALANCED_EE";

    // defaults from Osugi et.al.
    private static final double MIN_PROB_FOR_EXPLORATION_DEFAULT = 0.01;
    private double minProbabilityForExploration =
            MIN_PROB_FOR_EXPLORATION_DEFAULT;
    private static final double LEARNING_RATE_DEFAULT = 0.1;
    private double learningRate = LEARNING_RATE_DEFAULT;
    private static final long DEFAULT_RANDOM_SEED = 123456789;
    private long randomSeed = DEFAULT_RANDOM_SEED;
    private double probabilityForExploration;
    private Random rand;

    private boolean isLastQueryWasExploration;
    private double[] lastExplorationHypoVector;
    private DataContainer allData;

    public BALANCED_EE() {
        super();
        init();
        rand = new Random(randomSeed);
    }

    private void init() {
        probabilityForExploration = 1.0; //start with exploration
        isLastQueryWasExploration = false;
        lastExplorationHypoVector = null;
        allData = null;
    }


    public void init(DataContainer initialData) throws Exception {
        init();
        super.init(initialData);
    }

    //<minimal_odds_for_exploration>0.01< / minimal_odds_for_exploration>
    //        <learning_rate>0.1<learning_rate>
//    public void init(ReadOnlyConfiguration config) throws
//            CreationException {
//        super.init(config);
//
//        String sVal;
//        try {
//            sVal = config.get("balanced/minimal_odds_for_exploration");
//            minProbabilityForExploration = Double.parseDouble(sVal);
//        } catch (Exception e) {
//            throw new CreationException(
//                    "Failed to parse minimal odds for exploration");
//        }
//
//        try {
//            sVal = config.get("balanced/learning_rate");
//            learningRate = Double.parseDouble(sVal);
//        } catch (Exception e) {
//            throw new CreationException(
//                    "Failed to parse learning_rate");
//        }
//
//        try {
//            sVal = config.get("balanced/randomSeed");
//            randomSeed = Long.parseLong(sVal);
//            rand = new Random(randomSeed);
//        } catch (Exception e) {
//            throw new CreationException(
//                    "Failed to parse randomSeed");
//        }
//
//    }


    /**
     * @param data DataContainer
     * @throws Exception
     * @see svmalk.ml.active.XSvmActiveLearner
     */
    public void buildClassifier(DataContainer data) throws Exception {
        super.buildClassifier(data);
        if (isLastQueryWasExploration) {
            updateProbabilityForExploration();
        }
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

        if (allData == null) {
            createAllData(pool);
        }

        if (isDoExploration()) {
//            System.out.println(" Exploration");
            index = explorationQuery(pool);
            lastExplorationHypoVector = creatHypothesisVector();
            isLastQueryWasExploration = true;
        } else {
//            System.out.println(" Exploitation");
            index = exploitationQuery(pool);
            isLastQueryWasExploration = false;
        }

        return index;
    }

    /**
     * createAllData
     *
     * @param pool DataContainer
     */
    private void createAllData(DataContainer pool) {
        allData = pool.makeEmptyCopy();
        for (int i = 0, size = pool.size(); i < size; ++i) {
            allData.addInstance(pool.getInstance(i));
        }
        for (int i = 0, size = super.getSvmData().size(); i < size; ++i) {
            allData.addInstance(super.getSvmData().getInstance(i));
        }
    }

    /**
     * creatHypothesisVector
     */
    private double[] creatHypothesisVector() throws Exception {
        double[] vect = new double[allData.size()];

        for (int i = 0, size = vect.length; i < size; ++i) {
            vect[i] = classifyInstance(allData.getInstance(i));
        }

        return vect;
    }

    private int explorationQuery(DataContainer pool) throws Exception {
        return super.instanceToQuery(pool);
    }

    private int exploitationQuery(DataContainer pool) throws Exception {
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

    private boolean isDoExploration() {
        double randValue = rand.nextDouble(); //flip coin
        return (randValue <= probabilityForExploration);
    }

    /**
     * updateProbabilityForExploration
     */
    private void updateProbabilityForExploration() throws Exception {
        double h_dist = hypoDistance(lastExplorationHypoVector,
                creatHypothesisVector());
        if (h_dist < 0) {
            probabilityForExploration =
                    Math.max(minProbabilityForExploration,
                            Math.min(1 - minProbabilityForExploration,
                                    probabilityForExploration *
                                            learningRate *
                                            Math.exp(h_dist)
                            )
                    );
//            System.out.println(" reducing exploration probability to: " +                    probabilityForExploration);
        } else {
//            System.out.println(                    " Exploration was successful (no probability reduction)");
        }
    }


    /**
     * using d1 distance function defined by Osugi et.al.
     *
     * @param h1 double[]
     * @param h2 double[]
     * @return double
     */
    private double hypoDistance(double[] h1, double[] h2) {
        double innerProduct = 0.0, h1Len = 0.0, h2Len = 0.0;

        //  Euclidean space
        for (int i = 0; i < h1.length; ++i) {
            innerProduct += h1[i] * h2[i];
            h1Len += h1[i] * h1[i];
            h2Len += h2[i] * h2[i];
        }

        h1Len = Math.sqrt(h1Len);
        h2Len = Math.sqrt(h2Len);

        double s1 = innerProduct / (h1Len * h2Len);

        return 3 - 4 * s1;
    }

}
