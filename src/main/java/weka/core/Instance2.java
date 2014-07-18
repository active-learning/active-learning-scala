package weka.core;

import java.io.Serializable;
import java.util.Enumeration;

/**
 * Created by davi on 7/17/14.
 */
public class Instance2 extends DenseInstance implements Copyable, Serializable, InstanceContainer {

    /**
     * Constant representing a missing value.
     */
    private final static double MISSING_VALUE = Double.NaN;

    /**
     * The dataset the instance has access to.  Null if the instance
     * doesn't have access to any dataset.  Only if an instance has
     * access to a dataset, it knows about the actual attribute types.
     */
    protected Instances m_Dataset;

    /**
     * The instance's attribute values.
     */
    protected double[] m_AttValues;

    /**
     * The instance's weight.
     */
    protected double m_Weight;

    protected int m_index = -1; // in dataset if exists

    public Instance2(Instance instance) {
        super(instance);
    }
    /************************************************/

    /**
     * Constructor that copies the attribute values and the weight from
     * the given instance. Reference to the dataset is set to null.
     * (ie. the instance doesn't have access to information about the
     * attribute types)
     *
     * @param instance the instance from which the attribute
     * values and the weight are to be copied
     */

    /**
     * Constructor that inititalizes instance variable with given
     * values. Reference to the dataset is set to null. (ie. the instance
     * doesn't have access to information about the attribute types)
     *
     * @param weight    the instance's weight
     * @param attValues a vector of attribute values
     */
    public Instance2(double weight, double[] attValues) {
        super(weight, attValues);
    }

    /**
     * Constructor of an instance that sets weight to one, all values to
     * be missing, and the reference to the dataset to null. (ie. the instance
     * doesn't have access to information about the attribute types)
     *
     * @param numAttributes the size of the instance
     */
    public Instance2(int numAttributes) {
        super(numAttributes);
    }

    /**
     * Tests if the given value codes "missing".
     *
     * @param val the value to be tested
     * @return true if val codes "missing"
     */
    public static boolean isMissingValue(double val) {

        return Double.isNaN(val);
    }

    /**
     * Returns the double that codes "missing".
     *
     * @return the double that codes "missing"
     */
    public static double missingValue() {

        return MISSING_VALUE;
    }

    /**
     * Main method for testing this class.
     */
    public static void main(String[] options) {

        try {

            // Create numeric attributes "length" and "weight"
            Attribute length = new Attribute("length");
            Attribute weight = new Attribute("weight");

            // Create vector to hold nominal values "first", "second", "third"
            FastVector my_nominal_values = new FastVector(3);
            my_nominal_values.addElement("first");
            my_nominal_values.addElement("second");
            my_nominal_values.addElement("third");

            // Create nominal attribute "position"
            Attribute position = new Attribute("position", my_nominal_values);

            // Create vector of the above attributes
            FastVector attributes = new FastVector(3);
            attributes.addElement(length);
            attributes.addElement(weight);
            attributes.addElement(position);

            // Create the empty dataset "race" with above attributes
            Instances race = new Instances("race", attributes, 0);

            // Make position the class attribute
            race.setClassIndex(position.index());

            // Create empty instance with three attribute values
            Instance inst = new Instance2(3);

            // Set instance's values for the attributes "length", "weight", and "position"
            inst.setValue(length, 5.3);
            inst.setValue(weight, 300);
            inst.setValue(position, "first");

            // Set instance's dataset to be the dataset "race"
            inst.setDataset(race);

            // Print the instance
            System.out.println("The instance: " + inst);

            // Print the first attribute
            System.out.println("First attribute: " + inst.attribute(0));

            // Print the class attribute
            System.out.println("Class attribute: " + inst.classAttribute());

            // Print the class index
            System.out.println("Class index: " + inst.classIndex());

            // Say if class is missing
            System.out.println("Class is missing: " + inst.classIsMissing());

            // Print the instance's class value in internal format
            System.out.println("Class value (internal format): " + inst.classValue());

            // Print a shallow copy of this instance
            Instance copy = (Instance) inst.copy();
            System.out.println("Shallow copy: " + copy);

            // Set dataset for shallow copy
            copy.setDataset(inst.dataset());
            System.out.println("Shallow copy with dataset set: " + copy);

            // Unset dataset for copy, delete first attribute, and insert it again
            copy.setDataset(null);
            copy.deleteAttributeAt(0);
            copy.insertAttributeAt(0);
            copy.setDataset(inst.dataset());
            System.out.println("Copy with first attribute deleted and inserted: " + copy);

            // Enumerate attributes (leaving out the class attribute)
            System.out.println("Enumerating attributes (leaving out class):");
            Enumeration enm = inst.enumerateAttributes();
            while (enm.hasMoreElements()) {
                Attribute att = (Attribute) enm.nextElement();
                System.out.println(att);
            }

            // Headers are equivalent?
            System.out.println("Header of original and copy equivalent: " +
                    inst.equalHeaders(copy));

            // Test for missing values
            System.out.println("Length of copy missing: " + copy.isMissing(length));
            System.out.println("Weight of copy missing: " + copy.isMissing(weight.index()));
            System.out.println("Length of copy missing: " +
                    Instance2.isMissingValue(copy.value(length)));
            System.out.println("Missing value coded as: " + Instance2.missingValue());

            // Prints number of attributes and classes
            System.out.println("Number of attributes: " + copy.numAttributes());
            System.out.println("Number of classes: " + copy.numClasses());

            // Replace missing values
            double[] meansAndModes = {2, 3, 0};
            copy.replaceMissingValues(meansAndModes);
            System.out.println("Copy with missing value replaced: " + copy);

            // Setting and getting values and weights
            copy.setClassMissing();
            System.out.println("Copy with missing class: " + copy);
            copy.setClassValue(0);
            System.out.println("Copy with class value set to first value: " + copy);
            copy.setClassValue("third");
            System.out.println("Copy with class value set to \"third\": " + copy);
            copy.setMissing(1);
            System.out.println("Copy with second attribute set to be missing: " + copy);
            copy.setMissing(length);
            System.out.println("Copy with length set to be missing: " + copy);
            copy.setValue(0, 0);
            System.out.println("Copy with first attribute set to 0: " + copy);
            copy.setValue(weight, 1);
            System.out.println("Copy with weight attribute set to 1: " + copy);
            copy.setValue(position, "second");
            System.out.println("Copy with position set to \"second\": " + copy);
            copy.setValue(2, "first");
            System.out.println("Copy with last attribute set to \"first\": " + copy);
            System.out.println("Current weight of instance copy: " + copy.weight());
            copy.setWeight(2);
            System.out.println("Current weight of instance copy (set to 2): " + copy.weight());
            System.out.println("Last value of copy: " + copy.toString(2));
            System.out.println("Value of position for copy: " + copy.toString(position));
            System.out.println("Last value of copy (internal format): " + copy.value(2));
            System.out.println("Value of position for copy (internal format): " +
                    copy.value(position));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Produces a shallow copy of this instance. The copy doesn't have
     * access to a dataset.
     *
     * @return the shallow copy
     */
    public Object copy() {
        return this.makeCopy();
        //return new Instance(this);
    }

    public Instance makeCopy() {
        Instance2 ins = new Instance2(this.m_Weight, this.m_AttValues);
        ins.setIndex(this.m_index);
        ins.setDataset(this.m_Dataset);
        return ins;
    }

    /**
     * Returns the dataset this instance has access to. (ie. obtains
     * information about attribute types from) Null if the instance
     * doesn't have access to a dataset.
     *
     * @return the dataset the instance has accesss to
     */
    public Instances dataset() {

        return m_Dataset;
    }

    /**
     * Tests if a specific value is "missing".
     *
     * @param attIndex the attribute's index
     */
    public boolean isMissing(int attIndex) {

        if (Double.isNaN(m_AttValues[attIndex])) {
            return true;
        }
        return false;
    }

    /**
     * Tests if a specific value is "missing".
     * The given attribute has to belong to a dataset.
     *
     * @param att the attribute
     */
    public boolean isMissing(Attribute att) {

        return isMissing(att.index());
    }

    /**
     * Returns the number of attributes.
     *
     * @return the number of attributes as an integer
     */
    public int numAttributes() {

        return m_AttValues.length;
    }

    public void setValueNoClass(int attIndex, double value) throws Exception {
        if (attIndex < this.classIndex()) {
            this.setValue(attIndex, value);
        } else {
            this.setValue(attIndex + 1, value);
        }
    }

    /**
     * Returns the description of one instance. If the instance
     * doesn't have access to a dataset, it returns the internal
     * floating-point values. Quotes string
     * values that contain whitespace characters.
     *
     * @return the instance's description as a string
     */
    public String toString() {

        StringBuffer text = new StringBuffer();

        for (int i = 0; i < m_AttValues.length; i++) {
            if (i > 0) text.append(",");
            text.append(toString(i));
        }

        return text.toString();
    }

    /**
     * Returns an instance's attribute value in internal format.
     *
     * @param attIndex the attribute's index
     * @return the specified value as a double (If the corresponding
     * attribute is nominal (or a string) then it returns the value's index as a
     * double).
     */
    public double value(int attIndex) {

        return m_AttValues[attIndex];
    }

    public double valueNoClass(int attIndex) throws Exception {
        if (attIndex < this.classIndex()) {
            return this.value(attIndex);
        } else {
            return this.value(attIndex + 1);
        }
    }

    /**
     * Returns an instance's attribute value in internal format.
     * The given attribute has to belong to a dataset.
     *
     * @param att the attribute
     * @return the specified value as a double (If the corresponding
     * attribute is nominal (or a string) then it returns the value's index as a
     * double).
     */
    public double value(Attribute att) {

        return m_AttValues[att.index()];
    }

    /**
     * Clones the attribute vector of the instance and
     * overwrites it with the clone.
     */
    private void freshAttributeVector() {

        double[] newValues;

        newValues = new double[m_AttValues.length];
        System.arraycopy(m_AttValues, 0, newValues, 0,
                m_AttValues.length);
        m_AttValues = newValues;
    }

    public int getIndex() {
        return this.m_index;
    }

    public void setIndex(int indx) {
        this.m_index = indx;
    }

    public Instance toInstance(Instances dataSet) throws Exception {
        Instance copyIns = this.makeCopy();
        if (dataSet.numAttributes() != copyIns.numAttributes()) {
            throw new Exception("number of attributes is different");
        }
        if (dataSet.classIndex() != copyIns.classIndex()) {
            throw new Exception("class index is different");
        }
        copyIns.setDataset(dataSet);
        return copyIns;
    }

    public FastVector ids() throws Exception {
        int i;
        FastVector ids = new FastVector();
        for (i = 0; i < this.m_AttValues.length; i++) {
            if (isMissingValue(this.m_AttValues[i])) {
                throw new Exception("Missing values not suported");
            }
            ids.addElement(new Integer(i));
        }
        return ids;
    }

    public FastVector values() throws Exception {
        int i;
        FastVector vals = new FastVector();
        for (i = 0; i < this.m_AttValues.length; i++) {
            if (isMissingValue(this.m_AttValues[i])) {
                throw new Exception("Missing values not suported");
            }
            vals.addElement(new Double(this.m_AttValues[i]));
        }
        return vals;
    }
}
