package svmal.weka.core;

import weka.core.Instances;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;

/**
 * Created by davi on 7/17/14.
 */
public class Instances2 extends Instances {
    protected ArrayList<Instance2> m_Instances;

    public Instances2(/* @non_null@ */Instances2 source, int first, int toCopy) {
        super(source, first, toCopy);
    }

    public Instances2(Reader reader) throws IOException {
        super(reader);
    }

    public Instances2(Instances dataset, int capacity) {
        super(dataset, capacity);
    }

    public final void addNoCopy(Instance2 instance) {
        m_Instances.add(instance);
    }

    public Instance2 instance(int index) {
        return m_Instances.get(index);
    }
}
