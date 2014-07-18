/*
 *    Classifier.java
 *    Copyright (C) 1999 Eibe Frank, Len Trigg
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
package weka.classifiers;

import weka.core.Utils;
import weka.core.Instance;
import weka.core.Instances;
import weka.core.OptionHandler;

import java.io.*;
import java.util.Enumeration;

/**
 * Abstract classifier. All schemes for numeric or nominal prediction in
 * Weka extend this class.
 *
 * @author Eibe Frank (eibe@cs.waikato.ac.nz)
 * @author Len Trigg (trigg@cs.waikato.ac.nz)
 * @version $Revision: 1.4 $
 */
public abstract class Classifier implements Cloneable, Serializable {

    /**
     * Creates copies of the current classifier, which can then
     * be used for boosting etc.
     *
     * @param model an example classifier to copy
     * @param num   the number of classifiers copies to create.
     * @return an array of classifiers.
     * @throws Exception if an error occurs
     */
    public static Classifier[] makeCopies(Classifier model,
                                          int num) throws Exception {

        if (model == null) {
            throw new Exception("No model classifier set");
        }
        Classifier[] classifiers = new Classifier[num];
        String[] options = null;
        if (model instanceof OptionHandler) {
            options = ((OptionHandler) model).getOptions();
        }
        for (int i = 0; i < classifiers.length; i++) {
            classifiers[i] = (Classifier) model.getClass().newInstance();
            if (options != null) {
                String[] tempOptions = (String[]) options.clone();
                ((OptionHandler) classifiers[i]).setOptions(tempOptions);
                Utils.checkForRemainingOptions(tempOptions);
            }
        }
        return classifiers;
    }

    /**
     * Classifies a given instance.
     *
     * @param instance the instance to be classified
     * @return index of the predicted class as a double
     * if the class is nominal, otherwise the predicted value
     * @throws Exception if instance could not be classified
     *                   successfully
     */
    public abstract double classifyInstance(Instance instance) throws Exception;

    public double evaluateErr(Instances insts) {
        int total = insts.numInstances();
        double correct = 0;
        Enumeration enumInsts = insts.enumerateInstances();
        while (enumInsts.hasMoreElements()) {
            Instance inst = (Instance) enumInsts.nextElement();
            double p = 0;
            try {
                p = classifyInstance(inst);
            } catch (Exception e) {
            }
            int class_ = p < 0.5 ? 1 : -1;
            int realClass = inst.value(2) == 0 ? -1 : 1;
            if (class_ != realClass)
                correct++;
        }
        return correct / total;
    }

}

