/*
 * Copyright 2002-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.io.filefilter;

import java.io.File;

/**
 * An abstract class which implements the Java FileFilter and FilenameFilter 
 * interfaces via the IOFileFilter interface.
 * <p>
 * Note that a subclass <b>must</b> override one of the accept methods,
 * otherwise your class will infinitely loop.
 *
 * @since Commons IO 1.0
 * @version $Revision: 155419 $ $Date: 2005-02-26 13:02:41 +0000 (Sat, 26 Feb 2005) $
 * 
 * @author Henri Yandell
 * @author Stephen Colebourne
 */
public abstract class AbstractFileFilter implements IOFileFilter {

    /**
     * Checks to see if the File should be accepted by this filter.
     * 
     * @param file  the File to check
     * @return true if this file matches the test
     */
    public boolean accept(File file) {
        return accept(file.getParentFile(), file.getName());
    }

    /**
     * Checks to see if the File should be accepted by this filter.
     * 
     * @param dir  the directory File to check
     * @param name  the filename within the directory to check
     * @return true if this file matches the test
     */
    public boolean accept(File dir, String name) {
        return accept(new File(dir, name));
    }

}
