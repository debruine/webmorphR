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
 * This filter produces a logical NOT of the filters specified.
 *
 * @since Commons IO 1.0
 * @version $Revision: 155419 $ $Date: 2005-02-26 13:02:41 +0000 (Sat, 26 Feb 2005) $
 * 
 * @author Stephen Colebourne
 */
public class NotFileFilter extends AbstractFileFilter {
    
    /** The filter */
    private IOFileFilter filter;

    /**
     * Constructs a new file filter that NOTs the result of another filters.
     * 
     * @param filter  the filter, must not be null
     * @throws IllegalArgumentException if the filter is null
     */
    public NotFileFilter(IOFileFilter filter) {
        if (filter == null) {
            throw new IllegalArgumentException("The filter must not be null");
        }
        this.filter = filter;
    }

    /**
     * Checks to see if both filters are true.
     * 
     * @param file  the File to check
     * @return true if the filter returns false
     */
    public boolean accept(File file) {
        return ! filter.accept(file);
    }
    
    /**
     * Checks to see if both filters are true.
     * 
     * @param file  the File directory
     * @param name  the filename
     * @return true if the filter returns false
     */
    public boolean accept(File file, String name) {
        return ! filter.accept(file, name);
    }
    
}
