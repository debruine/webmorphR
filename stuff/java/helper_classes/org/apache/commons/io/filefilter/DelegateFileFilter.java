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
import java.io.FileFilter;
import java.io.FilenameFilter;

/**
 * This class turns a Java FileFilter or FilenameFilter into an IO FileFilter.
 * 
 * @since Commons IO 1.0
 * @version $Revision: 155419 $ $Date: 2005-02-26 13:02:41 +0000 (Sat, 26 Feb 2005) $
 * 
 * @author Henri Yandell
 * @author Stephen Colebourne
 */
public class DelegateFileFilter extends AbstractFileFilter {

    /** The Filename filter */
    private FilenameFilter filenameFilter;
    /** The File filter */
    private FileFilter fileFilter;

    /**
     * Constructs a delegate file filter around an existing FilenameFilter.
     * 
     * @param filter  the filter to decorate
     */
    public DelegateFileFilter(FilenameFilter filter) {
        if (filter == null) {
            throw new IllegalArgumentException("The FilenameFilter must not be null");
        }
        this.filenameFilter = filter;
    }

    /**
     * Constructs a delegate file filter around an existing FileFilter.
     * 
     * @param filter  the filter to decorate
     */
    public DelegateFileFilter(FileFilter filter) {
        if (filter == null) {
            throw new IllegalArgumentException("The FileFilter must not be null");
        }
        this.fileFilter = filter;
    }

    /**
     * Checks the filter.
     * 
     * @param file  the file to check
     * @return true if the filter matches
     */
    public boolean accept(File file) {
        if (fileFilter != null) {
            return fileFilter.accept(file);
        } else {
            return super.accept(file);
        }
    }

    /**
     * Checks the filter.
     * 
     * @param dir  the directory
     * @param name  the filename in the directory
     * @return true if the filter matches
     */
    public boolean accept(File dir, String name) {
        if (filenameFilter != null) {
            return filenameFilter.accept(dir, name);
        } else {
            return super.accept(dir, name);
        }
    }
    
}
