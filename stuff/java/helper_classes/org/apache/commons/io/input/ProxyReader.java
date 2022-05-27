/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
package org.apache.commons.io.input;

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;

/**
 * A Proxy stream which acts as expected, that is it passes the method 
 * calls on to the proxied stream and doesn't change which methods are 
 * being called. 
 * <p>
 * It is an alternative base class to FilterReader
 * to increase reusability, because FilterReader changes the 
 * methods being called, such as read(char[]) to read(char[], int, int).
 * 
 * @author Henri Yandell
 * @author Stephen Colebourne
 * @version $Id: ProxyReader.java 155419 2005-02-26 13:02:41Z dirkv $
 */
public abstract class ProxyReader extends FilterReader {

    /**
     * Constructs a new ProxyReader.
     * 
     * @param proxy  the Reader to delegate to
     */
    public ProxyReader(Reader proxy) {
        super(proxy);
        // the proxy is stored in a protected superclass variable named 'in'
    }

    /** @see java.io.Reader#read() */
    public int read() throws IOException {
        return in.read();
    }

    /** @see java.io.Reader#read(char[]) */
    public int read(char[] chr) throws IOException {
        return in.read(chr);
    }

    /** @see java.io.Reader#read(char[], int, int) */
    public int read(char[] chr, int st, int end) throws IOException {
        return in.read(chr, st, end);
    }

    /** @see java.io.Reader#skip(long) */
    public long skip(long ln) throws IOException {
        return in.skip(ln);
    }

    /** @see java.io.Reader#ready() */
    public boolean ready() throws IOException {
        return in.ready();
    }

    /** @see java.io.Reader#close() */
    public void close() throws IOException {
        in.close();
    }

    /** @see java.io.Reader#mark(int) */
    public synchronized void mark(int idx) throws IOException {
        in.mark(idx);
    }

    /** @see java.io.Reader#reset() */
    public synchronized void reset() throws IOException {
        in.reset();
    }

    /** @see java.io.Reader#markSupported() */
    public boolean markSupported() {
        return in.markSupported();
    }

}
