/*
 * Copyright 2001-2004 The Apache Software Foundation.
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
package org.apache.commons.io.output;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Data written to this stream is forwarded to a stream that has been associated
 * with this thread.
 *
 * @author <a href="mailto:peter@apache.org">Peter Donald</a>
 * @version $Revision: 293039 $ $Date: 2005-10-02 00:00:40 +0100 (Sun, 02 Oct 2005) $
 */
public class DemuxOutputStream
    extends OutputStream
{
    private InheritableThreadLocal m_streams = new InheritableThreadLocal();

    /**
     * Bind the specified stream to the current thread.
     *
     * @param output the stream to bind
     * @return the OutputStream that was previously active
     */
    public OutputStream bindStream( OutputStream output )
    {
        OutputStream stream = getStream();
        m_streams.set( output );
        return stream;
    }

    /**
     * Closes stream associated with current thread.
     *
     * @throws IOException if an error occurs
     */
    public void close()
        throws IOException
    {
        OutputStream output = getStream();
        if( null != output )
        {
            output.close();
        }
    }

    /**
     * Flushes stream associated with current thread.
     *
     * @throws IOException if an error occurs
     */
    public void flush()
        throws IOException
    {
        OutputStream output = getStream();
        if( null != output )
        {
            output.flush();
        }
    }

    /**
     * Writes byte to stream associated with current thread.
     *
     * @param ch the byte to write to stream
     * @throws IOException if an error occurs
     */
    public void write( int ch )
        throws IOException
    {
        OutputStream output = getStream();
        if( null != output )
        {
            output.write( ch );
        }
    }

    /**
     * Utility method to retrieve stream bound to current thread (if any).
     *
     * @return the output stream
     */
    private OutputStream getStream()
    {
        return (OutputStream)m_streams.get();
    }
}
