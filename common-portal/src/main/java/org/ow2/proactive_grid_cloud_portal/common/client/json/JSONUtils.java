/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.common.client.json;

import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;

/**
 * JSON utils to parse JSON responses from the server.
 * @author the activeeon team.
 *
 */
public class JSONUtils {

    //Repeated code from Controller. The code using ParseJSON in controller will be progressively 
    //refactored to use this new method and parseJSON in controller will then be removed.
    /**
     * Parse a JSON string. 
     * <p>
     * If the input is not valid JSON or parsing fails for some reason,
     * the exception will be logged in the UI but not thrown.
     * 
     * @param jsonStr a valid JSON string
     * @return a java representation of the JSON object hierarchy,
     * @throws JSONException if it fails to parse the JSONN
     */
    public static JSONValue parseJSON(String jsonStr) throws JSONException{
        try {
            return JSONParser.parseStrict(jsonStr);
        } catch (Throwable t) {
            String message = "JSON Parser failed " + t.getClass().getName() + ": " + t.getLocalizedMessage() 
                    + "\ninput was: " + jsonStr;
            throw new JSONException(message, t);
        }
    }
}
