/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.common.client;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;

import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;


public abstract class Controller {

    protected static final String SESSION_SETTING = "pa.session";

    protected static final String LOGIN_SETTING = "pa.login";

    /**
     * @return locally stored data
     */
    public abstract Model getModel();

    /**
     * @return event dispatcher, used to register new listeners
     */
    public abstract EventDispatcher getEventDispatcher();

    /**
     * login has been successfully performed,
     * this method sets the pages & model accordingly.
     * This does NOT perform a server login call
     * 
     * @param login can be null
     */
    public abstract void login(String sessionId, String login);

    /**
     * @return key of the 'login' setting in the local store, used to pre-fill
     *  the login page
     */
    public abstract String getLoginSettingKey();

    /**
     * @return URL of the large application logo
     */
    public abstract String getLogo350Url();

    /**
     * @return URL of the large application logo
     */
    public abstract String getPortalLogo();

    /**
     * Perform server logout,
     * updates the page accordingly
     */
    public abstract void logout();

    /**
     * Parse a JSON string
     * <p>
     * If the input is not valid JSON or parsing fails for some reason,
     * the exception will be logged in the UI but not thrown.
     * 
     * @param jsonStr a valid JSON string
     * @return a java representation of the JSON object hierarchy,
     *     or a JSONObject representing {} if parsing fails
     */
    public static JSONValue parseJSON(String jsonStr) {
        try {
            return JSONParser.parseStrict(jsonStr);
        } catch (Exception e) {
            // only shows up in eclipse dev mode
            e.printStackTrace();

            LogModel.getInstance().logCriticalMessage("JSON Parser failed " + e.getClass().getName() + ": " +
                                                      e.getLocalizedMessage());
            LogModel.getInstance().logCriticalMessage("input was: " + jsonStr);
            return new JSONObject();
        }
    }

}
