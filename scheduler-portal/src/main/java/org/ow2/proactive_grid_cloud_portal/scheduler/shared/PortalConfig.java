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
package org.ow2.proactive_grid_cloud_portal.scheduler.shared;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONBoolean;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONString;


/**
 * @author ActiveEon Team
 * @since Feb 27, 2017
 */
public class PortalConfig {

    /**
     * Logger
     */
    private static final Logger LOGGER = Logger.getLogger(PortalConfig.class.getName());

    /**
     * Path to the user-defined portal property file
     */
    public static final String CONFIG_PATH = "scheduler-portal.conf";

    /**
     * Extra columns property
     */
    private static final String EXTRA_COLUMNS_PROPERTY = "execution-list-extra-columns";

    /**
     * Singleton behaviour
     */
    private static PortalConfig instance = null;

    /**
     * Config properties
     */
    private Map<String, String> properties = new HashMap<>();

    /**
     * Config properties
     */
    private List<JSONColumn> extraColumns = null;

    /**
     * @return current static config instance, cannot be null
     */
    public static PortalConfig get() {
        if (instance == null) {
            instance = new PortalConfig();
        }
        return instance;
    }

    public List<JSONColumn> getExtraColumns() {

        if (extraColumns == null) {
            String extraColumnsJson = properties.get(EXTRA_COLUMNS_PROPERTY);
            List<JSONColumn> list = new ArrayList<>();
            if (extraColumnsJson != null) {
                JSONArray extraColumnsArray = null;
                try {
                    extraColumnsArray = JSONUtils.parseJSON(extraColumnsJson).isArray();
                } catch (JSONException e) {
                    LOGGER.log(Level.SEVERE, e.getMessage());
                }

                if (extraColumnsArray != null) {
                    for (int i = 0; i < extraColumnsArray.size(); i++) {
                        JSONObject extraColumnProperties = extraColumnsArray.get(i).isObject();

                        if (extraColumnProperties != null) {
                            JSONColumn column = new JSONColumn();
                            column.setTitle(getTitle(extraColumnProperties, i));
                            column.setName(getName(extraColumnProperties, i));
                            column.setHidden(getHiddenDefault(extraColumnProperties, i));
                            list.add(column);
                        }
                    }
                }
            }
            extraColumns = list;
        }
        return extraColumns;
    }

    private String getTitle(JSONObject extraColumnProperties, int columnIndex) {
        JSONString title = extraColumnProperties.get("title").isString();
        if (title != null)
            return title.stringValue();
        else {
            LOGGER.log(Level.SEVERE, "Could not read the field \"title\" of column number " + columnIndex);
            return new String();
        }
    }

    private String getName(JSONObject extraColumnProperties, int columnIndex) {

        JSONObject information = extraColumnProperties.get("information").isObject();
        if (information != null) {
            String type = getType(information, columnIndex);
            String key = getKey(information, columnIndex);
            return type + "-" + key;
        } else {
            LOGGER.log(Level.SEVERE, "Could not read the field \"information\" of column number " + columnIndex);
            return new String();
        }
    }

    private String getType(JSONObject information, int columnIndex) {
        JSONString typeJson = information.get("type").isString();
        if (typeJson != null)
            return typeJson.stringValue();
        else {
            LOGGER.log(Level.SEVERE,
                       "Could not read the field \"information\"->\"type\" of column number " + columnIndex);
            return new String();
        }
    }

    private String getKey(JSONObject information, int columnIndex) {
        JSONString keyJson = information.get("key").isString();
        if (keyJson != null)
            return keyJson.stringValue();
        else {
            LOGGER.log(Level.SEVERE,
                       "Could not read the field \"information\"->\"key\" of column number " + columnIndex);
            return new String();
        }
    }

    private boolean getHiddenDefault(JSONObject extraColumnProperties, int columnIndex) {

        JSONBoolean hidden = extraColumnProperties.get("hidden-default").isBoolean();
        if (hidden != null)
            return hidden.booleanValue();
        else {
            LOGGER.log(Level.SEVERE, "Could not read the field \"hidden-default\" of column number " + columnIndex);
            return false;
        }
    }

    /**
     * @return the config properties
     */
    public Map<String, String> getProperties() {
        return properties;
    }

    /**
     * @return the config properties
     */
    public void load(Map<String, String> properties) {
        this.properties.putAll(properties);
    }

    public static class JSONColumn {

        private String title;

        private String name;

        private boolean hidden;

        public JSONColumn() {
        }

        public String getTitle() {
            return title;
        }

        public void setTitle(String title) {
            this.title = title;
        }

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public boolean isHidden() {
            return hidden;
        }

        public void setHidden(boolean hidden) {
            this.hidden = hidden;
        }
    }
}
