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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.util.JobColumnsUtil;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;


/**
 * @author ActiveEon Team
 * @since Feb 27, 2017
 */
public class SchedulerPortalDisplayConfig {

    /**
     * Logger
     */
    private static final Logger LOGGER = Logger.getLogger(SchedulerPortalDisplayConfig.class.getName());

    /**
     * Path to the user-defined portal property file
     */
    public static final String CONFIG_PATH = "scheduler-portal-display.conf";

    /**
     * Extra columns property
     */
    private static final String EXTRA_COLUMNS_PROPERTY = "execution-list-extra-columns";

    /**
     * Singleton behaviour
     */
    private static SchedulerPortalDisplayConfig instance = null;

    /**
     * Config properties
     */
    private final Map<String, String> properties = new HashMap<>();

    /**
     * Config properties
     */
    private List<JSONColumn> extraColumns = null;

    /**
     * @return current static config instance, cannot be null
     */
    public static SchedulerPortalDisplayConfig get() {
        if (instance == null) {
            instance = new SchedulerPortalDisplayConfig();
        }
        return instance;
    }

    public List<JSONColumn> getExtraColumns() {

        if (extraColumns == null) {
            JSONArray extraColumnsArray = null;

            String extraColumnsJson = properties.get(EXTRA_COLUMNS_PROPERTY);
            if (extraColumnsJson != null) {
                try {
                    extraColumnsArray = JSONUtils.parseJSON(extraColumnsJson).isArray();
                } catch (JSONException e) {
                    LOGGER.log(Level.SEVERE, e.getMessage());
                }
            }

            if (extraColumnsArray != null) {

                List<JSONColumn> list = new ArrayList<>(extraColumnsArray.size());
                for (int i = 0; i < extraColumnsArray.size(); i++) {
                    JSONObject extraColumnProperties = extraColumnsArray.get(i).isObject();

                    if (extraColumnProperties != null) {
                        JSONColumn column = new JSONColumn();
                        column.setTitle(JobColumnsUtil.getTitle(extraColumnProperties, i));
                        column.setName(JobColumnsUtil.getName(extraColumnProperties, i));
                        column.setHidden(JobColumnsUtil.getHide(extraColumnProperties, i));
                        list.add(column);
                    }
                }
                extraColumns = list;
            } else
                extraColumns = Collections.emptyList();
        }
        return extraColumns;
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

    public static final class JSONColumn {

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
