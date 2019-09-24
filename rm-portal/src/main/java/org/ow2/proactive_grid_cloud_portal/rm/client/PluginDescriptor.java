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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;


/**
 * Describes either an InfrastructureManager or a Policy
 * <p>
 * Used to configure NodeSource creation 
 * 
 * 
 * 
 * @author mschnoor
 *
 */
public class PluginDescriptor {

    private String pluginName;

    private String pluginDescription;

    private List<Field> configurableFields;

    private Map<Integer, String> sectionDescriptions;

    private Map<String, String> meta;

    public static class Field {
        private String name;

        private String value;

        private String description;

        private boolean password;

        private boolean credential;

        private boolean file;

        private boolean textarea;

        private boolean checkbox;

        private boolean dynamic;

        private int sectionSelector;

        private boolean important;

        Field(String name, String value) {
            this(name, value, "");
        }

        Field(String name, String value, String description) {
            this(name, value, description, false, false, false, false, false, false, 0, false);
        }

        public Field(String name, String value, String description, boolean password, boolean credential, boolean file,
                boolean textarea, boolean checkbox, boolean dynamic, int sectionSelector, boolean important) {
            this.name = name;
            this.value = value;
            this.description = description;
            this.password = password;
            this.credential = credential;
            this.file = file;
            this.textarea = textarea;
            this.checkbox = checkbox;
            this.dynamic = dynamic;
            this.sectionSelector = sectionSelector;
            this.important = important;
        }

        public String getName() {
            return name;
        }

        public String getValue() {
            return value;
        }

        public String getDescription() {
            return description;
        }

        public boolean isPassword() {
            return password;
        }

        public boolean isCredential() {
            return credential;
        }

        public boolean isFile() {
            return file;
        }

        public boolean isTextarea() {
            return textarea;
        }

        public boolean isCheckbox() {
            return checkbox;
        }

        public boolean isDynamic() {
            return dynamic;
        }

        public int getSectionSelector() {
            return sectionSelector;
        }

        public boolean isImportant() {
            return important;
        }
    }

    public PluginDescriptor(String pluginName, String pluginDescription, Map<Integer, String> sectionDescriptions,
            Map<String, String> meta) {
        this.pluginDescription = pluginDescription;
        this.pluginName = pluginName;
        this.configurableFields = new ArrayList<>();
        this.sectionDescriptions = sectionDescriptions;
        this.meta = meta;
    }

    public String getPluginName() {
        return pluginName;
    }

    public String getPluginDescription() {
        return pluginDescription;
    }

    public List<Field> getConfigurableFields() {
        return configurableFields;
    }

    public Map<Integer, String> getSectionDescriptions() {
        return sectionDescriptions;
    }

    public Map<String, String> getMeta() {
        return meta;
    }
}
