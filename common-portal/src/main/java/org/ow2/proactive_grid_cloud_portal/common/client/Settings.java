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

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;

import com.google.gwt.storage.client.Storage;


/**
 * Persistent global key-value store using HTML5 storage
 * <p>
 * Some older browsers may not be compatible
 * in that case, they will neither store nor retrieve values,
 * but should not get a runtime exception. 
 * 
 * 
 * 
 * @author mschnoor
 *
 */
public class Settings {

    private static Settings instance = null;

    public static Settings get() {
        if (instance == null) {
            Settings.instance = new Settings();
        }
        return Settings.instance;
    }

    /**
     * Load settings by reading persistent storage and loading values into the global Config
     * Config must have been created before this can be called
     */
    public static void load() {
        for (String key : Config.get().getProperties().keySet()) {
            String val = Settings.get().getSetting(key);
            if (val != null && val.trim().length() > 0) {
                Config.get().set(key, val);
            }
        }
    }

    private Storage store = null;

    public Settings() {
        this.store = Storage.getLocalStorageIfSupported();
    }

    /**
     * @param key
     * @return the associated value if HTML5 Storage is supported, or null
     * @see com.google.gwt.storage.client.Storage
     */
    public String getSetting(String key) {
        if (this.store != null)
            return store.getItem(key);
        return null;
    }

    /**
     * Store a value in a global persistent storage
     * Does nothing if HTML5 Storage is not supported
     * @param key
     * @param value
     * @see com.google.gwt.storage.client.Storage
     */
    public void setSetting(String key, String value) {
        if (this.store != null) {
            this.store.setItem(key, value);
        }
    }

    /**
     * Clear the value associated with this key from the persistent storage
     * @param key
     */
    public void clearSetting(String key) {
        if (this.store != null) {
            this.store.removeItem(key);
        }
    }

}
