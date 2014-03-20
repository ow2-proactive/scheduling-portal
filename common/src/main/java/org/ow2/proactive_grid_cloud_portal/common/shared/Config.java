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
package org.ow2.proactive_grid_cloud_portal.common.shared;

import java.util.HashMap;
import java.util.Map;


/**
 * Static config utility
 * <p>
 * Config is read from a file by a server,
 * then sent to the client and user there
 * 
 * 
 * @author mschnoor
 *
 */
public abstract class Config {

    protected Map<String, String> properties = null;

    private Map<String, String> backup = null;

    private static Config instance = null;

    protected Config() {
        this.properties = new HashMap<String, String>();
        this.backup = new HashMap<String, String>();
        instance = this;
        setCommonDefaults();
    }

    /**
     * @return the static Config object containing generic configuration info
     * @throws IllegalStateException config was not created
     */
    public static Config get() {
        if (instance == null)
            throw new IllegalStateException("Config has not been created");
        return instance;
    }

    /**
     * Load a set of properties
     * This set of properties will be reset if {@link #reload()} is called
     * @param props a set of key/value pairs
     */
    public void load(Map<String, String> props) {
        properties.putAll(props);
        backup.putAll(props);
    }

    /**
     * Set a single property
     * @param key
     * @param value
     */
    public void set(String key, String value) {
        properties.put(key, value);
    }

    /**
     * Reset the properties as they were last time {@link #load(Map)} was called
     */
    public void reload() {
        if (backup == null)
            return;
        load(backup);
    }

    /**
     * @return current properties
     */
    public Map<String, String> getProperties() {
        return this.properties;
    }

    /**
     * @return the currently used Rest URL
     */
    public abstract String getRestUrl();

    /**
     * @return the REST public URL defined in properties or {@link #getRestUrl()} if empty or null
     */
    public abstract String getRestPublicUrl();

    /**
     * @return the REST server version string
     */
    public abstract String getRestVersion();

    /**
     * @return the application (scheduler/rm) version string
     */
    public abstract String getApplicationVersion();

    /**
     * @return name of the application, ie "Scheduler"
     */
    public abstract String getApplicationName();

    /**
     * @return version string of the application
     */
    public abstract String getVersion();

    /**
     * @return URL of the service to GET for the MOTD
     */
    public abstract String getMotdUrl();

    public String getAboutText() {
        return fillTemplate(properties.get(ABOUT));
    }

    private static final String ABOUT = "about";
    private static final String d_ABOUT = "<h3>ProActive @application_name@ Portal</h3>" +
            "Version: @version@" +
            "<br><br>" +
            "Copyright (C) 1997-2013 INRIA/University of Nice-Sophia Antipolis/ActiveEon<br><br>" +
            "Visit <a target='_blank' href='http://proactive.inria.fr/'>http://proactive.inria.fr/</a> " +
            "and <a target='_blank' href='http://www.activeeon.com/'>http://www.activeeon.com/</a><br>" +
            "Contact: +33 (0)9 88 777 660, <a target='_blank' href='mailto:contact@activeeon.com'>contact@activeeon.com</a>" +
            "<br><br><br>" +
            "<table style='color:#404040'>" +
            "<tr><td>REST server</td><td>@rest_public_url@</td></tr>" +
            "<tr><td>REST version</td><td>@rest_version@</td></tr>" +
            "<tr><td> @application_name@ version</td><td>@application_version@</td></tr>" +
            "</table>";

    private void setCommonDefaults() {
        properties.put(ABOUT, d_ABOUT);
    }

    private String fillTemplate(String template) {
        return template
                .replace("@application_name@", getApplicationName())
                .replace("@version@", getVersion())
                .replace("@rest_public_url@", getRestPublicUrl())
                .replace("@rest_version@", getRestVersion())
                .replace("@application_version@", getApplicationVersion());
    }
}
