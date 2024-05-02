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
package org.ow2.proactive_grid_cloud_portal.common.shared;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.google.gwt.core.client.GWT;
import com.google.gwt.i18n.shared.DateTimeFormat;
import com.google.gwt.i18n.shared.DefaultDateTimeFormatInfo;


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

    public static final String HTTPS_ALLOW_ANY_CERTIFICATE = "web.https.allow_any_certificate";

    public static final String HTTPS_ALLOW_ANY_HOSTNAME = "web.https.allow_any_hostname";

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

    public boolean isHttpsAllowAnyCertificate() {
        return getBooleanValue(HTTPS_ALLOW_ANY_CERTIFICATE, false);
    }

    public boolean isHttpsAllowAnyHostname() {
        return getBooleanValue(HTTPS_ALLOW_ANY_HOSTNAME, false);
    }

    private boolean getBooleanValue(String property, boolean defaultValue) {
        String value = this.properties.get(property);

        if (value == null) {
            return defaultValue;
        }

        return "true".equalsIgnoreCase(value);
    }

    /**
     * @return the currently used Rest URL
     */
    public abstract String getRestUrl();

    /**
     * @return the REST public URL if it has been overridden from properties
     * If the {@link #getRestUrl()} is different than its default value, it will be used as the REST public URL
     */
    protected abstract String getRestPublicUrlIfDefinedOrOverridden();

    /**
     * @return the {@link #getRestPublicUrlIfDefinedOrOverridden()} or guessed from current location if not set
     */
    public String getRestPublicUrlOrGuessRestUrl() {
        String restPublicUrl = getRestPublicUrlIfDefinedOrOverridden();
        if (restPublicUrl == null || restPublicUrl.isEmpty()) {
            return GWT.getHostPageBaseURL().replace("rm/", "").replace("scheduler/", "") + "rest";
        }
        return restPublicUrl;
    }

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

    public String getAboutText(String restUrl) {
        return fillTemplate(properties.get(ABOUT), restUrl);
    }

    private static final String ABOUT = "about";

    private static final String d_ABOUT = "<h3>ProActive @application_name@ Portal</h3>" + "Version: @version@" +
                                          "<br><br>" + "Copyright (C) 2007-" + getCurrentYear() +
                                          ", Activeeon, All rights reserved<br><br>" +
                                          "Visit <a target='_blank' href='http://proactive.inria.fr/'>http://proactive.inria.fr/</a> " +
                                          "and <a target='_blank' href='http://www.activeeon.com/'>http://www.activeeon.com/</a><br>" +
                                          "Contact: +33 (0)9 88 777 660, <a target='_blank' href='mailto:contact@activeeon.com'>contact@activeeon.com</a>" +
                                          "<br><br><br>" + "<table style='color:#404040'>" +
                                          "<tr><td>REST server</td><td>@rest_public_url@</td></tr>" +
                                          "<tr><td>REST version</td><td>@rest_version@</td></tr>" +
                                          "<tr><td> @application_name@ version</td><td>@application_version@</td></tr>" +
                                          "</table>";

    private void setCommonDefaults() {
        properties.put(ABOUT, d_ABOUT);
    }

    private static String getCurrentYear() {
        DefaultDateTimeFormatInfo info = new DefaultDateTimeFormatInfo();
        DateTimeFormat dateTimeFormat = new DateTimeFormat("yyyy", info) {
        };
        return dateTimeFormat.format(new Date());
    }

    private String fillTemplate(String template, String restUrl) {
        return template.replace("@application_name@", getApplicationName())
                       .replace("@version@", getVersion())
                       .replace("@rest_public_url@", restUrl)
                       .replace("@rest_version@", getRestVersion())
                       .replace("@application_version@", getApplicationVersion());
    }
}
