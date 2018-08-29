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
package org.ow2.proactive_grid_cloud_portal.rm.shared;

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;


/**
 * RM specific configuration
 *
 * @author mschnoor
 */
public class RMConfig extends Config {

    /**
     * Path to the user-defined RM property file, relative
     * to the webapp file path
     */
    public static final String CONFIG_PATH = "rm.conf";

    /** URL of the REST service */
    public static final String REST_URL = "rm.rest.url";

    private static final String d_REST_URL = "http://localhost:8080/rest";

    public static final String REST_PUBLIC_URL = "rm.rest.public.url";

    public static final String RM_URL = "rm.url";

    private static final String d_RM_URL = "pnp://localhost:64738";

    /** refresh rate in millis */
    public static final String CLIENT_REFRESH_TIME = "rm.client.refresh.time";

    private static final String d_CLIENT_REFRESH_TIME = "3000";

    public static final String CLIENT_BURST_REFRESH_TIME = "rm.client.burst.refresh.time";

    private static final String d_CLIENT_BURST_REFRESH_TIME = "10";

    /** release version string */
    public static final String VERSION = "rm.version";

    private static final String d_VERSION = "0.0";

    /** REST version string */
    public static final String REST_VERSION = "rm.rest.version";

    private static final String d_REST_VERSION = "0.0";

    /** RM version string */
    public static final String RM_VERSION = "rm.server.version";

    private static final String d_RM_VERSION = "0.0";

    /** stat history refresh rate in millis */
    public static final String STATISTICS_REFRESH_TIME = "rm.client.stats.time";

    private static final String d_STATISTICS_REFRESH_TIME = "3000";

    /** message of the day */
    public static final String MOTD_URL = "rm.motd.url";

    private static final String d_MOTD_URL = "";

    /** the protocol for jmx nodes communication (default or proactive) */
    public static final String MONITORING_PROTOCOL = "rm.monitoring.protocol";

    public static final String MONITORING_PROTOCOL_DEFAULT = "default";

    /** refresh period in milliseconds */
    public static final String MONITORING_PERIOD = "rm.monitoring.period";

    public static final String MONITORING_PERIOD_DEFAULT = "10000";

    /** hostname used to form the jmx url to monitor the node sources in the RM */
    public static final String RM_JMX_HOSTNAME = "rm.jmx.hostname";

    public static final String RM_JMX_HOSTNAME_DEFAULT = "localhost";

    /** port used to form the jmx url to monitor the node sources in the RM */
    public static final String RM_JMX_PORT = "rm.jmx.port";

    public static final String RM_JMX_PORT_DEFAULT = "5822";

    /** servername used to form the jmx url to monitor the node sources in the RM */
    public static final String RM_JMX_SERVER_NAME = "rm.jmx.servername";

    public static final String RM_JMX_SERVER_NAME_DEFAULT = "JMXRMAgent";

    /** prefix used to form the jmx url to monitor the node sources in the RM */
    public static final String RM_JMX_PREFIX = "rm.jmx.prefix";

    public static final String RM_JMX_PREFIX_DEFAULT = "service:jmx:rmi:///jndi/rmi://";

    /** Workflow Catalog URL **/
    public static final String CATALOG_URL = "sched.catalog.url";

    private static RMConfig instance = null;

    /**
     * @return static config instance, cannot be null
     */
    public static RMConfig get() {
        if (instance == null) {
            instance = new RMConfig();
            instance.setDefaults();
        }
        return instance;
    }

    private void setDefaults() {
        properties.put(REST_URL, d_REST_URL);
        properties.put(RM_URL, d_RM_URL);
        properties.put(CLIENT_REFRESH_TIME, d_CLIENT_REFRESH_TIME);
        properties.put(CLIENT_BURST_REFRESH_TIME, d_CLIENT_BURST_REFRESH_TIME);
        properties.put(VERSION, d_VERSION);
        properties.put(RM_VERSION, d_RM_VERSION);
        properties.put(REST_VERSION, d_REST_VERSION);
        properties.put(STATISTICS_REFRESH_TIME, d_STATISTICS_REFRESH_TIME);
        properties.put(MOTD_URL, d_MOTD_URL);
        properties.put(MONITORING_PROTOCOL, MONITORING_PROTOCOL_DEFAULT);
        properties.put(MONITORING_PERIOD, MONITORING_PERIOD_DEFAULT);
        properties.put(RM_JMX_HOSTNAME, RM_JMX_HOSTNAME_DEFAULT);
        properties.put(RM_JMX_PORT, RM_JMX_PORT_DEFAULT);
        properties.put(RM_JMX_SERVER_NAME, RM_JMX_SERVER_NAME_DEFAULT);
        properties.put(RM_JMX_PREFIX, RM_JMX_PREFIX_DEFAULT);
    }

    @Override
    public String getApplicationName() {
        return "Resource Manager";
    }

    @Override
    public String getRestUrl() {
        return properties.get(REST_URL);
    }

    @Override
    protected String getRestPublicUrlIfDefinedOrOverridden() {
        String restPublicUrl = properties.get(REST_PUBLIC_URL);
        if ((restPublicUrl == null || restPublicUrl.isEmpty()) && !getRestUrl().equals(d_REST_URL)) {
            return getRestUrl();
        }
        return restPublicUrl;
    }

    @Override
    public String getVersion() {
        return properties.get(VERSION);
    }

    @Override
    public String getRestVersion() {
        return properties.get(REST_VERSION);
    }

    @Override
    public String getApplicationVersion() {
        return properties.get(RM_VERSION);
    }

    @Override
    public String getMotdUrl() {
        return properties.get(MOTD_URL);
    }

    /**
     * @return refresh rate in millis
     */
    public int getClientRefreshTime() {
        return Integer.parseInt(properties.get(CLIENT_REFRESH_TIME));
    }

    public int getClientBurstRefreshTime() {
        return Integer.parseInt(properties.get(CLIENT_BURST_REFRESH_TIME));
    }

    /**
     * @return refresh rate in millis for the Statistics History
     */
    public int getStatisticsRefreshTime() {
        return Integer.parseInt(properties.get(STATISTICS_REFRESH_TIME));
    }

    /**
     * @return protocol for jmx nodes communication (default or proactive)
     */
    public String getMonitoringProtocol() {
        return properties.get(MONITORING_PROTOCOL);
    }

    /**
     * @return refresh period in milliseconds
     */
    public int getMonitoringPeriod() {
        return Integer.parseInt(properties.get(MONITORING_PERIOD));
    }

    /**
     * @return hostname used to form the jmx url to monitor the node sources in the RM
     */
    public String getRMJmxHostname() {
        return properties.get(RM_JMX_HOSTNAME);
    }

    /**
     * @return port used to form the jmx url to monitor the node sources in the RM
     */
    public int getRMJmxPort() {
        return Integer.parseInt(properties.get(RM_JMX_PORT));
    }

    /**
     * @return server name used to form the jmx url to monitor the node sources in the RM
     */
    public String getRMJmxServerName() {
        return properties.get(RM_JMX_SERVER_NAME);
    }

    /**
     * @return prefix used to form the jmx url to monitor the node sources in the RM
     */
    public String getRMJmxPrefix() {
        return properties.get(RM_JMX_PREFIX);
    }

    public String getRMUrl() {
        return properties.get(RM_URL);
    }

    /**
     * @return the catalog url or null if none has been defined
     */
    public String getCatalogUrl() {
        return properties.get(CATALOG_URL);
    }

}
