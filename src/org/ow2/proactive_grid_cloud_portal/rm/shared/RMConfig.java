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
package org.ow2.proactive_grid_cloud_portal.rm.shared;

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;


/**
 * RM specific configuration
 * 
 * 
 * @author mschnoor
 *
 */
public class RMConfig extends Config {

	/**
	 * Path to the user-defined RM property file, relative
	 * to the webapp file path
	 */
	public static final String CONFIG_PATH = "rm.conf";

	/** URL of the REST service */
	public static final String REST_URL = "rm.rest.url";
	private static final String d_REST_URL = "http://localhost:8080/proactive_grid_cloud_portal/rest";

	/** refresh rate in millis */
	public static final String CLIENT_REFRESH_TIME = "rm.client.refresh.time";
	private static final String d_CLIENT_REFRESH_TIME = "3000";

	/** release version string */
	public static final String VERSION = "rm.version";
	private static final String d_VERSION = "0.0";

	/** REST version string */
	public static final String REST_VERSION = "rm.rest.version";
	private static final String d_REST_VERSION = "0.0";

	/** RM version string */
	public static final String RM_VERSION = "rm.server.version";
	private static final String d_RM_VERSION = "0.0";

	/** SCM revision number */
	public static final String REVISION = "rm.revision";
	private static final String d_REVISION = "0";

	/** stat history refresh rate in millis */
	public static final String STATISTICS_REFRESH_TIME = "rm.client.stats.time";
	private static final String d_STATISTICS_REFRESH_TIME = "5000";

	/** message of the day */
	public static final String MOTD_URL = "rm.motd.url";
	private static final String d_MOTD_URL = "";

	/** the protocol for jmx nodes communication (default or proactive) */
	public static final String MONITORING_PROTOCOL = "rm.monitoring.protocol";
	public static final String MONITORING_PROTOCOL_DEFAULT = "default";

	/** refresh period in milliseconds */
	public static final String MONITORING_PERIOD = "rm.monitoring.period";
	public static final String MONITORING_PERIOD_DEFAULT = "10000";

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
		properties.put(CLIENT_REFRESH_TIME, d_CLIENT_REFRESH_TIME);
		properties.put(VERSION, d_VERSION);
		properties.put(REVISION, d_REVISION);
		properties.put(RM_VERSION, d_RM_VERSION);
		properties.put(REST_VERSION, d_REST_VERSION);
		properties.put(STATISTICS_REFRESH_TIME, d_STATISTICS_REFRESH_TIME);
		properties.put(MOTD_URL, d_MOTD_URL);
		properties.put(MONITORING_PROTOCOL, MONITORING_PROTOCOL_DEFAULT);
		properties.put(MONITORING_PERIOD, MONITORING_PERIOD_DEFAULT);
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
	public String getVersion() {
		return properties.get(VERSION);
	}

	@Override
	public String getRevision() {
		return properties.get(REVISION);
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
}
