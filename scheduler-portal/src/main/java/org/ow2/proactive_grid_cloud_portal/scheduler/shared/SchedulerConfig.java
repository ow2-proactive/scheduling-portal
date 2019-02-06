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

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;


/**
 * Scheduler specific configuration
 *
 * @author mschnoor
 */
public class SchedulerConfig extends Config {

    /**
     * Path to the user-defined Scheduler property file, relative to the webapp file path
     */
    public static final String CONFIG_PATH = "scheduler.conf";

    /** URL of the remote REST service */
    public static final String REST_URL = "sched.rest.url";

    /** URL of the remote job-planner REST service */
    public static final String JOBPLANNER_URL = "jobplanner.rest.url";

    public static final String REST_PUBLIC_URL = "sched.rest.public.url";

    /** URL of the remote noVNC proxy */
    public static final String NOVNC_URL = "sched.novnc.url";

    /** URL of the remote noVNC webpage */
    public static final String NOVNC_PAGE_URL = "sched.novnc.page.url";

    /** client refresh rate in millis */
    public static final String CLIENT_REFRESH_TIME = "sched.client.refresh.time";

    private static final String DEFAULT_CLIENT_REFRESH_TIME = "3000";

    /** client livelog refresh rate in millis */
    public static final String LIVELOGS_REFRESH_TIME = "sched.client.livelog.refresh.time";

    private static final String DEFAULT_LIVELOGS_REFRESH_TIME = "1000";

    /** job page size */
    public static final String JOBS_PAGE_SIZE = "sched.jobs.page.size";

    private static final String DEFAULT_JOBS_PAGE_SIZE = "50";

    /** task page size */
    public static final String TASKS_PAGE_SIZE = "sched.tasks.page.size";

    private static final String DEFAULT_TASKS_PAGE_SIZE = "20";

    /** the number max of tag suggestions that should be displayed for autocompletion. */
    public static final String TAG_SUGGESTIONS_SIZE = "sched.tags.suggestions.size";

    private static final String DEFAULT_TAG_SUGGESTIONS_SIZE = "20";

    /** the delay applied before refreshing the tag suggestions for a running job. */
    public static final String TAG_SUGGESTIONS_DELAY = "sched.tags.suggestions.delay";

    private static final String DEFAULT_TAG_SUGGESTIONS_DELAY = "30000";

    /** release version string */
    public static final String VERSION = "sched.version";

    private static final String DEFAULT_VERSION = "0.0";

    /** REST version string */
    public static final String REST_VERSION = "sched.rest.version";

    private static final String DEFAULT_REST_VERSION = "0.0";

    /** Sched version string */
    public static final String SCHED_VERSION = "sched.server.version";

    private static final String DEFAULT_SCHED_VERSION = "0.0";

    /** message of the day service URL */
    public static final String MOTD_URL = "sched.motd.url";

    private static final String DEFAULT_MOTD_URL = "";

    /** URL of the scheduler graphql API */
    private static final String DEFAULT_SCHEDULING_API_URL = "http://localhost:8080/scheduling-api";

    public static final String SCHEDULING_API_URL = "pa.scheduling.api";

    /** Workflow Catalog URL **/
    public static final String CATALOG_URL = "sched.catalog.url";

    private static SchedulerConfig instance = null;

    /**
     * @return current static config instance, cannot be null
     */
    public static SchedulerConfig get() {
        if (instance == null) {
            instance = new SchedulerConfig();
            instance.setDefaults();
        }
        return instance;
    }

    private void setDefaults() {
        properties.put(CLIENT_REFRESH_TIME, DEFAULT_CLIENT_REFRESH_TIME);
        properties.put(LIVELOGS_REFRESH_TIME, DEFAULT_LIVELOGS_REFRESH_TIME);
        properties.put(JOBS_PAGE_SIZE, DEFAULT_JOBS_PAGE_SIZE);
        properties.put(TASKS_PAGE_SIZE, DEFAULT_TASKS_PAGE_SIZE);
        properties.put(VERSION, DEFAULT_VERSION);
        properties.put(SCHED_VERSION, DEFAULT_SCHED_VERSION);
        properties.put(REST_VERSION, DEFAULT_REST_VERSION);
        properties.put(MOTD_URL, DEFAULT_MOTD_URL);
        properties.put(TAG_SUGGESTIONS_SIZE, DEFAULT_TAG_SUGGESTIONS_SIZE);
        properties.put(TAG_SUGGESTIONS_DELAY, DEFAULT_TAG_SUGGESTIONS_DELAY);
        properties.put(SCHEDULING_API_URL, DEFAULT_SCHEDULING_API_URL);
    }

    @Override
    public String getApplicationName() {
        return "Scheduler";
    }

    @Override
    public String getRestUrl() {
        String restUrlFromProperties = properties.get(REST_URL);
        if (restUrlFromProperties == null) {
            String protocol = com.google.gwt.user.client.Window.Location.getProtocol();
            String port = com.google.gwt.user.client.Window.Location.getPort();
            String restUrl = protocol + "://localhost:" + port + "/rest";
            return restUrl;
        }
        return restUrlFromProperties;
    }

    /**
     * Returns the job-planner url from shceduler.conf if it exists or the default local URL otherwise.
     * @return job-planner url
     */
    public String getJobplannerUrl() {
        String jpUrlFromProperties = properties.get(JOBPLANNER_URL);
        if (jpUrlFromProperties == null) {
            String protocol = com.google.gwt.user.client.Window.Location.getProtocol();
            String port = com.google.gwt.user.client.Window.Location.getPort();
            String jobplannerUrl = protocol + "://localhost:" + port + "/job-planner/planned_jobs";
            return jobplannerUrl;
        }
        return jpUrlFromProperties;
    }

    /**
     * @return the URL from the window location
     */
    public String getWindowsLocationUrl() {
        String urlFromCurrentLocation = com.google.gwt.user.client.Window.Location.getHref();
        urlFromCurrentLocation = urlFromCurrentLocation.replace(com.google.gwt.user.client.Window.Location.getPath(),
                                                                "");
        return urlFromCurrentLocation;
    }

    /**
     * @return the REST_PUBLIC_URL if it is set or take it from the window location
     */
    @Override
    public String getRestPublicUrlIfDefinedOrOverridden() {
        String restPublicUrl = properties.get(REST_PUBLIC_URL);
        if (restPublicUrl == null) {
            String restUrlFromCurrentLocation = getWindowsLocationUrl();
            restUrlFromCurrentLocation += "/rest";
            return restUrlFromCurrentLocation;
        }
        return restPublicUrl;
    }

    /**
     * @return the NOVNC_URL if it is set or take it from the window location
     */
    public String getNoVncUrl() {
        String noVncUrl = properties.get(NOVNC_URL);
        if (noVncUrl == null) {
            String protocol = com.google.gwt.user.client.Window.Location.getProtocol();
            String host = com.google.gwt.user.client.Window.Location.getHost();
            String noVncUrlFromCurrentLocation = protocol + "://" + host + ":5900/rest/novnc";
            noVncUrlFromCurrentLocation.replace(":", "\\:");
            return noVncUrlFromCurrentLocation;
        }
        return noVncUrl;
    }

    /**
     * @return the NOVNC_PAGE_URL if it is set or take it from the window location
     */
    public String getNoVncPageUrl() {
        String noVncPageUrl = properties.get(NOVNC_PAGE_URL);
        if (noVncPageUrl == null) {
            String noVncPageUrlFromCurrentLocation = getWindowsLocationUrl();
            noVncPageUrlFromCurrentLocation += "/rest/novnc.html";
            noVncPageUrlFromCurrentLocation.replace(":", "\\:");
            return noVncPageUrlFromCurrentLocation;
        }
        return noVncPageUrl;
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
        return properties.get(SCHED_VERSION);
    }

    @Override
    public String getMotdUrl() {
        return properties.get(MOTD_URL);
    }

    /**
     * @return the client refresh rate in millis
     */
    public int getClientRefreshTime() {
        return Integer.parseInt(properties.get(CLIENT_REFRESH_TIME));
    }

    /**
     * @return number of jobs per page
     */
    public int getPageSize(PaginatedItemType itemType) {
        if (itemType == PaginatedItemType.JOB) {
            return Integer.parseInt(properties.get(JOBS_PAGE_SIZE));
        }
        if (itemType == PaginatedItemType.TASK) {
            return Integer.parseInt(properties.get(TASKS_PAGE_SIZE));
        }
        return 0;
    }

    /**
     * @return the number of tag suggestions that should be displayed for autocompletion.
     */
    public int getTagSuggestionSize() {
        return Integer.parseInt(this.properties.get(TAG_SUGGESTIONS_SIZE));
    }

    /**
     * @return the delay applied before refreshing the tag suggestions for a running job.
     */
    public long getTagSuggestionDelay() {
        return Long.parseLong(this.properties.get(TAG_SUGGESTIONS_DELAY));
    }

    /**
     * @return refresh rate for live logs in millis
     */
    public int getLivelogsRefreshTime() {
        return Integer.parseInt(properties.get(LIVELOGS_REFRESH_TIME));
    }

    /**
     * @return the catalog url or null if none has been defined
     */
    public String getCatalogUrl() {
        return properties.get(CATALOG_URL);
    }

    /**
     * @return Scheduling API URL
     */
    public String getSchedulingApiUrl() {
        return properties.get(SCHEDULING_API_URL);
    }
}
