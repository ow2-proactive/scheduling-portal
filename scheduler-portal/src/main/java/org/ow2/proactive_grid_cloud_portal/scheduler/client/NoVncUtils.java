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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.http.client.URL;


public class NoVncUtils {

    private NoVncUtils() {
    }

    public static String createNoVncPageUrl(String sessionId, String jobId, String taskName) {
        String noVncUrl = createNoVncUrl(sessionId, jobId, taskName);
        return createHttpsRedirectUrl(noVncUrl);
    }

    private static String createNoVncUrl(String sessionId, String jobId, String taskName) {
        String noVncPageUrl = SchedulerConfig.get().getNoVncPageUrl();
        // we want to connect to a task (task output will be checked)
        noVncPageUrl += "?sessionId=" + sessionId;
        noVncPageUrl += "&jobId=" + jobId;
        noVncPageUrl += "&taskName=" + taskName;

        // we connect to this websocket proxy
        String noVncUrl = SchedulerConfig.get().getNoVncUrl();
        String[] splitUrl = noVncUrl.split(":");
        noVncPageUrl += "&host=" + splitUrl[1].replace("//", "");
        noVncPageUrl += "&port=" + splitUrl[2].replaceAll("[^0-9]", "");

        // we encrypt if https is used
        noVncPageUrl += "&encrypt=" + (noVncUrl.toLowerCase().contains("https") ? "true" : "false");
        return noVncPageUrl;
    }

    private static String createHttpsRedirectUrl(String noVncPageUrl) {
        // first we hit the websocket host and port to validate the HTTPS certificate
        // see https://bugzilla.mozilla.org/show_bug.cgi?id=594502
        // we will be redirect to novnc static web page afterwards
        String redirectedUrl = SchedulerConfig.get().getNoVncUrl();
        redirectedUrl += "/redirect";
        redirectedUrl += "?url=" + URL.encodeQueryString(noVncPageUrl);
        return redirectedUrl;
    }
}
