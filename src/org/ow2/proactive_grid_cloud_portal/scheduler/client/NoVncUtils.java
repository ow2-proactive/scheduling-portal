/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
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
        String redirectedUrl = SchedulerConfig.get().getRestUrl();
        // novnc static web page is hosted on the Rest server
        redirectedUrl += "/../novnc.html";
        // we want to connect to this VNC host
        redirectedUrl += "?sessionId=" + sessionId;
        redirectedUrl += "&jobId=" + jobId;
        redirectedUrl += "&taskName=" + taskName;

        // we connect to this websocket proxy
        String noVncUrl = SchedulerConfig.get().getNoVncUrl();
        String[] splitUrl = noVncUrl.split(":");
        redirectedUrl += "&host=" + splitUrl[1].replace("//", "");
        redirectedUrl += "&port=" + splitUrl[2].replaceAll("[^0-9]", "");

        // we encrypt if https is used
        redirectedUrl += "&encrypt=" + (noVncUrl.toLowerCase().contains("https") ? "True" : "False");
        return redirectedUrl;
    }

    private static String createHttpsRedirectUrl(String redirectedUrl) {
        // first we hit the websocket host and port to validate the HTTPS certificate
        // see https://bugzilla.mozilla.org/show_bug.cgi?id=594502
        // we will be redirect to novnc static web page afterwards
        String noVncUrl = SchedulerConfig.get().getNoVncUrl();
        noVncUrl += "/redirect";
        noVncUrl += "?url=" + URL.encodeQueryString(redirectedUrl);
        return noVncUrl;
    }
}
