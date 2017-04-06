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
package org.ow2.proactive_grid_cloud_portal.common.server;

import java.io.File;

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;


/**
 * GWT Service abstraction
 * <p>
 * Allows some degree of factorization for GWT server code:
 * some generic servlets can access a generic {@link #Service()}
 * statically, and using generic static {@link Config}, do things
 * independently on RM or Scheduler side.
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public abstract class Service extends RemoteServiceServlet {

    private static Service instance = null;

    /**
     * @return current static Service instance, if it has been created
     * @throws IllegalStateException Service was not created
     */
    public static Service get() {
        if (instance == null) {
            throw new IllegalStateException("Service has not been loaded");
        }
        return instance;
    }

    public Service() {
        instance = this;
    }

    /**
     * Attempt login through the REST API
     * <p>
     * username and password may be null if credential is valid,
     * credential may be null if username and password are valid,
     * private ssh key may always be null
     * 
     * @param login username
     * @param pass password
     * @param cred credential file in its base64 form
     * @param ssh private ssh key
     */
    public abstract String login(String login, String pass, File cred, String ssh)
            throws RestServerException, ServiceException;

    /**
     * Get the connected user associated with a session id
     * @param sessionId
     * @return user name
     * @throws RestServerException
     * @throws ServiceException
     */
    public abstract String getLoginFromSessionId(String sessionId) throws RestServerException, ServiceException;

    /**
     * Create a Credentials file with the provided authentication parameters
     * 
     * @param login username
     * @param pass password
     * @param ssh private ssh key
     * @return the the Credentials file as a base64 String
     * @throws ServiceException
     */
    public abstract String createCredentials(String login, String pass, String ssh)
            throws RestServerException, ServiceException;

    @Override
    public void checkPermutationStrongName() {
        /*
         * FIXME
         * disable the check for XSRF attack, which finds false positives and refuses to serve
         * requests to some clients, for no apparent reason:
         * >> java.lang.SecurityException: Blocked request without GWT permutation header (XSRF
         * attack?)
         * >> at com.google.gwt.user.server.rpc.RemoteServiceServlet.checkPermutationStrongName(
         * RemoteServiceServlet.java:267)
         *
         * This may be fixed in later versions of GWT, just remove this method to restore the
         * original behaviour
         */
    }
}
