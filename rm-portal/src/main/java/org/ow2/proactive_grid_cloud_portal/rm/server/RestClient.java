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
package org.ow2.proactive_grid_cloud_portal.rm.server;

import org.jboss.resteasy.annotations.GZIP;

import javax.management.ObjectName;
import javax.ws.rs.*;
import java.io.InputStream;
import java.util.List;
import java.util.Set;


@Path("/")
public interface RestClient {

    @POST
    @Path("/rm/disconnect")
    @Produces("application/json")
    void logout(@HeaderParam("sessionid")
                String sessionId);

    @GET
    @Path("/rm/state")
    @Produces("application/json")
    InputStream state(@HeaderParam("sessionid")
                                      String sessionId);

    @GET
    @GZIP
    @Path("/rm/monitoring")
    @Produces("application/json")
    InputStream monitoring(@HeaderParam("sessionid")
                                           String sessionId);

    @GET
    @GZIP
    @Path("/rm/infrastructures")
    @Produces("application/json")
    InputStream infrastructures(@HeaderParam("sessionid")
                                                String sessionId);

    @GET
    @Path("/rm/policies")
    @GZIP
    @Produces("application/json")
    InputStream policies(@HeaderParam("sessionid")
                                         String sessionId);

    @POST
    @Path("/rm/nodesource/create")
    @Produces("application/json")
    InputStream createnodeSource(@HeaderParam("sessionId")
                                                 String sessionId, @FormParam("nodeSourceName")
                                                 String nodeSourceName, @FormParam("infrastructureType")
                                                 String infrastructureType, @FormParam("infrastructureParameters")
                                                 String[] infrastructureParameters, @FormParam("infrastructureFileParameters")
                                                 String[] infrastructureFileParameters, @FormParam("policyType")
                                                 String policyType, @FormParam("policyParameters")
                                                 String[] policyParameters, @FormParam("policyFileParameters")
                                                 String[] policyFileParameters);

    @POST
    @Path("/rm/node/lock")
    @Produces("application/json")
    InputStream lockNodes(@HeaderParam("sessionid")
                                          String sessionId, @FormParam("nodeurls")
                                          Set<String> nodeUrls);

    @POST
    @Path("/rm/node/unlock")
    @Produces("application/json")
    InputStream unlockNodes(@HeaderParam("sessionid")
                                            String sessionId, @FormParam("nodeurls")
                                            Set<String> nodeUrls);

    @POST
    @Path("/rm/nodesource/remove")
    @Produces("application/json")
    InputStream removeNodesource(@HeaderParam("sessionId")
                                                 String sessionId, @FormParam("name")
                                                 String nsName, @FormParam("preempt")
                                                 boolean preempt);

    @POST
    @Path("/rm/node/remove")
    @Produces("application/json")
    InputStream removeNode(@HeaderParam("sessionId")
                                           String sessionId, @FormParam("url")
                                           String url, @FormParam("preempt")
                                           boolean preempt);

    @POST
    @Path("/rm/node/release")
    @Produces("application/json")
    InputStream releaseNode(@HeaderParam("sessionId")
                                            String sessionId, @FormParam("url")
                                            String url);

    @GET
    @Path("/rm/version")
    InputStream getVersion();

    @GET
    @GZIP
    @Path("/rm/info/{name}")
    @Produces("application/json")
    InputStream getMBeanInfo(@HeaderParam("sessionid")
                                             String sessionId, @PathParam("name")
                                             ObjectName name, @QueryParam("attr")
                                             List<String> attrs);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbean")
    InputStream getNodeMBeanInfo(@HeaderParam("sessionid")
                                                 String sessionId, @QueryParam("nodejmxurl")
                                                 String nodeJmxUrl, @QueryParam("objectname")
                                                 String objectName, @QueryParam("attrs")
                                                 List<String> attrs);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbean/history")
    InputStream getNodeMBeanHistory(
            @HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl,
            @QueryParam("objectname") String objectName,
            @QueryParam("attrs") List<String> attrs,
            @QueryParam("range") String range);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbeans")
    InputStream getNodeMBeansInfo(@HeaderParam("sessionid")
                                                  String sessionId, @QueryParam("nodejmxurl")
                                                  String nodeJmxUrl, @QueryParam("objectname")
                                                  String objectNames, @QueryParam("attrs")
                                                  List<String> attrs);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbeans/history")
    InputStream getNodeMBeansHistory(
            @HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl,
            @QueryParam("objectname") String objectNames,
            @QueryParam("attrs") List<String> attrs,
            @QueryParam("range") String range);

    @GET
    @GZIP
    @Path("/rm/stathistory")
    @Produces("application/json")
    InputStream getStatHistory(@HeaderParam("sessionid")
                                               String sessionId, @QueryParam("range")
                                               String range);

    @POST
    @GZIP
    @Path("/rm/node/script")
    @Produces("application/json")
    InputStream executeNodeScript(@HeaderParam("sessionid")
                                                  String sessionId, @FormParam("nodeurl")
                                                  String nodeUrl, @FormParam("script")
                                                  String script, @FormParam("scriptEngine")
                                                  String scriptEngine);

}