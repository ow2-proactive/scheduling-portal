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
package org.ow2.proactive_grid_cloud_portal.rm.server;

import java.io.InputStream;
import java.util.List;
import java.util.Set;

import javax.management.ObjectName;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.jboss.resteasy.annotations.GZIP;


@Path("/")
public interface RestClient {

    @GET
    @Path("/common/permissions/portals/{portal}")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream portalAccess(@HeaderParam("sessionid") String sessionId, @PathParam("portal") String portal);

    @POST
    @Path("/rm/disconnect")
    @Produces(MediaType.APPLICATION_JSON)
    void logout(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("/rm/state")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream state(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/monitoring")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream monitoring(@HeaderParam("sessionid") String sessionId,
            @HeaderParam("clientCounter") @DefaultValue("-1") String counter);

    @GET
    @GZIP
    @Path("/rm/infrastructures")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream infrastructures(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("/rm/policies")
    @GZIP
    @Produces(MediaType.APPLICATION_JSON)
    InputStream policies(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/infrastructures/mapping")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getInfrasToPoliciesMapping(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/nodesource/configuration")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getNodeSourceConfiguration(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodeSourceName") String nodeSourceName);

    @POST
    @Path("/rm/nodesource")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream defineNodeSource(@HeaderParam("sessionId") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName,
            @FormParam("infrastructureType") String infrastructureType,
            @FormParam("infrastructureParameters") String[] infrastructureParameters,
            @FormParam("infrastructureFileParameters") String[] infrastructureFileParameters,
            @FormParam("policyType") String policyType, @FormParam("policyParameters") String[] policyParameters,
            @FormParam("policyFileParameters") String[] policyFileParameters,
            @FormParam("nodesRecoverable") String nodesRecoverable);

    @PUT
    @Path("/rm/nodesource/edit")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream editNodeSource(@HeaderParam("sessionId") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName,
            @FormParam("infrastructureType") String infrastructureType,
            @FormParam("infrastructureParameters") String[] infrastructureParameters,
            @FormParam("infrastructureFileParameters") String[] infrastructureFileParameters,
            @FormParam("policyType") String policyType, @FormParam("policyParameters") String[] policyParameters,
            @FormParam("policyFileParameters") String[] policyFileParameters,
            @FormParam("nodesRecoverable") String nodesRecoverable);

    @PUT
    @Path("/rm/nodesource/parameter")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream updateDynamicParameters(@HeaderParam("sessionId") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName,
            @FormParam("infrastructureType") String infrastructureType,
            @FormParam("infrastructureParameters") String[] infrastructureParameters,
            @FormParam("infrastructureFileParameters") String[] infrastructureFileParameters,
            @FormParam("policyType") String policyType, @FormParam("policyParameters") String[] policyParameters,
            @FormParam("policyFileParameters") String[] policyFileParameters);

    @PUT
    @Path("/rm/nodesource/deploy")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream deployNodeSource(@HeaderParam("sessionid") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName);

    @PUT
    @Path("/rm/nodesource/undeploy")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream undeployNodeSource(@HeaderParam("sessionid") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName, @FormParam("preempt") boolean preempt);

    @POST
    @Path("/rm/node/lock")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream lockNodes(@HeaderParam("sessionid") String sessionId, @FormParam("nodeurls") Set<String> nodeUrls);

    @POST
    @Path("/rm/node/unlock")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream unlockNodes(@HeaderParam("sessionid") String sessionId, @FormParam("nodeurls") Set<String> nodeUrls);

    @POST
    @Path("/rm/nodesource/remove")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream removeNodesource(@HeaderParam("sessionId") String sessionId, @FormParam("name") String nsName,
            @FormParam("preempt") boolean preempt);

    @POST
    @Path("/rm/node/remove")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream removeNode(@HeaderParam("sessionId") String sessionId, @FormParam("url") String url,
            @FormParam("preempt") boolean preempt);

    @POST
    @Path("/rm/node/release")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream releaseNode(@HeaderParam("sessionId") String sessionId, @FormParam("url") String url);

    @GET
    @Path("/rm/version")
    @Produces(MediaType.TEXT_PLAIN)
    InputStream getVersion();

    @GET
    @GZIP
    @Path("/rm/info/{name}")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getMBeanInfo(@HeaderParam("sessionid") String sessionId, @PathParam("name") ObjectName name,
            @QueryParam("attr") List<String> attrs);

    @GET
    @Path("/rm/node/mbean")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getNodeMBeanInfo(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectName,
            @QueryParam("attrs") List<String> attrs);

    @GET
    @Path("/rm/node/mbean/history")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getNodeMBeanHistory(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectName,
            @QueryParam("attrs") List<String> attrs, @QueryParam("range") String range);

    @GET
    @Path("/rm/node/mbeans")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getNodeMBeansInfo(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectNames,
            @QueryParam("attrs") List<String> attrs);

    @GET
    @Path("/rm/node/mbeans/history")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getNodeMBeansHistory(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectNames,
            @QueryParam("attrs") List<String> attrs, @QueryParam("range") String range);

    @GET
    @GZIP
    @Path("/rm/stathistory")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getStatHistory(@HeaderParam("sessionid") String sessionId, @QueryParam("range") String range);

    @POST
    @GZIP
    @Path("/rm/node/script")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream executeNodeScript(@HeaderParam("sessionid") String sessionId, @FormParam("nodeurl") String nodeUrl,
            @FormParam("script") String script, @FormParam("scriptEngine") String scriptEngine);

    @POST
    @GZIP
    @Path("/rm/nodesource/script")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream executeNodeSourceScript(@HeaderParam("sessionid") String sessionId,
            @FormParam("nodesource") String nodeSource, @FormParam("script") String script,
            @FormParam("scriptEngine") String scriptEngine);

    @POST
    @GZIP
    @Path("/rm/host/script")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream executeHostScript(@HeaderParam("sessionid") String sessionId, @FormParam("host") String host,
            @FormParam("script") String script, @FormParam("scriptEngine") String scriptEngine);

    @GET
    @GZIP
    @Path("/rm/threaddump")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getRMThreadDump(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/node/threaddump")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getNodeThreadDump(@HeaderParam("sessionid") String sessionId, @QueryParam("nodeurl") String nodeUrl);

    @POST
    @GZIP
    @Path("/rm/node/tokens")
    @Produces(MediaType.APPLICATION_JSON)
    void setNodeTokens(@HeaderParam("sessionid") String sessionId, @HeaderParam("nodeurl") String nodeUrl,
            @QueryParam("tokens") List<String> tokens);
}
