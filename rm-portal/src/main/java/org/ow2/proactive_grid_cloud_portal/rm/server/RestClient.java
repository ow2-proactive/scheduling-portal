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

import org.jboss.resteasy.annotations.GZIP;


@Path("/")
public interface RestClient {

    @POST
    @Path("/rm/disconnect")
    @Produces("application/json")
    void logout(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("/rm/state")
    @Produces("application/json")
    InputStream state(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/monitoring")
    @Produces("application/json")
    InputStream monitoring(@HeaderParam("sessionid") String sessionId,
            @HeaderParam("clientCounter") @DefaultValue("-1") String counter);

    @GET
    @GZIP
    @Path("/rm/infrastructures")
    @Produces("application/json")
    InputStream infrastructures(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("/rm/policies")
    @GZIP
    @Produces("application/json")
    InputStream policies(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("infrastructures/mapping")
    @GZIP
    @Produces("application/json")
    InputStream getInfrasToPoliciesMapping(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/nodesource/configuration")
    @Produces("application/json")
    InputStream getNodeSourceConfiguration(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodeSourceName") String nodeSourceName);

    @POST
    @Path("/rm/nodesource")
    @Produces("application/json")
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
    @Produces("application/json")
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
    @Produces("application/json")
    InputStream updateDynamicParameters(@HeaderParam("sessionId") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName,
            @FormParam("infrastructureType") String infrastructureType,
            @FormParam("infrastructureParameters") String[] infrastructureParameters,
            @FormParam("infrastructureFileParameters") String[] infrastructureFileParameters,
            @FormParam("policyType") String policyType, @FormParam("policyParameters") String[] policyParameters,
            @FormParam("policyFileParameters") String[] policyFileParameters);

    @PUT
    @Path("/rm/nodesource/deploy")
    @Produces("application/json")
    InputStream deployNodeSource(@HeaderParam("sessionid") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName);

    @PUT
    @Path("/rm/nodesource/undeploy")
    @Produces("application/json")
    InputStream undeployNodeSource(@HeaderParam("sessionid") String sessionId,
            @FormParam("nodeSourceName") String nodeSourceName, @FormParam("preempt") boolean preempt);

    @POST
    @Path("/rm/node/lock")
    @Produces("application/json")
    InputStream lockNodes(@HeaderParam("sessionid") String sessionId, @FormParam("nodeurls") Set<String> nodeUrls);

    @POST
    @Path("/rm/node/unlock")
    @Produces("application/json")
    InputStream unlockNodes(@HeaderParam("sessionid") String sessionId, @FormParam("nodeurls") Set<String> nodeUrls);

    @POST
    @Path("/rm/nodesource/remove")
    @Produces("application/json")
    InputStream removeNodesource(@HeaderParam("sessionId") String sessionId, @FormParam("name") String nsName,
            @FormParam("preempt") boolean preempt);

    @POST
    @Path("/rm/node/remove")
    @Produces("application/json")
    InputStream removeNode(@HeaderParam("sessionId") String sessionId, @FormParam("url") String url,
            @FormParam("preempt") boolean preempt);

    @POST
    @Path("/rm/node/release")
    @Produces("application/json")
    InputStream releaseNode(@HeaderParam("sessionId") String sessionId, @FormParam("url") String url);

    @GET
    @Path("/rm/version")
    InputStream getVersion();

    @GET
    @GZIP
    @Path("/rm/info/{name}")
    @Produces("application/json")
    InputStream getMBeanInfo(@HeaderParam("sessionid") String sessionId, @PathParam("name") ObjectName name,
            @QueryParam("attr") List<String> attrs);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbean")
    InputStream getNodeMBeanInfo(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectName,
            @QueryParam("attrs") List<String> attrs);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbean/history")
    InputStream getNodeMBeanHistory(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectName,
            @QueryParam("attrs") List<String> attrs, @QueryParam("range") String range);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbeans")
    InputStream getNodeMBeansInfo(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectNames,
            @QueryParam("attrs") List<String> attrs);

    @GET
    @Produces("application/json")
    @Path("/rm/node/mbeans/history")
    InputStream getNodeMBeansHistory(@HeaderParam("sessionid") String sessionId,
            @QueryParam("nodejmxurl") String nodeJmxUrl, @QueryParam("objectname") String objectNames,
            @QueryParam("attrs") List<String> attrs, @QueryParam("range") String range);

    @GET
    @GZIP
    @Path("/rm/stathistory")
    @Produces("application/json")
    InputStream getStatHistory(@HeaderParam("sessionid") String sessionId, @QueryParam("range") String range);

    @POST
    @GZIP
    @Path("/rm/node/script")
    @Produces("application/json")
    InputStream executeNodeScript(@HeaderParam("sessionid") String sessionId, @FormParam("nodeurl") String nodeUrl,
            @FormParam("script") String script, @FormParam("scriptEngine") String scriptEngine);

    @POST
    @GZIP
    @Path("/rm/nodesource/script")
    @Produces("application/json")
    InputStream executeNodeSourceScript(@HeaderParam("sessionid") String sessionId,
            @FormParam("nodesource") String nodeSource, @FormParam("script") String script,
            @FormParam("scriptEngine") String scriptEngine);

    @POST
    @GZIP
    @Path("/rm/host/script")
    @Produces("application/json")
    InputStream executeHostScript(@HeaderParam("sessionid") String sessionId, @FormParam("host") String host,
            @FormParam("script") String script, @FormParam("scriptEngine") String scriptEngine);

    @GET
    @GZIP
    @Path("/rm/threaddump")
    @Produces("application/json")
    InputStream getRMThreadDump(@HeaderParam("sessionid") String sessionId);

    @GET
    @GZIP
    @Path("/rm/node/threaddump")
    @Produces("application/json")
    InputStream getNodeThreadDump(@HeaderParam("sessionid") String sessionId, @QueryParam("nodeurl") String nodeUrl);

}
