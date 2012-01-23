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

import java.io.InputStream;
import java.util.List;
import java.util.Set;

import javax.management.ObjectName;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;

import org.jboss.resteasy.annotations.GZIP;
import org.jboss.resteasy.client.ClientResponse;


@Path("/")
public interface RestClient {

	@POST
	@Path("/rm/disconnect")
	@Produces("application/json")
	public void logout(@HeaderParam("sessionid")
	String sessionId);

	@GET
	@Path("/rm/state")
	@Produces("application/json")
	public ClientResponse<InputStream> state(@HeaderParam("sessionid")
	String sessionId);

	@GET
	@GZIP
	@Path("/rm/monitoring")
	@Produces("application/json")
	public ClientResponse<InputStream> monitoring(@HeaderParam("sessionid")
	String sessionId);

	@GET
	@GZIP
	@Path("/rm/infrastructures")
	@Produces("application/json")
	public ClientResponse<InputStream> infrastructures(@HeaderParam("sessionid")
	String sessionId);

	@GET
	@Path("/rm/policies")
	@GZIP
	@Produces("application/json")
	public ClientResponse<InputStream> policies(@HeaderParam("sessionid")
	String sessionId);

	@POST
	@Path("/rm/nodesource/create")
	@Produces("application/json")
	public ClientResponse<InputStream> createnodeSource(@HeaderParam("sessionId")
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
	public ClientResponse<InputStream> lockNodes(@HeaderParam("sessionid")
	String sessionId, @FormParam("nodeurls")
	Set<String> nodeUrls);

	@POST
	@Path("/rm/node/unlock")
	@Produces("application/json")
	public ClientResponse<InputStream> unlockNodes(@HeaderParam("sessionid")
	String sessionId, @FormParam("nodeurls")
	Set<String> nodeUrls);

	@POST
	@Path("/rm/nodesource/remove")
	@Produces("application/json")
	public ClientResponse<InputStream> removeNodesource(@HeaderParam("sessionId")
	String sessionId, @FormParam("name")
	String nsName, @FormParam("preempt")
	boolean preempt);

	@POST
	@Path("/rm/node/remove")
	@Produces("application/json")
	public ClientResponse<InputStream> removeNode(@HeaderParam("sessionId")
	String sessionId, @FormParam("url")
	String url, @FormParam("preempt")
	boolean preempt);

	@POST
	@Path("/rm/node/release")
	@Produces("application/json")
	public ClientResponse<InputStream> releaseNode(@HeaderParam("sessionId")
	String sessionId, @FormParam("url")
	String url);

	@GET
	@Path("/rm/version")
	public ClientResponse<InputStream> getVersion();

	@GET
	@GZIP
	@Path("/rm/info/{name}")
	@Produces("application/json")
	public ClientResponse<InputStream> getMBeanInfo(@HeaderParam("sessionid")
	String sessionId, @PathParam("name")
	ObjectName name, @QueryParam("attr")
	List<String> attrs);

	@GET
	@GZIP
	@Path("/rm/stathistory")
	@Produces("application/json")
	public ClientResponse<InputStream> getStatHistory(@HeaderParam("sessionid")
	String sessionId, @QueryParam("range")
	String range);

}