package org.ow2.proactive_grid_cloud_portal.common.server;

import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import java.io.InputStream;

@Path("/common/")
public interface CommonRestClient {

    @GET
    @Path("/permissions/portals/{portal}")
    InputStream portalAccess(@HeaderParam("sessionid") String sessionId, @PathParam("portal") String portal);

}
