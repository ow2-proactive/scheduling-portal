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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.ws.rs.core.Response.Status;

import org.ow2.proactive_grid_cloud_portal.common.server.ConfigReader;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.HttpUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMService;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.DefaultHttpClient;
import org.jboss.resteasy.client.ClientExecutor;
import org.jboss.resteasy.client.ClientResponse;
import org.jboss.resteasy.client.ProxyFactory;
import org.jboss.resteasy.client.core.executors.ApacheHttpClient4Executor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.ow2.proactive_grid_cloud_portal.common.server.HttpUtils.convertToString;


/**
 * The server side implementation of the RPC service.
 */
@SuppressWarnings("serial")
public class RMServiceImpl extends Service implements RMService {

    private static final Logger LOGGER = LoggerFactory.getLogger(RMServiceImpl.class);

    private ClientExecutor executor;
    private DefaultHttpClient httpClient;

    @Override
    public void init() {
        loadProperties();
        httpClient = HttpUtils.createDefaultExecutor();
        executor = new ApacheHttpClient4Executor(httpClient);
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getProperties()
     */
    public Map<String, String> getProperties() {
        return RMConfig.get().getProperties();
    }

    /**
     * Loads properties defined in the configuration file and in JVM arguments.
     */
    private void loadProperties() {
        RMConfig.get().load(ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(RMConfig.CONFIG_PATH)));
        ConfigUtils.loadSystemProperties(RMConfig.get());
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#logout(java.lang.String)
     */
    public void logout(String sessionId) throws ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        client.logout(sessionId);
    }

    /*
     * (non-Javadoc)
     * @see Service#login(java.lang.String, java.lang.String, java.io.File, java.lang.String)
     */
    public String login(String login, String pass, File cred, String ssh) throws RestServerException,
      ServiceException {
        HttpPost method = new HttpPost(RMConfig.get().getRestUrl() + "/rm/login");

        try {
            MultipartEntity entity = new MultipartEntity();

            if (cred == null) {
                entity.addPart("username", new StringBody(login));
                entity.addPart("password", new StringBody(pass));
                entity.addPart("sshkey", new StringBody(ssh));

            } else {
                entity.addPart("credential", new FileBody(cred, "application/octet-stream"));
            }

            method.setEntity(entity);

            HttpResponse response = httpClient.execute(method);
            String responseAsString = convertToString(response.getEntity().getContent());
            switch (response.getStatusLine().getStatusCode()) {
                case 200:
                    break;
                default:
                    String message = responseAsString;
                    if (message == null || message.trim().length() == 0) {
                        message = "{ \"httpErrorCode\": " + response.getStatusLine().getStatusCode() + "," + "\"errorMessage\": \"" +
                          response.getStatusLine().getReasonPhrase() + "\" }";
                    }
                    throw new RestServerException(response.getStatusLine().getStatusCode(), message);
            }
            return responseAsString;
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            method.releaseConnection();
            if (cred != null) {
                cred.delete();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getState(java.lang.String)
     */
    public String getState(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.state(sessionId);
            Status status = clientResponse.getResponseStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getMonitoring(java.lang.String)
     */
    public String getMonitoring(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.monitoring(sessionId);
            Status status = clientResponse.getResponseStatus();
            String ret = convertToString(clientResponse.getEntity());
            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /**
     * Create a Credentials file with the provided authentication parameters
     * 
     * @param login username
     * @param pass password
     * @param ssh private ssh key
     * @return the the Credentials file as a base64 String
     * @throws ServiceException
     */
    public String createCredentials(String login, String pass, String ssh) throws RestServerException,
            ServiceException {
        HttpPost method = new HttpPost(RMConfig.get().getRestUrl() + "/scheduler/createcredential");

        try {
            MultipartEntity entity = new MultipartEntity();
            entity.addPart("username", new StringBody(login));
            entity.addPart("password", new StringBody(pass));
            entity.addPart("sshkey", new StringBody(ssh));

            method.setEntity(entity);

            HttpResponse response = httpClient.execute(method);
            String responseAsString = convertToString(response.getEntity().getContent());

            switch (response.getStatusLine().getStatusCode()) {
                case 200:
                    return responseAsString;
                default:
                    throw new RestServerException(response.getStatusLine().getStatusCode(), responseAsString);
            }
        } catch (Exception e) {
            LOGGER.warn("Failed to create credentials", e);
            throw new ServiceException(e.getMessage());
        } finally {
            method.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#createNodeSource(java.lang.String, java.lang.String, java.lang.String, java.lang.String[], java.lang.String[], java.lang.String, java.lang.String[], java.lang.String[])
     */
    public String createNodeSource(String sessionId, String nodeSourceName, String infrastructureType,
            String[] infrastructureParameters, String[] infrastructureFileParameters, String policyType,
            String[] policyParameters, String[] policyFileParameters) throws RestServerException,
            ServiceException {

        RestClient cli = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = cli.createnodeSource(sessionId, nodeSourceName, infrastructureType,
                    infrastructureParameters, infrastructureFileParameters, policyType, policyParameters,
                    policyFileParameters);
            String ret = convertToString(clientResponse.getEntity());
            Status status = clientResponse.getResponseStatus();

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }

    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getInfrastructures(java.lang.String)
     */
    public String getInfrastructures(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.infrastructures(sessionId);
            String ret = convertToString(clientResponse.getEntity());
            Status status = clientResponse.getResponseStatus();

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getPolicies(java.lang.String)
     */
    public String getPolicies(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.policies(sessionId);
            String ret = convertToString(clientResponse.getEntity());
            Status status = clientResponse.getResponseStatus();

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#lockNodes(java.lang.String, java.util.Set)
     */
    public String lockNodes(String sessionId, Set<String> urls) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (String url : urls) {
            ClientResponse<InputStream> clientResponse = null;
            Set<String> singleton = new HashSet<String>();
            singleton.add(url);
            try {
                clientResponse = client.lockNodes(sessionId, singleton);
                Status status = clientResponse.getResponseStatus();

                switch (status) {
                    case OK:
                        success++;
                        break;
                    default:
                        failures++;
                }
            } finally {
                if (clientResponse != null) {
                    clientResponse.releaseConnection();
                }
            }
        }
        if (failures > 0) {
            throw new RestServerException("Failed to lock all requested nodes; " + success + " succeeded, " +
                failures + " failed.");
        }
        return "";
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#unlockNodes(java.lang.String, java.util.Set)
     */
    public String unlockNodes(String sessionId, Set<String> urls) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (String url : urls) {
            ClientResponse<InputStream> clientResponse = null;
            Set<String> singleton = new HashSet<String>();
            singleton.add(url);
            try {
                clientResponse = client.unlockNodes(sessionId, singleton);
                Status status = clientResponse.getResponseStatus();

                switch (status) {
                    case OK:
                        success++;
                        break;
                    default:
                        failures++;
                }
            } finally {
                if (clientResponse != null) {
                    clientResponse.releaseConnection();
                }
            }
        }
        if (failures > 0) {
            throw new RestServerException("Failed to unlock all requested nodes; " + success +
                " succeeded, " + failures + " failed.");
        }
        return "";
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#removeNode(java.lang.String, java.lang.String)
     */
    public String removeNode(String sessionId, String url, boolean force) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.removeNode(sessionId, url, force);
            Status status = clientResponse.getResponseStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#removeNodesource(java.lang.String, java.lang.String)
     */
    public String removeNodesource(String sessionId, String name, boolean preempt)
            throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.removeNodesource(sessionId, name, preempt);
            Status status = clientResponse.getResponseStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#releaseNode(java.lang.String, java.lang.String)
     */
    public String releaseNode(String sessionId, String url) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);

        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.releaseNode(sessionId, url);
            Status status = clientResponse.getResponseStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see Service#getVersion()
     */
    public String getVersion() throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.getVersion();
            Status status = clientResponse.getResponseStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getMBeanInfo(String sessionId, String name, List<String> attrs) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            ObjectName obj = new ObjectName(name);
            clientResponse = client.getMBeanInfo(sessionId, obj, attrs);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } catch (MalformedObjectNameException e) {
            throw new ServiceException("Malformed MBean name", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getNodeMBeanInfo(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs)
            throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.getNodeMBeanInfo(sessionId, nodeJmxUrl, objectName, attrs);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getNodeMBeanHistory(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs, String timeRange) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.getNodeMBeanHistory(sessionId, nodeJmxUrl, objectName, attrs, timeRange);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getNodeMBeansInfo(String sessionId, String nodeJmxUrl, String objectNames,
            List<String> attrs) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.getNodeMBeansInfo(sessionId, nodeJmxUrl, objectNames, attrs);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getNodeMBeansHistory(String sessionId, String nodeJmxUrl, String objectNames,
                                      List<String> attrs, String timeRange) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.getNodeMBeansHistory(sessionId, nodeJmxUrl, objectNames, attrs, timeRange);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getStatHistory(String sessionId, String range) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.getStatHistory(sessionId, range);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity());

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String executeNodeScript(String sessionId, String script, String engine, String nodeUrl) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;
        try {
            clientResponse = client.executeNodeScript(sessionId, nodeUrl, script, engine);
            int code = clientResponse.getStatus();
            String ret = convertToString(clientResponse.getEntity(), true);

            switch (code) {
                case 200:
                    return ret;
                default:
                    throw new RestServerException(code, ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

}
