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

import static org.ow2.proactive_grid_cloud_portal.common.shared.HttpUtils.convertToString;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.ws.rs.core.Response.Status;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.methods.multipart.FilePart;
import org.apache.commons.httpclient.methods.multipart.MultipartRequestEntity;
import org.apache.commons.httpclient.methods.multipart.Part;
import org.apache.commons.httpclient.methods.multipart.StringPart;
import org.jboss.resteasy.client.ClientExecutor;
import org.jboss.resteasy.client.ClientResponse;
import org.jboss.resteasy.client.ProxyFactory;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.HttpUtils;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.common.shared.User;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMService;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

/**
 * The server side implementation of the RPC service.
 */
@SuppressWarnings("serial")
public class RMServiceImpl extends Service implements RMService {

    /** current users */
    private Map<String, User> users;

    private ClientExecutor executor;

    /**
     * Default constructor
     */
    public RMServiceImpl() {
        super();
        this.users = new HashMap<String, User>();
    }

    @Override
    public void init() {
        loadProperties();

        executor = HttpUtils.createDefaultExecutor();
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

        /**
         * Loads the Default properties written in the config file
         */
        java.util.Properties properties = new java.util.Properties();
        String path = getServletContext().getRealPath(RMConfig.CONFIG_PATH);
        try {
            properties.load(new FileInputStream(new File(path)));
        } catch (Exception e) {
            System.out.println("Failed to load config from file: " + path);
            e.printStackTrace();
        }
        HashMap<String, String> props = new HashMap<String, String>();
        Set<Entry<Object, Object>> entries = properties.entrySet();
        for (Entry<Object, Object> entry : entries) {
            String name = (String) entry.getKey();
            String value = (String) entry.getValue();
            props.put(name, value);
        }
        RMConfig.get().load(props);
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#logout(java.lang.String)
     */
    public void logout(String sessionId) throws ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, RMConfig.get().getRestUrl(), executor);
        User user = this.users.remove(sessionId);
        if (user != null) {
            client.logout(sessionId);
        } else {
            throw new ServiceException("No user for session id '" + sessionId + "'");
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.common.server.Service#login(java.lang.String, java.lang.String, java.io.File, java.lang.String)
     */
    public String login(String login, String pass, File cred, String ssh) throws RestServerException,
            ServiceException {
        PostMethod method = new PostMethod(RMConfig.get().getRestUrl() + "/rm/login");

        try {
            Part[] parts = null;
            if (cred == null) {
                parts = new Part[] { new StringPart("username", login), new StringPart("password", pass),
                        new StringPart("sshkey", ssh) };
            } else {
                parts = new Part[] { new FilePart("credential", cred) };
            }

            method.setRequestEntity(new MultipartRequestEntity(parts, method.getParams()));
            HttpClient httpClient = new HttpClient();
            int status = httpClient.executeMethod(method);
            String response = convertToString(method.getResponseBodyAsStream());

            switch (status) {
                case 200:
                    User user = new User(login);
                    user.setSessionId(response);
                    this.users.put(response, user);
                    break;
                default:
                    throw new RestServerException(status, response);
            }
            return response;

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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
        PostMethod method = new PostMethod(RMConfig.get().getRestUrl() + "/scheduler/createcredential");

        try {
            Part[] parts = new Part[] { new StringPart("username", login), new StringPart("password", pass),
                    new StringPart("sshkey", ssh) };

            method.setRequestEntity(new MultipartRequestEntity(parts, method.getParams()));

            HttpClient httpClient = new HttpClient();

            int status = httpClient.executeMethod(method);
            String response = convertToString(method.getResponseBodyAsStream());

            switch (status) {
                case 200:
                    return response;
                default:
                    throw new RestServerException(status, response);
            }
        } catch (Exception e) {
            e.printStackTrace();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
                clientResponse.releaseConnection();
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
                clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.common.server.Service#getVersion()
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
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
            clientResponse.releaseConnection();
        }
    }

    @Override
    public void checkPermutationStrongName() {
        /* FIXME
         * disable the check for XSRF attack, which finds false positives and refuses to serve
         * requests to some clients, for no apparent reason:
         * >> java.lang.SecurityException: Blocked request without GWT permutation header (XSRF attack?)
         * >> at com.google.gwt.user.server.rpc.RemoteServiceServlet.checkPermutationStrongName(RemoteServiceServlet.java:267)
         * 
         * This may be fixed in later versions of GWT, just remove this method to restore the original behaviour
         */
        return;
    }
}
