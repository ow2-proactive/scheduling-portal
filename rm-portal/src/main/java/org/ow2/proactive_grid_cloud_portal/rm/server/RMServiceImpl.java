/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;

import org.ow2.proactive.http.HttpClientBuilder;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigReader;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMService;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.ByteArrayBody;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.jboss.resteasy.client.jaxrs.ResteasyClient;
import org.jboss.resteasy.client.jaxrs.ResteasyClientBuilder;
import org.jboss.resteasy.client.jaxrs.ResteasyWebTarget;
import org.jboss.resteasy.client.jaxrs.engines.ApacheHttpClient4Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.ow2.proactive_grid_cloud_portal.common.server.HttpUtils.convertToString;


/**
 * The server side implementation of the RPC service.
 */
@SuppressWarnings("serial")
public class RMServiceImpl extends Service implements RMService {

    private static final Logger LOGGER = LoggerFactory.getLogger(RMServiceImpl.class);

    /**
     * Number of threads created for the threadPool shared by RestEasy client proxies.
     */
    private static final int THREAD_POOL_SIZE = Runtime.getRuntime().availableProcessors() * 8;

    /**
     * Thread pool shared by RestEasy client proxies.
     */
    private ExecutorService threadPool;

    private CloseableHttpClient httpClient;

    @Override
    public void init() {
        loadProperties();

        Config config = Config.get();

        httpClient =
                new HttpClientBuilder()
                        .maxConnections(50)
                        .allowAnyCertificate(config.isHttpsAllowAnyCertificate())
                        .allowAnyHostname(config.isHttpsAllowAnyHostname())
                        .useSystemProperties().build();

        threadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
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
        RMConfig.get().load(
                ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(RMConfig.CONFIG_PATH)));
        ConfigUtils.loadSystemProperties(RMConfig.get());
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#logout(java.lang.String)
     */
    public void logout(String sessionId) throws ServiceException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            restClientProxy.logout(sessionId);
        } catch (WebApplicationException e) {
            throw new ServiceException(e.getMessage());
        }
    }

    /*
     * (non-Javadoc)
     * @see Service#login(java.lang.String, java.lang.String, java.io.File, java.lang.String)
     */
    public String login(String login, String pass, File cred, String ssh) throws RestServerException,
            ServiceException {
        HttpPost httpPost = new HttpPost(RMConfig.get().getRestUrl() + "/rm/login");

        try {
            MultipartEntity entity;
            if (cred == null) {
                entity = createLoginPasswordSSHKeyMultipart(login, pass, ssh);
            } else {
                entity = new MultipartEntity();
                entity.addPart("credential", new FileBody(cred, "application/octet-stream"));
            }
            httpPost.setEntity(entity);

            HttpResponse response = httpClient.execute(httpPost);
            String responseAsString = convertToString(response.getEntity().getContent());
            switch (response.getStatusLine().getStatusCode()) {
                case 200:
                    break;
                default:
                    if (responseAsString.trim().length() == 0) {
                        responseAsString = "{ \"httpErrorCode\": " + response.getStatusLine().getStatusCode() + "," + "\"errorMessage\": \"" +
                                response.getStatusLine().getReasonPhrase() + "\" }";
                    }
                    throw new RestServerException(response.getStatusLine().getStatusCode(), responseAsString);
            }
            return responseAsString;
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            httpPost.releaseConnection();
            if (cred != null) {
                cred.delete();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getState(java.lang.String)
     */
    public String getState(final String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.state(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getMonitoring(java.lang.String)
     */
    public String getMonitoring(final String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.monitoring(sessionId);
            }
        });
    }

    /**
     * Create a Credentials file with the provided authentication parameters
     *
     * @param login username
     * @param pass  password
     * @param ssh   private ssh key
     * @return the the Credentials file as a base64 String
     * @throws ServiceException
     */
    public String createCredentials(String login, String pass, String ssh) throws RestServerException,
            ServiceException {
        HttpPost httpPost = new HttpPost(RMConfig.get().getRestUrl() + "/scheduler/createcredential");

        try {
            MultipartEntity entity = createLoginPasswordSSHKeyMultipart(login, pass, ssh);

            httpPost.setEntity(entity);

            HttpResponse response = httpClient.execute(httpPost);
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
            httpPost.releaseConnection();
        }
    }

    private MultipartEntity createLoginPasswordSSHKeyMultipart(String login, String pass,
            String ssh) throws UnsupportedEncodingException {
        MultipartEntity entity = new MultipartEntity();
        entity.addPart("username", new StringBody(login));
        entity.addPart("password", new StringBody(pass));
        if (ssh != null && !ssh.isEmpty()) {
            entity.addPart("sshKey",
                    new ByteArrayBody(ssh.getBytes(), MediaType.APPLICATION_OCTET_STREAM, null));
        }
        return entity;
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#createNodeSource(java.lang.String, java.lang.String, java.lang.String, java.lang.String[], java.lang.String[], java.lang.String, java.lang.String[], java.lang.String[])
     */
    public String createNodeSource(final String sessionId, final String nodeSourceName,
            final String infrastructureType,
            final String[] infrastructureParameters, final String[] infrastructureFileParameters,
            final String policyType,
            final String[] policyParameters, final String[] policyFileParameters) throws RestServerException,
            ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.createnodeSource(sessionId, nodeSourceName, infrastructureType,
                        infrastructureParameters, infrastructureFileParameters, policyType, policyParameters,
                        policyFileParameters);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getInfrastructures(java.lang.String)
     */
    public String getInfrastructures(final String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.infrastructures(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#getPolicies(java.lang.String)
     */
    public String getPolicies(final String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.policies(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#lockNodes(java.lang.String, java.util.Set)
     */
    public String lockNodes(final String sessionId,
            Set<String> urls) throws RestServerException, ServiceException {
        return executeFunction(new BiFunction<RestClient, Set<String>, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient, Set<String> strings) {
                return restClient.lockNodes(sessionId, strings);
            }
        }, urls, "lock");
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#unlockNodes(java.lang.String, java.util.Set)
     */
    public String unlockNodes(final String sessionId, Set<String> urls) throws RestServerException,
            ServiceException {
        return executeFunction(new BiFunction<RestClient, Set<String>, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient, Set<String> strings) {
                return restClient.unlockNodes(sessionId, strings);
            }
        }, urls, "unlock");
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#removeNode(java.lang.String, java.lang.String)
     */
    public String removeNode(final String sessionId, final String url,
            final boolean force) throws RestServerException,
            ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.removeNode(sessionId, url, force);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#removeNodesource(java.lang.String, java.lang.String)
     */
    public String removeNodesource(final String sessionId, final String name, final boolean preempt)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.removeNodesource(sessionId, name, preempt);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.rm.client.RMService#releaseNode(java.lang.String, java.lang.String)
     */
    public String releaseNode(final String sessionId,
            final String url) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.releaseNode(sessionId, url);
            }
        });
    }

    /*
     * (non-Javadoc)
     * @see Service#getVersion()
     */
    public String getVersion() throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getVersion();
            }
        });
    }

    @Override
    public String getMBeanInfo(final String sessionId, String name,
            final List<String> attrs) throws RestServerException,
            ServiceException {
        try {
            final ObjectName obj = new ObjectName(name);

            return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
                @Override
                public InputStream apply(RestClient restClient) {
                    return restClient.getMBeanInfo(sessionId, obj, attrs);
                }
            });
        } catch (MalformedObjectNameException e) {
            throw new ServiceException("Malformed MBean name", e);
        }
    }

    @Override
    public String getNodeMBeanInfo(final String sessionId, final String nodeJmxUrl, final String objectName,
            final List<String> attrs)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getNodeMBeanInfo(sessionId, nodeJmxUrl, objectName, attrs);
            }
        });
    }

    @Override
    public String getNodeMBeanHistory(final String sessionId, final String nodeJmxUrl,
            final String objectName, final List<String> attrs,
            final String timeRange) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getNodeMBeanHistory(sessionId, nodeJmxUrl, objectName, attrs, timeRange);
            }
        });
    }

    @Override
    public String getNodeMBeansInfo(final String sessionId, final String nodeJmxUrl, final String objectNames,
            final List<String> attrs) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getNodeMBeansInfo(sessionId, nodeJmxUrl, objectNames, attrs);
            }
        });
    }

    @Override
    public String getNodeMBeansHistory(final String sessionId, final String nodeJmxUrl,
            final String objectNames,
            final List<String> attrs, final String timeRange) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getNodeMBeansHistory(sessionId, nodeJmxUrl, objectNames, attrs, timeRange);
            }
        });
    }

    @Override
    public String getStatHistory(final String sessionId,
            final String range) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getStatHistory(sessionId, range);
            }
        });
    }

    @Override
    public String executeNodeScript(final String sessionId, final String script, final String engine,
            final String nodeUrl) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.executeNodeScript(sessionId, nodeUrl, script, engine);
            }
        });
    }

    private RestClient getRestClientProxy() {
        ResteasyClient client =
                new ResteasyClientBuilder().asyncExecutor(threadPool)
                        .httpEngine(new ApacheHttpClient4Engine(httpClient)).build();

        ResteasyWebTarget target = client.target(RMConfig.get().getRestUrl());

        return target.proxy(RestClient.class);
    }

    private String executeFunction(BiFunction<RestClient, Set<String>, InputStream> action, Set<String> urls,
            String actionName) throws ServiceException, RestServerException {

        RestClient restClientProxy = getRestClientProxy();

        int failures = 0;
        int success = 0;

        for (String url : urls) {
            InputStream inputStream = null;

            try {
                inputStream = action.apply(restClientProxy, Collections.singleton(url));
                success++;
            } catch (WebApplicationException e) {
                failures++;
            } finally {
                IOUtils.closeQuietly(inputStream);
            }
        }

        if (failures > 0) {
            throw new RestServerException(
                    "Failed to " + actionName + " all requested nodes: " + success + " succeeded, " + failures + " failed.");
        }

        return "";
    }

    private String executeFunctionReturnStreamAsString(
            Function<RestClient, InputStream> function) throws ServiceException, RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);

            try {
                return convertToString(inputStream);
            } catch (IOException e) {
                throw new ServiceException(e.getMessage());
            }
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    private String rethrowRestServerException(WebApplicationException e) throws RestServerException {
        throw new RestServerException(e.getResponse().getStatus(), e.getMessage());
    }

    private interface BiFunction<T, U, R> {

        R apply(T t, U u);

    }

    private interface Function<T, R> {

        R apply(T t);

    }

}
