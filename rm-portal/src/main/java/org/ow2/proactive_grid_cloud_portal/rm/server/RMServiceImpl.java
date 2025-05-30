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

import static org.ow2.proactive_grid_cloud_portal.common.server.HttpUtils.*;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
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
import org.jboss.resteasy.plugins.interceptors.encoding.AcceptEncodingGZIPFilter;
import org.jboss.resteasy.plugins.interceptors.encoding.GZIPDecodingInterceptor;
import org.jboss.resteasy.plugins.interceptors.encoding.GZIPEncodingInterceptor;
import org.ow2.proactive.http.HttpClientBuilder;
import org.ow2.proactive_grid_cloud_portal.common.server.CommonRestClient;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigReader;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.rm.client.RMService;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


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

        httpClient = new HttpClientBuilder().maxConnections(50)
                                            .allowAnyCertificate(config.isHttpsAllowAnyCertificate())
                                            .allowAnyHostname(config.isHttpsAllowAnyHostname())
                                            .useSystemProperties()
                                            .build();

        threadPool = Executors.newFixedThreadPool(THREAD_POOL_SIZE);
    }

    @Override
    public Map<String, String> getProperties() {
        return RMConfig.get().getProperties();
    }

    /**
     * Loads properties defined in the configuration file and in JVM arguments.
     */
    private void loadProperties() {
        RMConfig.get().load(ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(RMConfig.HELP_PATH)));
        RMConfig.get().load(ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(RMConfig.CONFIG_PATH)));
        ConfigUtils.loadSystemProperties(RMConfig.get());
    }

    @Override
    public void logout(String sessionId) throws ServiceException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            restClientProxy.logout(sessionId);
        } catch (WebApplicationException e) {
            throw new ServiceException(e.getMessage());
        }
    }

    @Override
    public String portalAccess(String sessionId) throws ServiceException, RestServerException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.portalAccess(sessionId,
                                                                                                        "rm"));
    }

    @Override
    public List<String> portalsAccess(String sessionId, List<String> portals)
            throws ServiceException, RestServerException {
        return executeFunctionReturnStreamAsListCommon(restClient -> restClient.portalsAccess(sessionId, portals));
    }

    @Override
    public String login(String login, String pass, File cred, String ssh) throws RestServerException, ServiceException {
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
            responseAsString = handleResponseStatus(response, responseAsString);
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

    private String handleResponseStatus(HttpResponse response, String responseAsString) throws RestServerException {
        switch (response.getStatusLine().getStatusCode()) {
            case 200:
                break;
            default:
                if (responseAsString.trim().length() == 0) {
                    responseAsString = "{ \"httpErrorCode\": " + response.getStatusLine().getStatusCode() + "," +
                                       "\"errorMessage\": \"" + response.getStatusLine().getReasonPhrase() + "\" }";
                }
                throw new RestServerException(response.getStatusLine().getStatusCode(), responseAsString);
        }
        return responseAsString;
    }

    @Override
    public String getLoginFromSessionId(String sessionId) throws RestServerException, ServiceException {
        HttpGet method = new HttpGet(RMConfig.get().getRestUrl() + "/rm/logins/sessionid/" + sessionId);
        try {
            HttpResponse response = httpClient.execute(method);
            String responseAsString = convertToString(response.getEntity().getContent());
            handleResponseStatus(response, responseAsString);
            return responseAsString;
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            method.releaseConnection();
        }
    }

    @Override
    public String getState(String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.state(sessionId));
    }

    @Override
    public String getCurrentLoggers(String sessionId) throws ServiceException, RestServerException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getCurrentLoggers(sessionId));
    }

    @Override
    public String setLogLevelMultiple(String sessionId, Map<String, String> loggersConfiguration)
            throws ServiceException, RestServerException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.setLogLevelMultiple(sessionId,
                                                                                                               loggersConfiguration));
    }

    @Override
    public String getMonitoring(String sessionId, Long counter) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.monitoring(sessionId,
                                                                                                      counter.toString()));
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
    public String createCredentials(String login, String pass, String ssh)
            throws RestServerException, ServiceException {
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

    private MultipartEntity createLoginPasswordSSHKeyMultipart(String login, String pass, String ssh)
            throws UnsupportedEncodingException {
        MultipartEntity entity = new MultipartEntity();
        entity.addPart("username", new StringBody(login));
        entity.addPart("password", new StringBody(pass));
        if (ssh != null && !ssh.isEmpty()) {
            entity.addPart("sshKey", new ByteArrayBody(ssh.getBytes(), MediaType.APPLICATION_OCTET_STREAM, null));
        }
        return entity;
    }

    @Override
    public String defineNodeSource(String sessionId, String nodeSourceName, String infrastructureType,
            String[] infrastructureParameters, String[] infrastructureFileParameters, String policyType,
            String[] policyParameters, String[] policyFileParameters, String nodesRecoverable)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.defineNodeSource(sessionId,
                                                                                                            nodeSourceName,
                                                                                                            infrastructureType,
                                                                                                            infrastructureParameters,
                                                                                                            infrastructureFileParameters,
                                                                                                            policyType,
                                                                                                            policyParameters,
                                                                                                            policyFileParameters,
                                                                                                            nodesRecoverable));
    }

    @Override
    public String editNodeSource(String sessionId, String nodeSourceName, String infrastructureType,
            String[] infrastructureParameters, String[] infrastructureFileParameters, String policyType,
            String[] policyParameters, String[] policyFileParameters, String nodesRecoverable)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.editNodeSource(sessionId,
                                                                                                          nodeSourceName,
                                                                                                          infrastructureType,
                                                                                                          infrastructureParameters,
                                                                                                          infrastructureFileParameters,
                                                                                                          policyType,
                                                                                                          policyParameters,
                                                                                                          policyFileParameters,
                                                                                                          nodesRecoverable));
    }

    @Override
    public String updateDynamicParameters(String sessionId, String nodeSourceName, String infrastructureType,
            String[] infrastructureParameters, String[] infrastructureFileParameters, String policyType,
            String[] policyParameters, String[] policyFileParameters) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.updateDynamicParameters(sessionId,
                                                                                                                   nodeSourceName,
                                                                                                                   infrastructureType,
                                                                                                                   infrastructureParameters,
                                                                                                                   infrastructureFileParameters,
                                                                                                                   policyType,
                                                                                                                   policyParameters,
                                                                                                                   policyFileParameters));
    }

    @Override
    public String deployNodeSource(String sessionId, String nodeSourceName)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.deployNodeSource(sessionId,
                                                                                                            nodeSourceName));
    }

    @Override
    public String undeployNodeSource(String sessionId, String nodeSourceName, boolean force)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.undeployNodeSource(sessionId,
                                                                                                              nodeSourceName,
                                                                                                              force));
    }

    @Override
    public String redeployNodeSource(String sessionId, String nodeSourceName)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.redeployNodeSource(sessionId,
                                                                                                              nodeSourceName));
    }

    @Override
    public String getInfrastructures(String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.infrastructures(sessionId));
    }

    @Override
    public String getPolicies(String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.policies(sessionId));
    }

    public String getInfrasToPoliciesMapping(String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getInfrasToPoliciesMapping(sessionId));
    }

    @Override
    public String getNodeSourceConfiguration(String sessionId, String nodeSourceName)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getNodeSourceConfiguration(sessionId,
                                                                                                                      nodeSourceName));
    }

    @Override
    public String lockNodes(String sessionId, Set<String> urls) throws RestServerException, ServiceException {
        return executeFunction((restClient, strings) -> restClient.lockNodes(sessionId, strings), urls, "lock");
    }

    @Override
    public String unlockNodes(String sessionId, Set<String> urls) throws RestServerException, ServiceException {
        return executeFunction((restClient, strings) -> restClient.unlockNodes(sessionId, strings), urls, "unlock");
    }

    @Override
    public String removeNode(String sessionId, String url, boolean force) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.removeNode(sessionId,
                                                                                                      url,
                                                                                                      force));
    }

    @Override
    public String removeNodesource(String sessionId, String name, boolean preempt)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.removeNodesource(sessionId,
                                                                                                            name,
                                                                                                            preempt));
    }

    @Override
    public String releaseNode(String sessionId, String url) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.releaseNode(sessionId, url));
    }

    @Override
    public String getVersion() throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(RestClient::getVersion);
    }

    @Override
    public String getMBeanInfo(String sessionId, String name, List<String> attrs)
            throws RestServerException, ServiceException {
        try {
            final ObjectName obj = new ObjectName(name);

            return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getMBeanInfo(sessionId,
                                                                                                            obj,
                                                                                                            attrs));
        } catch (MalformedObjectNameException e) {
            throw new ServiceException("Malformed MBean name", e);
        }
    }

    @Override
    public String getNodeMBeanInfo(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getNodeMBeanInfo(sessionId,
                                                                                                            nodeJmxUrl,
                                                                                                            objectName,
                                                                                                            attrs));
    }

    @Override
    public String getNodeMBeanHistory(String sessionId, String nodeJmxUrl, String objectName, List<String> attrs,
            String timeRange) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getNodeMBeanHistory(sessionId,
                                                                                                               nodeJmxUrl,
                                                                                                               objectName,
                                                                                                               attrs,
                                                                                                               timeRange));
    }

    @Override
    public String getNodeMBeansInfo(String sessionId, String nodeJmxUrl, String objectNames, List<String> attrs)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getNodeMBeansInfo(sessionId,
                                                                                                             nodeJmxUrl,
                                                                                                             objectNames,
                                                                                                             attrs));
    }

    @Override
    public String getNodeMBeansHistory(String sessionId, String nodeJmxUrl, String objectNames, List<String> attrs,
            String timeRange) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getNodeMBeansHistory(sessionId,
                                                                                                                nodeJmxUrl,
                                                                                                                objectNames,
                                                                                                                attrs,
                                                                                                                timeRange));
    }

    @Override
    public String getStatHistory(String sessionId, String range) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.getStatHistory(sessionId,
                                                                                                          range));
    }

    @Override
    public String executeNodeScript(String sessionId, String script, String engine, String nodeUrl)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.executeNodeScript(sessionId,
                                                                                                             nodeUrl,
                                                                                                             script,
                                                                                                             engine));
    }

    @Override
    public String executeNodeSourceScript(String sessionId, String script, String engine, String nodeSourceName)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.executeNodeSourceScript(sessionId,
                                                                                                                   nodeSourceName,
                                                                                                                   script,
                                                                                                                   engine));
    }

    @Override
    public String executeHostScript(String sessionId, String script, String engine, String hostname)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.executeHostScript(sessionId,
                                                                                                             hostname,
                                                                                                             script,
                                                                                                             engine));
    }

    @Override
    public String getRMThreadDump(String sessionId) throws ServiceException, RestServerException {
        return executeFunctionReturnStreamAsString(restClient -> restClient.getRMThreadDump(sessionId));
    }

    @Override
    public String getNodeThreadDump(String sessionId, String nodeUrl) throws ServiceException, RestServerException {
        return executeFunctionReturnStreamAsString(restClient -> restClient.getNodeThreadDump(sessionId, nodeUrl));
    }

    @Override
    public void setNodeTokens(String sessionId, String nodeurl, List<String> tokens) {
        RestClient restClientProxy = getRestClientProxy();
        restClientProxy.setNodeTokens(sessionId, nodeurl, tokens);
    }

    @Override
    public String checkNodePermission(String sessionId, String nodeUrl, boolean provider)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.checkNodePermission(sessionId,
                                                                                                               nodeUrl,
                                                                                                               provider));
    }

    @Override
    public String checkNodeSourcePermission(String sessionId, String nodeSourceName, boolean provider)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringWithoutNewLines(restClient -> restClient.checkNodeSourcePermission(sessionId,
                                                                                                                     nodeSourceName,
                                                                                                                     provider));
    }

    private Map<String, Boolean> executeFunctionReturnStreamAsMapWithoutNewLines(
            Function<RestClient, InputStream> function) throws ServiceException, RestServerException {
        RestClient restClientProxy = getRestClientProxy();
        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);

            try {
                return convertToHashMap(inputStream);
            } catch (IOException e) {
                throw new ServiceException(e.getMessage());
            }
        } catch (WebApplicationException e) {
            HashMap map = new HashMap<String, Boolean>();
            map.put(rethrowRestServerException(e), null);
            return map;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    @Override
    public Map<String, Boolean> checkMethodsPermissions(final String sessionId, List<String> methods)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsMapCommon(restClient -> restClient.checkMethodsPermissions(sessionId,
                                                                                                       methods));
    }

    @Override
    public String getCurrentUserData(String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsStringCommon(restClient -> restClient.getCurrentUserData(sessionId));
    }

    @Override
    public List<String> getDomains() throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsList(RestClient::getDomains);

    }

    private Map<String, Boolean>
            executeFunctionReturnStreamAsMapCommon(java.util.function.Function<CommonRestClient, InputStream> function)
                    throws ServiceException, RestServerException {
        CommonRestClient restClientProxy = getCommonRestClient();

        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);

            try {
                return convertToHashMap(inputStream);
            } catch (IOException e) {
                throw new ServiceException(e.getMessage());
            }
        } catch (WebApplicationException e) {
            HashMap map = new HashMap<String, Boolean>();
            map.put(rethrowRestServerException(e), null);
            return map;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    private List<String>
            executeFunctionReturnStreamAsListCommon(java.util.function.Function<CommonRestClient, InputStream> function)
                    throws ServiceException, RestServerException {
        CommonRestClient restClientProxy = getCommonRestClient();

        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);

            try {
                return convertToList(inputStream);
            } catch (IOException e) {
                throw new ServiceException(e.getMessage());
            }
        } catch (WebApplicationException e) {
            List list = new ArrayList<String>();
            list.add(rethrowRestServerException(e));
            return list;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    private List<String>
            executeFunctionReturnStreamAsList(java.util.function.Function<RestClient, InputStream> function)
                    throws ServiceException, RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);

            try {
                return convertToList(inputStream);
            } catch (IOException e) {
                throw new ServiceException(e.getMessage());
            }
        } catch (WebApplicationException e) {
            List list = new ArrayList<String>();
            list.add(rethrowRestServerException(e));
            return list;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    private CommonRestClient getCommonRestClient() {
        ResteasyClientBuilder builder = new ResteasyClientBuilder();
        builder.register(AcceptEncodingGZIPFilter.class);
        builder.register(GZIPDecodingInterceptor.class);
        builder.register(GZIPEncodingInterceptor.class);
        ResteasyClient client = builder.asyncExecutor(threadPool)
                                       .httpEngine(new ApacheHttpClient4Engine(httpClient))
                                       .build();
        ResteasyWebTarget target = client.target(RMConfig.get().getRestUrl());

        return target.proxy(CommonRestClient.class);
    }

    private RestClient getRestClientProxy() {
        ResteasyClientBuilder builder = new ResteasyClientBuilder();
        builder.register(AcceptEncodingGZIPFilter.class);
        builder.register(GZIPDecodingInterceptor.class);
        builder.register(GZIPEncodingInterceptor.class);

        ResteasyClient client = builder.asyncExecutor(threadPool)
                                       .httpEngine(new ApacheHttpClient4Engine(httpClient))
                                       .build();

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
            throw new RestServerException("Failed to " + actionName + " all requested nodes: " + success +
                                          " succeeded, " + failures + " failed.");
        }

        return "";
    }

    private String executeFunctionReturnStreamAsStringWithoutNewLines(Function<RestClient, InputStream> function)
            throws ServiceException, RestServerException {
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

    private String executeFunctionReturnStreamAsString(Function<RestClient, InputStream> function)
            throws ServiceException, RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);

            try {
                return convertToString(inputStream, true);
            } catch (IOException e) {
                throw new ServiceException(e.getMessage());
            }
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    private String executeFunctionReturnStreamAsStringCommon(
            java.util.function.Function<CommonRestClient, InputStream> function)
            throws ServiceException, RestServerException {
        CommonRestClient restClientProxy = getCommonRestClient();

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
