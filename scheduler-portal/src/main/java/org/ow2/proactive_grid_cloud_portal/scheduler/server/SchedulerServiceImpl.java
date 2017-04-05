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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import static org.ow2.proactive_grid_cloud_portal.common.server.HttpUtils.convertToString;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.ByteArrayBody;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.CloseableHttpClient;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.jboss.resteasy.client.jaxrs.ResteasyClient;
import org.jboss.resteasy.client.jaxrs.ResteasyClientBuilder;
import org.jboss.resteasy.client.jaxrs.ResteasyWebTarget;
import org.jboss.resteasy.client.jaxrs.engines.ApacheHttpClient4Engine;
import org.ow2.proactive.http.HttpClientBuilder;
import org.ow2.proactive.scheduling.api.graphql.beans.input.Query;
import org.ow2.proactive.scheduling.api.graphql.client.SchedulingApiClientGwt;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigReader;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobUsage;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.OutputMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerPortalDisplayConfig;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SharedProperties;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;


/**
 * The server side implementation of the RPC service.
 */
@SuppressWarnings("serial")
public class SchedulerServiceImpl extends Service implements SchedulerService {

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mmZ";

    private CloseableHttpClient httpClient;

    /**
     * Number of threads created for the threadPool shared by RestEasy client proxies.
     */
    private static final int THREAD_POOL_SIZE = Runtime.getRuntime().availableProcessors() * 8;

    /**
     * Thread pool shared by RestEasy client proxies.
     */
    private ExecutorService threadPool;

    /**
     * GraphQL Client
     */
    private SchedulingApiClientGwt graphQLClient;

    /**
     * JSON Mapper
     */
    private final static ObjectMapper JSON_MAPPER = new ObjectMapper();

    /**
     * LOGGER
     */
    private final static Logger LOGGER = Logger.getLogger(SchedulerServiceImpl.class.getName());

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

        graphQLClient = new SchedulingApiClientGwt(SchedulerConfig.get().getSchedulingApiUrl(), httpClient, threadPool);
    }

    /**
     * Loads properties defined in the configuration file and in JVM arguments.
     */
    private void loadProperties() {
        final Map<String, String> props = ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(SchedulerConfig.CONFIG_PATH));
        SchedulerConfig.get().load(props);
        ConfigUtils.loadSystemProperties(SchedulerConfig.get());

        final Map<String, String> portalProperties = ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(SchedulerPortalDisplayConfig.CONFIG_PATH));
        SchedulerPortalDisplayConfig.get().load(portalProperties);
    }

    /**
     * Submits a XML file to the REST part by using an HTTP client.
     *
     * @param sessionId the id of the client which submits the job
     * @param file      the XML file that is submitted
     * @return an error message upon failure, "id=<jobId>" upon success
     * @throws RestServerException
     * @throws ServiceException
     */
    public String submitXMLFile(String sessionId, File file) throws RestServerException, ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/submit");
        method.addHeader("sessionId", sessionId);

        boolean isJar = isJarFile(file);

        try {
            String name = isJar ? "jar" : "file";
            String mime = isJar ? "application/java-archive" : "application/xml";
            String charset = "ISO-8859-1";

            MultipartEntity entity = new MultipartEntity();
            entity.addPart("file", new FileBody(file, name, mime, charset));
            method.setEntity(entity);

            HttpResponse execute = httpClient.execute(method);
            InputStream is = execute.getEntity().getContent();
            String ret = convertToString(is);

            if (execute.getStatusLine().getStatusCode() == Response.Status.OK.getStatusCode()) {
                return ret;
            } else {
                throw new RestServerException(execute.getStatusLine().getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read response: " + e.getMessage());
        } finally {
            method.releaseConnection();
            if (file != null) {
                file.delete();
            }
        }
    }

    /**
     * Validate a XML file to the REST part by using an HTTP client.
     *
     * @param sessionId the id of the client which submits the job
     * @param file      the XML file that is submitted
     * @return an error message upon failure, "id=<jobId>" upon success
     * @throws RestServerException
     * @throws ServiceException
     */
    public String validateXMLFile(String sessionId, File file) throws RestServerException, ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/validate");
        method.addHeader("sessionId", sessionId);

        boolean isJar = isJarFile(file);

        try {
            String name = isJar ? "jar" : "file";
            String mime = isJar ? "application/java-archive" : "application/xml";
            String charset = "ISO-8859-1";

            MultipartEntity entity = new MultipartEntity();
            entity.addPart("file", new FileBody(file, name, mime, charset));
            method.setEntity(entity);

            HttpResponse execute = httpClient.execute(method);
            InputStream is = execute.getEntity().getContent();
            String ret = convertToString(is);

            if (execute.getStatusLine().getStatusCode() == Response.Status.OK.getStatusCode()) {
                return ret;
            } else {
                throw new RestServerException(execute.getStatusLine().getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException("Failed to read response: " + e.getMessage());
        } finally {
            method.releaseConnection();
            if (file != null) {
                file.delete();
            }
        }
    }

    private boolean isJarFile(File file) {
        try {
            new JarFile(file);
            return true;
        } catch (IOException e1) {
            return false;
        }
    }

    /**
     * Submit flat command file
     *
     * @param sessionId                current session
     * @param commandFileContent       content of the command file: endline
     *                                 separated native commands
     * @param jobName                  name of the job to create
     * @param selectionScriptContent   selection script content, or null
     * @param selectionScriptExtension selection script extension for script
     *                                 engine detection ("js", "py", "rb")
     * @return JobId of created Job as JSON
     * @throws RestServerException
     * @throws ServiceException
     */
    public String submitFlatJob(String sessionId, String commandFileContent, String jobName,
            String selectionScriptContent, String selectionScriptExtension)
            throws RestServerException, ServiceException {

        try {
            return getRestClientProxy().submitFlat(sessionId,
                                                   commandFileContent,
                                                   jobName,
                                                   selectionScriptContent,
                                                   selectionScriptExtension);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
        }

        return null;
    }

    /**
     * Getter of the result of a task.
     *
     * @param sessionId the session id of the user which is logged in
     * @param jobId     the id of the job the task belongs to
     * @param taskId    the id of the task
     * @return the result
     * @throws RestServerException
     * @throws ServiceException
     */
    public InputStream getTaskResult(String sessionId, String jobId, String taskId)
            throws RestServerException, ServiceException {
        try {
            return getRestClientProxy().taskresult(sessionId, jobId, taskId);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return null;
        }
    }

    /**
     * Getter of the serialized result of a task.
     *
     * @param sessionId the session id of the user which is looged in
     * @param jobId     the id of the job the task belongs to
     * @param taskId    the id of the task
     * @return the serialized result
     * @throws RestServerException
     * @throws ServiceException
     */
    public InputStream getTaskSerializedResult(String sessionId, String jobId, String taskId)
            throws RestServerException, ServiceException {
        try {
            return getRestClientProxy().taskSerializedResult(sessionId, jobId, taskId);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return null;
        }
    }

    /**
     * Getter of the result metadata of a task.
     *
     * @param sessionId the session id of the user which is looged in
     * @param jobId     the id of the job the task belongs to
     * @param taskId    the id of the task
     * @return the result metadata
     * @throws RestServerException
     * @throws ServiceException
     */
    public String getTaskResultMetadata(final String sessionId, final String jobId, final String taskId)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.taskResultMetadata(sessionId, jobId, taskId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#removeJobs(java.lang
     * .String, java.util.List)
     */
    @Override
    public int removeJobs(final String sessionId, List<Integer> jobIdList)
            throws RestServerException, ServiceException {
        return executeFunction(new BiFunction<RestClient, Integer, InputStream>() {
            @Override
            public InputStream apply(RestClient restClientProxy, Integer jobId) {
                return restClientProxy.removeJob(sessionId, Integer.toString(jobId));
            }
        }, jobIdList, "job removal");
    }

    @Override
    public int pauseJobs(final String sessionId, List<Integer> jobIdList) throws RestServerException, ServiceException {
        return executeFunction(new BiFunction<RestClient, Integer, InputStream>() {
            @Override
            public InputStream apply(RestClient restClientProxy, Integer jobId) {
                return restClientProxy.pauseJob(sessionId, Integer.toString(jobId));
            }
        }, jobIdList, "job paused");
    }

    @Override
    public int restartAllInErrorTasks(final String sessionId, List<Integer> jobIdList)
            throws RestServerException, ServiceException {
        return executeFunction(new BiFunction<RestClient, Integer, InputStream>() {
            @Override
            public InputStream apply(RestClient restClientProxy, Integer jobId) {
                return restClientProxy.restartAllTasksInError(sessionId, Integer.toString(jobId));
            }
        }, jobIdList, "restart all in error tasks in a job");
    }

    @Override
    public int resumeJobs(final String sessionId, List<Integer> jobIdList)
            throws RestServerException, ServiceException {
        return executeFunction(new BiFunction<RestClient, Integer, InputStream>() {
            @Override
            public InputStream apply(RestClient restClientProxy, Integer jobId) {
                return restClientProxy.resumeJob(sessionId, Integer.toString(jobId));
            }
        }, jobIdList, "job resumed");
    }

    @Override
    public int killJobs(final String sessionId, List<Integer> jobIdList) throws RestServerException, ServiceException {
        return executeFunction(new BiFunction<RestClient, Integer, InputStream>() {
            @Override
            public InputStream apply(RestClient restClientProxy, Integer jobId) {
                return restClientProxy.killJob(sessionId, Integer.toString(jobId));
            }
        }, jobIdList, "job killed");
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#setPriorityByName(java
     * .lang.String, java.util.List, java.lang.String)
     */
    @Override
    public void setPriorityByName(final String sessionId, List<Integer> jobIdList, final String priorityName)
            throws ServiceException, RestServerException {
        executeFunction(new BiFunction<RestClient, Integer, InputStream>() {
            @Override
            public InputStream apply(RestClient restClientProxy, Integer jobId) {
                return restClientProxy.schedulerChangeJobPriorityByName(sessionId,
                                                                        Integer.toString(jobId),
                                                                        priorityName);
            }
        }, jobIdList, "job set to priority " + priorityName);
    }

    /**
     * Login to the scheduler using a Credentials file
     *
     * @return the sessionId which can be parsed as an Integer, or an error
     * message
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String login(String login, String pass, File cred, String ssh) throws RestServerException, ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/login");

        try {
            MultipartEntity entity;
            if (cred == null) {
                entity = createLoginPasswordSSHKeyMultipart(login, pass, ssh);
            } else {
                entity = new MultipartEntity();
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
                    if (message.trim().length() == 0) {
                        message = "{ \"httpErrorCode\": " + response.getStatusLine().getStatusCode() + "," +
                                  "\"errorMessage\": \"" + response.getStatusLine().getReasonPhrase() + "\" }";
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

    /**
     * Create a Credentials file with the provided authentication parameters
     *
     * @param login username
     * @param pass  password
     * @param ssh   private ssh key
     * @return the the Credentials file as a base64 String
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String createCredentials(String login, String pass, String ssh)
            throws RestServerException, ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/createcredential");

        try {
            MultipartEntity entity = createLoginPasswordSSHKeyMultipart(login, pass, ssh);
            method.setEntity(entity);

            HttpResponse response = httpClient.execute(method);
            String responseAsString = convertToString(response.getEntity().getContent());

            switch (response.getStatusLine().getStatusCode()) {
                case 200:
                    return responseAsString;
                default:
                    throw new RestServerException(response.getStatusLine().getStatusCode(), responseAsString);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            method.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#logout(java.lang.String
     * )
     */
    @Override
    public void logout(String sessionId) throws RestServerException {
        getRestClientProxy().disconnect(sessionId);
    }

    @Override
    public boolean killTask(final String sessionId, final Integer jobId, final String taskName)
            throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.killTask(sessionId, jobId.toString(), taskName);
            }
        });
    }

    @Override
    public boolean restartRunningTask(final String sessionId, final Integer jobId, final String taskName)
            throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.restartTask(sessionId, jobId.toString(), taskName);
            }
        });
    }

    @Override
    public boolean restartInErrorTask(final String sessionId, final Integer jobId, final String taskName)
            throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.restartInErrorTask(sessionId, jobId.toString(), taskName);
            }
        });
    }

    @Override
    public boolean preemptTask(final String sessionId, final Integer jobId, final String taskName)
            throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.preemptTask(sessionId, jobId.toString(), taskName);
            }
        });
    }

    public boolean markAsFinishedAndResume(final String sessionId, final Integer jobId, final String taskName)
            throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.markAsFinishedAndResume(sessionId, jobId.toString(), taskName);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @seeimit
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getTasks(java.lang.
     * String, java.lang.String)
     */
    @Override
    public String getTasks(final String sessionId, final String jobId, final int offset, final int limit)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getJobTaskStatesPaginated(sessionId, jobId, offset, limit);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getTasksByTag(java.
     * lang. String, java.lang.String, java.lang.String)
     */
    @Override
    public String getTasksByTag(final String sessionId, final String jobId, final String tag, final int offset,
            final int limit) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getJobTaskStatesByTagPaginated(sessionId, jobId, tag, offset, limit);
            }
        });
    }

    public String getTaskCentric(final String sessionId, final long fromDate, final long toDate, final boolean myTasks,
            final boolean pending, final boolean running, final boolean finished, final int offset, final int limit,
            final TasksCentricController.SortSpecifierRestContainer sortParameters)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getTaskStates(sessionId,
                                                fromDate,
                                                toDate,
                                                myTasks,
                                                running,
                                                pending,
                                                finished,
                                                offset,
                                                limit,
                                                sortParameters);
            }
        });
    }

    public String getTaskCentricByTag(final String sessionId, final String tag, final long fromDate, final long toDate,
            final boolean myTasks, final boolean pending, final boolean running, final boolean finished,
            final int offset, final int limit, final TasksCentricController.SortSpecifierRestContainer sortParameters)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getTaskStatesByTag(sessionId,
                                                     tag,
                                                     fromDate,
                                                     toDate,
                                                     myTasks,
                                                     running,
                                                     pending,
                                                     finished,
                                                     offset,
                                                     limit,
                                                     sortParameters);
            }
        });
    }

    @Override
    public String getJobTaskTagsPrefix(final String sessionId, final String jobId, final String prefix)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getJobTaskTagsPrefix(sessionId, jobId, prefix);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getProperties()
     */
    @Override
    public SharedProperties getProperties() {
        return new SharedProperties(SchedulerConfig.get().getProperties(),
                                    SchedulerPortalDisplayConfig.get().getProperties());
    }

    @Override
    public Map<String, String> getSchedulerPortalDisplayProperties(final String sessionId) {
        RestClient restClientProxy = getRestClientProxy();
        return restClientProxy.getSchedulerPortalDisplayProperties(sessionId)
                              .entrySet()
                              .stream()
                              .collect(Collectors.toMap(entry -> (String) entry.getKey(),
                                                        entry -> (String) entry.getValue()));
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getJobInfo(java.lang
     * .String, java.lang.String)
     */
    @Override
    public String getJobInfo(final String sessionId, final String jobId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.job(sessionId, jobId);
            }
        });
    }

    public String getJobInfoDetails(final String sessionId, final String jobId)
            throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.jobInfo(sessionId, jobId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#pauseScheduler(java
     * .lang.String)
     */
    @Override
    public boolean pauseScheduler(final String sessionId) throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.pauseScheduler(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#resumeScheduler(java
     * .lang.String)
     */
    @Override
    public boolean resumeScheduler(final String sessionId) throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.resumeScheduler(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#freezeScheduler(java
     * .lang.String)
     */
    @Override
    public boolean freezeScheduler(final String sessionId) throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.freezeScheduler(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#killScheduler(java.
     * lang.String)
     */
    @Override
    public boolean killScheduler(final String sessionId) throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.killScheduler(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#startScheduler(java
     * .lang.String)
     */
    @Override
    public boolean startScheduler(final String sessionId) throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.startScheduler(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#stopScheduler(java.
     * lang.String)
     */
    @Override
    public boolean stopScheduler(final String sessionId) throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.stopScheduler(sessionId);
            }
        });
    }

    /**
     * Fetch logs for a given task in a given job
     *
     * @param sessionId current session id
     * @param jobId     id of the job
     * @param taskName  name of the task
     * @param logMode   one of {@link SchedulerServiceAsync#LOG_ALL}, {@link
     *                  SchedulerServiceAsync#LOG_STDERR}, {@link
     *                  SchedulerServiceAsync#LOG_STDOUT}
     * @return the logs for the given task
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String getTaskOutput(String sessionId, String jobId, String taskName, OutputMode logMode)
            throws RestServerException, ServiceException {

        RestClient restClientProxy = getRestClientProxy();

        try {
            switch (logMode) {
                case LOG_OUT_ERR:
                    return restClientProxy.tasklog(sessionId, jobId, taskName);
                case LOG_OUT:
                    return restClientProxy.taskStdout(sessionId, jobId, taskName);
                case LOG_ERR:
                    return restClientProxy.taskStderr(sessionId, jobId, taskName);
                default:
                    throw new RestServerException("Invalid logMode value: " + logMode);
            }
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    /**
     * Gets the output of a job even for tasks that have not terminated yet
     *
     * @param sessionId current session id
     * @param jobId     id of the job for which logs should be fetched
     * @return console output for the whole job
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String getLiveLogJob(final String sessionId, final String jobId)
            throws RestServerException, ServiceException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            return restClientProxy.getLiveLogJob(sessionId, jobId);
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    /**
     * Gets the number of bytes available in the job output stream for the given
     * job id, might be used to determine if fetch is necessary
     *
     * @param sessionId current session id
     * @param jobId     id of the job for which logs should be fetched
     * @return number of bytes available in the log for the given job, -1 if no
     * avail
     * @throws RestServerException
     */
    @Override
    public int getLiveLogJobAvailable(final String sessionId, final String jobId) throws RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        String number = null;

        try {
            number = restClientProxy.getLiveLogJobAvailable(sessionId, jobId);

            return Integer.parseInt(number);
        } catch (NumberFormatException e) {
            throw new RestServerException("Invalid number: " + number);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return -1;
        }
    }

    /**
     * Clean the remote live log object
     *
     * @param sessionId current session id
     * @param jobId     id of the job for which live logs should be cleaned
     * @return true if something was actually deleted
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public boolean deleteLiveLogJob(final String sessionId, final String jobId)
            throws RestServerException, ServiceException {
        return executeFunction(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.deleteLiveLogJob(sessionId, jobId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getStatistics(java.
     * lang.String)
     */
    @Override
    public String getStatistics(String sessionId) throws RestServerException, ServiceException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            return restClientProxy.getStatistics(sessionId);
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#
     * getStatisticsOnMyAccount (java.lang.String)
     */
    @Override
    public String getStatisticsOnMyAccount(String sessionId) throws RestServerException, ServiceException {

        RestClient restClientProxy = getRestClientProxy();

        try {
            return restClientProxy.getStatisticsOnMyAccount(sessionId);
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#schedulerStateRevision
     * (java.lang.String)
     */
    @Override
    public long schedulerStateRevision(String sessionId) throws RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        String revision = null;

        try {
            revision = restClientProxy.schedulerStateRevision(sessionId);

            return Long.parseLong(revision);
        } catch (NumberFormatException e) {
            throw new RestServerException("Revision is not a number: " + revision);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return -1L;
        }
    }

    /**
     * Get information for all users currently connected to the scheduler
     *
     * @param sessionId session id
     * @return user info as a json array
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String getSchedulerUsers(final String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getSchedulerUsers(sessionId);
            }
        });
    }

    /**
     * Get users having jobs in the scheduler
     *
     * @param sessionId session id
     * @return user info as a json array
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String getSchedulerUsersWithJobs(final String sessionId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getSchedulerUsersWithJobs(sessionId);
            }
        });
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#revisionAndjobsinfo
     * (java.lang.String, int, int, boolean, boolean, boolean, boolean)
     */
    @Override
    public String revisionAndjobsinfo(final String sessionId, final String startCursor, final String endCursor,
            int pageSize, boolean last, final String user, final boolean pending, final boolean running,
            final boolean finished) throws RestServerException, ServiceException {
        Query query = GraphQLQueries.get().getRevisionAndjobsInfoQuery(user,
                                                                       pending,
                                                                       running,
                                                                       finished,
                                                                       startCursor,
                                                                       endCursor,
                                                                       pageSize,
                                                                       last);
        String response = executeGraphQLQuery(sessionId, query);
        LOGGER.log(Level.SEVERE, response);
        return response;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getJobImage(java.lang
     * .String, java.lang.String)
     */
    @Override
    public String getJobImage(String sessionId, String jobId) throws RestServerException, ServiceException {
        String url = "img_" + jobId + ".png";
        String path = getServletContext().getRealPath("/images");

        File file = new File(path + File.separator + url);
        file.deleteOnExit();

        if (file.exists()) {
            // this might very well return the wrong file if you restart
            // the server but omit to clean tmpdir; not my problem
            return url;
        }

        throw new RestServerException(Status.NOT_FOUND.getStatusCode(), "File not found: " + file.getPath());
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getSchedulerStatus(
     * java.lang.String)
     */
    @Override
    public String getSchedulerStatus(String sessionId) throws RestServerException {
        try {
            return getRestClientProxy().schedulerStatus(sessionId);
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    /*
     * (non-Javadoc)
     *
     * @see Service#getVersion()
     */
    @Override
    public String getVersion() throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getVersion();
            }
        });
    }

    /**
     * Get server logs for a given task
     *
     * @param sessionId current session
     * @param jobId     id of a job
     * @param taskName  name of a task to restart within that job
     * @return job server logs
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String getTaskServerLogs(String sessionId, Integer jobId, String taskName)
            throws RestServerException, ServiceException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            return restClientProxy.taskServerLogs(sessionId, "" + jobId, taskName);
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    /**
     * Get server logs for a given job
     *
     * @param sessionId current session
     * @param jobId     id of a job
     * @return task server logs
     * @throws RestServerException
     * @throws ServiceException
     */
    @Override
    public String getJobServerLogs(String sessionId, Integer jobId) throws RestServerException, ServiceException {

        RestClient restClientProxy = getRestClientProxy();

        try {
            return restClientProxy.jobServerLogs(sessionId, jobId.toString());
        } catch (WebApplicationException e) {
            return rethrowRestServerException(e);
        }
    }

    @Override
    public List<JobUsage> getUsage(String sessionId, String user, Date startDate, Date endDate)
            throws RestServerException, ServiceException {
        RestClient restClientProxy = getRestClientProxy();

        InputStream inputStream = null;

        try {
            DateFormat df = new SimpleDateFormat(ISO_8601_FORMAT);
            String startDateAsString = df.format(startDate);
            String endDateAsString = df.format(endDate);

            if (user != null) {
                inputStream = restClientProxy.getUsageOnAccount(sessionId, user, startDateAsString, endDateAsString);
            } else {
                inputStream = restClientProxy.getUsageOnMyAccount(sessionId, startDateAsString, endDateAsString);
            }

            String responseAsString = convertToString(inputStream);

            return UsageJsonReader.readJobUsages(responseAsString);
        } catch (IOException | JSONException e) {
            throw new ServiceException(e.getMessage());
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return null;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    @Override
    public String getJobHtml(final String sessionId, final String jobId) throws RestServerException, ServiceException {
        return executeFunctionReturnStreamAsString(new Function<RestClient, InputStream>() {
            @Override
            public InputStream apply(RestClient restClient) {
                return restClient.getJobHtml(sessionId, jobId);
            }
        });
    }

    @Override
    public void putThirdPartyCredential(String sessionId, String key, String value) throws RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            restClientProxy.putThirdPartyCredential(sessionId, key, value);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
        }
    }

    @Override
    public Set<String> thirdPartyCredentialKeySet(String sessionId) throws ServiceException, RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        InputStream inputStream = null;
        try {
            inputStream = restClientProxy.thirdPartyCredentialsKeySet(sessionId);

            String responseAsString = convertToString(inputStream);

            JSONArray jsonArray = new JSONArray(responseAsString);
            HashSet<String> result = new HashSet<>(jsonArray.length());
            for (int i = 0; i < jsonArray.length(); i++) {
                result.add(jsonArray.getString(i));
            }
            return result;
        } catch (IOException | JSONException e) {
            throw new ServiceException(e.getMessage());
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return null;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    @Override
    public void removeThirdPartyCredential(String sessionId, String key) throws RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        try {
            restClientProxy.removeThirdPartyCredential(sessionId, key);
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
        }
    }

    /**
     * Execute a graphQL query. The queries should be built using the GraphQLQueries class
     * @param sessionId
     * @param query
     * @return
     * @throws ServiceException
     * @throws RestServerException
     */
    private String executeGraphQLQuery(String sessionId, Query query) throws ServiceException, RestServerException {

        if (sessionId == null || query == null)
            return null;

        Map<String, Object> result = graphQLClient.execute(sessionId, query);
        try {
            String data = JSON_MAPPER.writeValueAsString(result);
            return data;
        } catch (JsonProcessingException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            return "{\"error\": \"Cannot process JSON\"}";
        }

    }

    private boolean executeFunction(Function<RestClient, InputStream> function)
            throws ServiceException, RestServerException {
        RestClient restClientProxy = getRestClientProxy();

        InputStream inputStream = null;

        try {
            inputStream = function.apply(restClientProxy);
            return true;
        } catch (WebApplicationException e) {
            rethrowRestServerException(e);
            return false;
        } finally {
            IOUtils.closeQuietly(inputStream);
        }
    }

    private int executeFunction(BiFunction<RestClient, Integer, InputStream> action, List<Integer> jobIdList,
            String actionName) throws ServiceException, RestServerException {

        RestClient restClientProxy = getRestClientProxy();

        int failures = 0;
        int success = 0;

        for (Integer jobId : jobIdList) {
            InputStream inputStream = null;

            try {
                inputStream = action.apply(restClientProxy, jobId);

                if (Boolean.parseBoolean(convertToString(inputStream))) {
                    success++;
                } else {
                    failures++;
                }
            } catch (WebApplicationException e) {
                failures++;
            } catch (IOException e) {
                throw new ServiceException("Error while reading InputStream response: " + e.getMessage());
            } finally {
                IOUtils.closeQuietly(inputStream);
            }
        }

        if (failures > 0) {
            throw new RestServerException("Requested " + jobIdList.size() + " " + actionName + ": " + success +
                                          " succeeded, " + failures + " failed.");
        }

        return success;
    }

    private String executeFunctionReturnStreamAsString(Function<RestClient, InputStream> function)
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

    private RestClient getRestClientProxy() {
        ResteasyClient client = new ResteasyClientBuilder().asyncExecutor(threadPool)
                                                           .httpEngine(new ApacheHttpClient4Engine(httpClient))
                                                           .build();
        ResteasyWebTarget target = client.target(SchedulerConfig.get().getRestUrl());

        return target.proxy(RestClient.class);
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
