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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.jar.JarFile;

import javax.ws.rs.core.Response.Status;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.ow2.proactive_grid_cloud_portal.common.server.ConfigReader;
import org.ow2.proactive_grid_cloud_portal.common.server.ConfigUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.HttpUtils;
import org.ow2.proactive_grid_cloud_portal.common.server.Service;
import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobUsage;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.jaxb.MapRecord;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.jaxb.ObjectFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.server.jaxb.TaskRecord;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.DefaultHttpClient;
import org.codehaus.jettison.json.JSONException;
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
public class SchedulerServiceImpl extends Service implements SchedulerService {

    private static final Logger LOGGER = LoggerFactory.getLogger(SchedulerServiceImpl.class);

    private static final String ISO_8601_FORMAT = "yyyy-MM-dd'T'HH:mmZ";

    private ClientExecutor executor;
    private DefaultHttpClient httpClient;

    @Override
    public void init() {
        loadProperties();
        httpClient = HttpUtils.createDefaultExecutor();
        executor = new ApacheHttpClient4Executor(httpClient);
    }

    /**
     * Loads properties defined in the configuration file and in JVM arguments.
     */
    private void loadProperties() {
        SchedulerConfig.get().load(
          ConfigReader.readPropertiesFromFile(getServletContext().getRealPath(SchedulerConfig.CONFIG_PATH)));
        ConfigUtils.loadSystemProperties(SchedulerConfig.get());
    }

    /**
     * Submits a XML file to the REST part by using an HTTP client.
     * @param sessionId the id of the client which submits the job
     * @param file the XML file that is submitted 
     * @return an error message upon failure, "id=<jobId>" upon success
     * @throws RestServerException 
     * @throws ServiceException
     */
    public String submitXMLFile(String sessionId, File file) throws RestServerException, ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/submit");
        method.addHeader("sessionId", sessionId);

        boolean isJar = isJarFile(file);

        try {
            String name = (isJar) ? "jar" : "file";
            String mime = (isJar) ? "application/java-archive" : "application/xml";
            String charset = "ISO-8859-1";

            MultipartEntity entity = new MultipartEntity();
            entity.addPart("file", new FileBody(file, name, mime, charset));
            method.setEntity(entity);

            HttpResponse execute = httpClient.execute(method);
            InputStream is = execute.getEntity().getContent();
            String ret = convertToString(is);

            if (execute.getStatusLine().getStatusCode() == 200) {
                if (ret == null) {
                    throw new RestServerException(500, "Failed to get submission Id");
                } else {
                    return ret;
                }
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
     * @param sessionId current session
     * @param commandFileContent content of the command file: endline separated native commands 
     * @param jobName name of the job to create
     * @param selectionScriptContent selection script content, or null
     * @param selectionScriptExtension selection script extension for script engine detection ("js", "py", "rb")
     * @return JobId of created Job as JSON
     * @throws RestServerException
     * @throws ServiceException
     */
    public String submitFlatJob(String sessionId, String commandFileContent, String jobName,
            String selectionScriptContent, String selectionScriptExtension) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.submitFlat(sessionId, commandFileContent, jobName,
                selectionScriptContent, selectionScriptExtension);

        Status status = clientResponse.getResponseStatus();
        String stringResponse = clientResponse.getEntity();
        switch (status) {
            case OK:
                return stringResponse;
            default:
                throw new RestServerException(status.getStatusCode(), stringResponse);
        }
    }

    /**
     * Getter of the result of a task.
     * @param sessionId the session id of the user which is looged in
     * @param jobId the id of the job the task belongs to
     * @param taskId the id of the task
     * @return the result
     * @throws RestServerException
     * @throws ServiceException
     */
    public InputStream getTaskResult(String sessionId, String jobId, String taskId)
            throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.taskresult(sessionId, jobId, taskId);

        Status status = clientResponse.getResponseStatus();
        switch (status) {
            case OK:
                return clientResponse.getEntity();

            default:
                try {
                    String stringResponse = convertToString(clientResponse.getEntity());
                    throw new RestServerException(status.getStatusCode(), stringResponse);
                } catch (IOException e) {
                    throw new ServiceException("Error while converting InputStream to String: " +
                        e.getMessage());
                }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#removeJobs(java.lang
     * .String, java.util.List)
     */
    public int removeJobs(String sessionId, List<Integer> jobIdList) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (Integer jobId : jobIdList) {
            ClientResponse<InputStream> clientResponse = null;
            try {
                clientResponse = client.removeJob(sessionId, Integer.toString(jobId));
                Status status = clientResponse.getResponseStatus();
                String ret = convertToString(clientResponse.getEntity());

                switch (status) {
                    case OK:
                        if (Boolean.parseBoolean(ret)) {
                            success++;
                        }
                        break;
                    default:
                        failures++;
                        break;
                }
            } catch (IOException e) {
                throw new ServiceException("Error while reading InputStream response: " + e.getMessage());
            } finally {
                if (clientResponse != null) {
                    clientResponse.releaseConnection();
                }
            }
        }
        if (failures > 0) {
            throw new RestServerException("Requested " + jobIdList.size() + " job removal; " + success +
                " succeeded, " + failures + " failed.");
        }
        return success;
    }

    public int pauseJobs(String sessionId, List<Integer> jobIdList) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (Integer jobId : jobIdList) {
            ClientResponse<InputStream> clientResponse = null;
            try {
                clientResponse = client.pauseJob(sessionId, Integer.toString(jobId));
                Status status = clientResponse.getResponseStatus();
                String ret = convertToString(clientResponse.getEntity());

                switch (status) {
                    case OK:
                        if (Boolean.parseBoolean(ret)) {
                            success++;
                        }
                        break;
                    default:
                        failures++;
                        break;
                }
            } catch (IOException e) {
                throw new ServiceException("Error while reading InputStream response: " + e.getMessage());
            } finally {
                if (clientResponse != null) {
                    clientResponse.releaseConnection();
                }
            }
        }
        if (failures > 0) {
            throw new RestServerException("Requested " + jobIdList.size() + " job paused; " + success +
                " succeeded, " + failures + " failed.");
        }
        return success;
    }

    public int resumeJobs(String sessionId, List<Integer> jobIdList) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (Integer jobId : jobIdList) {
            ClientResponse<InputStream> clientResponse = null;
            try {
                clientResponse = client.resumeJob(sessionId, Integer.toString(jobId));
                Status status = clientResponse.getResponseStatus();
                String ret = convertToString(clientResponse.getEntity());

                switch (status) {
                    case OK:
                        if (Boolean.parseBoolean(ret)) {
                            success++;
                        }
                        break;
                    default:
                        failures++;
                        break;
                }
            } catch (IOException e) {
                throw new ServiceException("Error while reading InputStream response: " + e.getMessage());
            } finally {
                if (clientResponse != null) {
                    clientResponse.releaseConnection();
                }
            }
        }
        if (failures > 0) {
            throw new RestServerException("Requested " + jobIdList.size() + " job resumed; " + success +
                " succeeded, " + failures + " failed.");
        }
        return success;
    }

    public int killJobs(String sessionId, List<Integer> jobIdList) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (Integer jobId : jobIdList) {
            ClientResponse<InputStream> clientResponse = null;
            try {
                clientResponse = client.killJob(sessionId, Integer.toString(jobId));
                Status status = clientResponse.getResponseStatus();
                String ret = convertToString(clientResponse.getEntity());

                switch (status) {
                    case OK:
                        if (Boolean.parseBoolean(ret)) {
                            success++;
                        }
                        break;
                    default:
                        failures++;
                        break;
                }
            } catch (IOException e) {
                throw new ServiceException("Error while reading InputStream response: " + e.getMessage());
            } finally {
                if (clientResponse != null) {
                    clientResponse.releaseConnection();
                }
            }
        }
        if (failures > 0) {
            throw new RestServerException("Requested " + jobIdList.size() + " job killed; " + success +
                " succeeded, " + failures + " failed.");
        }
        return success;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#setPriorityByName(java
     * .lang.String, java.util.List, java.lang.String)
     */
    public void setPriorityByName(String sessionId, List<Integer> jobIdList, String priorityName)
            throws ServiceException, RestServerException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        int failures = 0;
        int success = 0;

        for (Integer jobId : jobIdList) {
            ClientResponse<InputStream> clientResponse = client.schedulerChangeJobPriorityByName(sessionId, Integer.toString(jobId),
                    priorityName);
            Status st = clientResponse.getResponseStatus();
            switch (st) {
                case NO_CONTENT:
                    success++;
                    break;
                default:
                    failures++;
                    break;
            }
            clientResponse.releaseConnection();
        }
        if (failures > 0) {
            throw new RestServerException("Requested " + jobIdList.size() + " job set to priority " +
                priorityName + "; " + success + " succeeded, " + failures + " failed.");
        }
    }

    /**
     * Login to the scheduler using a Credentials file
     * 
     * @return the sessionId which can be parsed as an Integer, or an error message
     * @throws RestServerException
     * @throws ServiceException 
     */
    public String login(String login, String pass, File cred, String ssh) throws RestServerException,
            ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/login");

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

    /**
     * Create a Credentials file with the provided authentication parameters
     * 
     * @param login username
     * @param pass password
     * @param ssh private ssh key
     * @return the the Credentials file as a base64 String
     * @throws RestServerException
     * @throws ServiceException 
     */
    public String createCredentials(String login, String pass, String ssh) throws RestServerException,
            ServiceException {
        HttpPost method = new HttpPost(SchedulerConfig.get().getRestUrl() + "/scheduler/createcredential");

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
    public void logout(String sessionId) throws RestServerException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        client.disconnect(sessionId);
    }

    @Override
    public boolean killTask(String sessionId, Integer jobId, String taskName) throws RestServerException,
            ServiceException {
        ClientResponse<InputStream> clientResponse = null;
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        try {
            clientResponse = client.killTask(sessionId, "" + jobId, taskName);
            Status status = clientResponse.getResponseStatus();
            InputStream response = clientResponse.getEntity();
            String info = convertToString(response);
            switch (status) {
                case OK:
                    return true;
                default:
                    throw new RestServerException(status.getStatusCode(), info);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public boolean restartTask(String sessionId, Integer jobId, String taskName) throws RestServerException,
            ServiceException {
        ClientResponse<InputStream> clientResponse = null;
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        try {
            clientResponse = client.restartTask(sessionId, "" + jobId, taskName);
            Status status = clientResponse.getResponseStatus();
            InputStream response = clientResponse.getEntity();
            String info = convertToString(response);
            LOGGER.info("SchedulerServiceImpl.restartTask() " + status + "//" + info);
            switch (status) {
                case OK:
                    return true;
                default:
                    throw new RestServerException(status.getStatusCode(), info);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public boolean preemptTask(String sessionId, Integer jobId, String taskName) throws RestServerException,
            ServiceException {
        ClientResponse<InputStream> clientResponse = null;
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        try {
            clientResponse = client.preemptTask(sessionId, "" + jobId, taskName);
            Status status = clientResponse.getResponseStatus();
            InputStream response = clientResponse.getEntity();
            String info = convertToString(response);
            switch (status) {
                case OK:
                    return true;
                default:
                    throw new RestServerException(status.getStatusCode(), info);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getTasks(java.lang.
     * String, java.lang.String)
     */
    public String getTasks(String sessionId, String jobId) throws RestServerException, ServiceException {
        ClientResponse<InputStream> clientResponse = null;
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        try {
            clientResponse = client.getJobTaskStates(sessionId, jobId);
            Status status = clientResponse.getResponseStatus();
            InputStream response = clientResponse.getEntity();
            String info = convertToString(response);
            switch (status) {
                case OK:
                    return info;
                default:
                    throw new RestServerException(status.getStatusCode(), info);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getProperties()
     */
    public Map<String, String> getProperties() {
        return SchedulerConfig.get().getProperties();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getJobInfo(java.lang
     * .String, java.lang.String)
     */
    public String getJobInfo(String sessionId, String jobId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.job(sessionId, jobId);
        try {
            Status status = clientResponse.getResponseStatus();
            InputStream response = clientResponse.getEntity();
            String respStr = convertToString(response);

            switch (status) {
                case OK:
                    return respStr;
                default:
                    throw new RestServerException(status.getStatusCode(), status.getReasonPhrase(), respStr);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#pauseScheduler(java
     * .lang.String)
     */
    public boolean pauseScheduler(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;

        try {
            clientResponse = client.pauseScheduler(sessionId);
            Status status = clientResponse.getResponseStatus();
            switch (status) {
                case OK:
                    return Boolean.parseBoolean(convertToString(clientResponse.getEntity()));
                default:
                    String stringResponse = convertToString(clientResponse.getEntity(InputStream.class));
                    throw new RestServerException(clientResponse.getStatus(), stringResponse);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#resumeScheduler(java
     * .lang.String)
     */
    public boolean resumeScheduler(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;

        try {
            clientResponse = client.resumeScheduler(sessionId);
            Status status = clientResponse.getResponseStatus();
            switch (status) {
                case OK:
                    return Boolean.parseBoolean(convertToString(clientResponse.getEntity()));
                default:
                    String stringResponse = convertToString(clientResponse.getEntity(InputStream.class));
                    throw new RestServerException(clientResponse.getStatus(), stringResponse);
            }

        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#freezeScheduler(java
     * .lang.String)
     */
    public boolean freezeScheduler(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;

        try {
            clientResponse = client.freezeScheduler(sessionId);
            Status status = clientResponse.getResponseStatus();
            switch (status) {
                case OK:
                    return Boolean.parseBoolean(convertToString(clientResponse.getEntity()));
                default:
                    String stringResponse = convertToString(clientResponse.getEntity(InputStream.class));
                    throw new RestServerException(clientResponse.getStatus(), stringResponse);
            }

        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#killScheduler(java.
     * lang.String)
     */
    public boolean killScheduler(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;

        try {
            clientResponse = client.killScheduler(sessionId);
            Status status = clientResponse.getResponseStatus();
            switch (status) {
                case OK:
                    return Boolean.parseBoolean(convertToString(clientResponse.getEntity()));
                default:
                    String stringResponse = convertToString(clientResponse.getEntity(InputStream.class));
                    throw new RestServerException(clientResponse.getStatus(), stringResponse);
            }

        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#startScheduler(java
     * .lang.String)
     */
    public boolean startScheduler(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;

        try {
            clientResponse = client.startScheduler(sessionId);
            Status status = clientResponse.getResponseStatus();
            switch (status) {
                case OK:
                    return Boolean.parseBoolean(convertToString(clientResponse.getEntity()));
                default:
                    String stringResponse = convertToString(clientResponse.getEntity(InputStream.class));
                    throw new RestServerException(clientResponse.getStatus(), stringResponse);
            }

        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#stopScheduler(java.
     * lang.String)
     */
    public boolean stopScheduler(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = null;

        try {
            clientResponse = client.stopScheduler(sessionId);
            Status status = clientResponse.getResponseStatus();
            switch (status) {
                case OK:
                    return Boolean.parseBoolean(convertToString(clientResponse.getEntity()));
                default:
                    String stringResponse = convertToString(clientResponse.getEntity(InputStream.class));
                    throw new RestServerException(clientResponse.getStatus(), stringResponse);
            }

        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /**
     * Fetch logs for a given task in a given job
     * 
     * @param sessionId current session id
     * @param jobId id of the job
     * @param taskName name of the task
     * @param logMode one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     * 			 {@link SchedulerServiceAsync#LOG_STDOUT}
     * @return the logs for the given task
     * @throws RestServerException
     * @throws ServiceException 
     */
    public String getTaskOutput(String sessionId, String jobId, String taskName, int logMode)
            throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = null;
        if (logMode == SchedulerServiceAsync.LOG_ALL) {
            clientResponse = client.tasklog(sessionId, jobId, taskName);
        } else if (logMode == SchedulerServiceAsync.LOG_STDOUT) {
            clientResponse = client.taskStdout(sessionId, jobId, taskName);
        } else if (logMode == SchedulerServiceAsync.LOG_STDERR) {
            clientResponse = client.taskStderr(sessionId, jobId, taskName);
        }

        if (clientResponse != null) {
            String ret = clientResponse.getEntity();
            if (clientResponse.getStatus() == 200) {
                return ret;
            }
            throw new RestServerException(clientResponse.getResponseStatus().getStatusCode(), ret);
        } else {
            throw new RestServerException("Invalid logMode value");
        }
    }

    /**
     * Gets the output of a job even for tasks that have not terminated yet
     * @param sessionId current session id
     * @param jobId id of the job for which logs should be fetched
     * @return console output for the whole job
     * @throws RestServerException
     * @throws ServiceException 
     */
    public String getLiveLogJob(final String sessionId, final String jobId) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.getLiveLogJob(sessionId, jobId);
        String out = clientResponse.getEntity();
        if (clientResponse.getStatus() == 200) {
            return out;
        } else {
            throw new RestServerException(clientResponse.getStatus(), out);
        }

    }

    /**
     * Gets the number of bytes available in the job output stream for the given job id,
     * might be used to determine if fetch is necessary
     * @param sessionId current session id
     * @param jobId id of the job for which logs should be fetched
     * @return number of bytes available in the log for the given job, -1 if no avail
     * @throws RestServerException
     */
    public int getLiveLogJobAvailable(final String sessionId, final String jobId) throws RestServerException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.getLiveLogJobAvailable(sessionId, jobId);
        String out = clientResponse.getEntity();

        if (clientResponse.getStatus() == 200) {
            return Integer.parseInt(out);
        } else {
            throw new RestServerException(clientResponse.getStatus(), out);
        }
    }

    /**
     * Clean the remote live log object
     * @param sessionId current session id
     * @param jobId id of the job for which live logs should be cleaned
     * @return true if something was actually deleted
     * @throws RestServerException
     * @throws ServiceException 
     */
    public boolean deleteLiveLogJob(final String sessionId, final String jobId) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> resp = client.deleteLiveLogJob(sessionId, jobId);
        try {
            String ret = convertToString(resp.getEntity());
            if (resp.getStatus() == 200) {
                return Boolean.parseBoolean(ret);
            } else {
                throw new RestServerException(resp.getStatus(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            resp.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getStatistics(java.
     * lang.String)
     */
    public String getStatistics(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);

        ClientResponse<String> clientResponse = client.getStatistics(sessionId);
        Status status = clientResponse.getResponseStatus();
        switch (status) {
            case OK:
                return clientResponse.getEntity();
            default:
                throw new RestServerException("Failed to fetch account stats: " +
                        clientResponse.getEntity());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getStatisticsOnMyAccount
     * (java.lang.String)
     */
    public String getStatisticsOnMyAccount(String sessionId) throws RestServerException, ServiceException {

        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);

        ClientResponse<String> clientResponse = client.getStatisticsOnMyAccount(sessionId);
        Status status = clientResponse.getResponseStatus();
        switch (status) {
            case OK:
                return clientResponse.getEntity();
            default:
                throw new RestServerException("Failed to fetch account stats: " +
                        clientResponse.getEntity());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#schedulerStateRevision
     * (java.lang.String)
     */
    public long schedulerStateRevision(String sessionId) throws RestServerException {

        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.schedulerStateRevision(sessionId);

        Status status = clientResponse.getResponseStatus();
        switch (status) {
            case OK:
                return Long.parseLong(clientResponse.getEntity());
            default:
                throw new RestServerException(clientResponse.getEntity());
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
    public String getSchedulerUsers(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.getSchedulerUsers(sessionId);

        Status status = clientResponse.getResponseStatus();

        try {
            String ret = convertToString(clientResponse.getEntity(InputStream.class));

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(clientResponse.getStatus(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }
    }

    /**
     * Get users having jobs in the scheduler
     * 
     * @param sessionId session id
     * @return user info as a json array
     * @throws RestServerException
     * @throws ServiceException 
     */
    public String getSchedulerUsersWithJobs(String sessionId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.getSchedulerUsersWithJobs(sessionId);

        Status status = clientResponse.getResponseStatus();

        try {
            String ret = convertToString(clientResponse.getEntity(InputStream.class));

            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(clientResponse.getStatus(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#revisionAndjobsinfo
     * (java.lang.String, int, int, boolean, boolean, boolean, boolean)
     */
    public String revisionAndjobsinfo(String sessionId, int index, int range, boolean myJobsOnly,
            boolean pending, boolean running, boolean finished) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.revisionAndjobsinfo(sessionId, index, range,
                myJobsOnly, pending, running, finished);

        Status status = clientResponse.getResponseStatus();
        try {
            String ret = convertToString(clientResponse.getEntity(InputStream.class));
            switch (status) {
                case OK:
                    return ret;
                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getJobImage(java.lang
     * .String, java.lang.String)
     */
    public String getJobImage(String sessionId, String jobId) throws RestServerException, ServiceException {
        String url = "img_" + jobId + ".png";
        String path = getServletContext().getRealPath("/images");

        File f = new File(path + File.separator + url);
        f.deleteOnExit();

        if (f.exists()) {
            // this might very well return the wrong file if you restart
            // the server but omit to clean tmpdir; not my problem
            return url;
        }

        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.getJobImage(sessionId, jobId);
        Status status = clientResponse.getResponseStatus();

        try {
            InputStream response = clientResponse.getEntity();
            String ret = IOUtils.toString(response, RestClient.ENCODING);
            switch (status) {
                case OK:
                    byte[] dec = Base64.decodeBase64(ret.getBytes(RestClient.ENCODING));
                    FileUtils.writeByteArrayToFile(f,dec);
                    return url;

                default:
                    throw new RestServerException(status.getStatusCode(), ret);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getJobMap(java.lang
     * .String, java.lang.String)
     */
    public JobVisuMap getJobMap(String sessionId, String jobId) throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.getJobMap(sessionId, jobId);
        Status status = clientResponse.getResponseStatus();

        try {
            InputStream response = clientResponse.getEntity();
            String res = convertToString(response);
            switch (status) {
                case OK:
                    try {
                        // it's kinda bad to do this server-side,
                        // but JAXB doesn't really work great client side
                        InputStream in = new ByteArrayInputStream(res.getBytes());
                        JAXBContext jc = JAXBContext.newInstance(ObjectFactory.class.getPackage().getName());
                        Unmarshaller um = jc.createUnmarshaller();
                        JAXBElement<?> elt = (JAXBElement<?>) um.unmarshal(in);
                        MapRecord rec = (MapRecord) elt.getValue();

                        JobVisuMap ret = new JobVisuMap();
                        for (TaskRecord tr : rec.getMap().getTask()) {
                            int x = tr.getPosition().getX();
                            int y = tr.getPosition().getY();
                            int w = tr.getSize().getX();
                            int h = tr.getSize().getY();
                            String name = tr.getName();
                            ret.addTask(x, y, w, h, name);
                        }

                        return ret;

                    } catch (JAXBException e) {
                        throw new ServiceException(e.getMessage());
                    }
                default:
                    throw new RestServerException(status.getStatusCode(), res);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerService#getSchedulerStatus(
     * java.lang.String)
     */
    public String getSchedulerStatus(String sessionId) throws RestServerException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.schedulerStatus(sessionId);
        Status status = clientResponse.getResponseStatus();
        String ret = clientResponse.getEntity();

        switch (status) {
            case OK:
                return ret;
            default:
                throw new RestServerException(ret);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see Service#getVersion()
     */
    public String getVersion() throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
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
            LOGGER.warn("Failed to read server response", e);
            throw new ServiceException("Failed to read server response", e);
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    /**
     * Get server logs for a given task
     * 
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to restart within that job
     * @return job server logs
     * @throws RestServerException
     * @throws ServiceException
     */
    public String getTaskServerLogs(String sessionId, Integer jobId, String taskName)
            throws RestServerException, ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.taskServerLogs(sessionId, "" + jobId, taskName);
        Status status = clientResponse.getResponseStatus();
        String response = clientResponse.getEntity();
        switch (status) {
            case OK:
                return response;
            default:
                throw new RestServerException(status.getStatusCode(), response);
        }
    }

    /**
     * Get server logs for a given job
     * 
     * @param sessionId current session
     * @param jobId id of a job
     * @return task server logs
     * @throws RestServerException
     * @throws ServiceException
     */
    public String getJobServerLogs(String sessionId, Integer jobId) throws RestServerException,
            ServiceException {
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<String> clientResponse = client.jobServerLogs(sessionId, "" + jobId);
        Status status = clientResponse.getResponseStatus();
        String response = clientResponse.getEntity();
        switch (status) {
            case OK:
                return response;
            default:
                throw new RestServerException(status.getStatusCode(), response);
        }
    }

    @Override
    public List<JobUsage> getUsage(String sessionId, String user, Date startDate, Date endDate) throws RestServerException, ServiceException {
        ClientResponse<InputStream> clientResponse = null;
        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        try {
            DateFormat df = new SimpleDateFormat(ISO_8601_FORMAT);
            String startDateAsString = df.format(startDate);
            String endDateAsString = df.format(endDate);

            if (user!=null) {
                clientResponse = client.getUsageOnAccount(sessionId, user, startDateAsString, endDateAsString);
            } else {
                clientResponse = client.getUsageOnMyAccount(sessionId, startDateAsString, endDateAsString);
            }

            Status status = clientResponse.getResponseStatus();
            InputStream response = clientResponse.getEntity();
            String responseAsString = convertToString(response);

            switch (status) {
                case OK:
                    return UsageJsonReader.readJobUsages(responseAsString);
                default:
                    throw new RestServerException(status.getStatusCode(), responseAsString);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } catch (JSONException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            if (clientResponse != null) {
                clientResponse.releaseConnection();
            }
        }
    }

    @Override
    public String getJobHtml(String sessionId, String jobId) throws RestServerException, ServiceException {

        RestClient client = ProxyFactory.create(RestClient.class, SchedulerConfig.get().getRestUrl(), executor);
        ClientResponse<InputStream> clientResponse = client.getJobHtml(sessionId, jobId);
        Status status = clientResponse.getResponseStatus();

        try {
            InputStream response = clientResponse.getEntity();
            String res = convertToString(response);
            switch (status) {
                case OK:
                    return res;

                default:
                    throw new RestServerException(status.getStatusCode(), res);
            }
        } catch (IOException e) {
            throw new ServiceException(e.getMessage());
        } finally {
            clientResponse.releaseConnection();
        }
    }
}
