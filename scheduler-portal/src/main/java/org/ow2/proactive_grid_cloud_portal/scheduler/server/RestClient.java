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

import java.io.InputStream;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
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
import org.jboss.resteasy.client.ClientResponse;
import org.jboss.resteasy.plugins.providers.multipart.MultipartInput;


/**
 * @author ffonteno
 *
 * Interface defining a client for the REST service
 */
@Path("/scheduler/")
public interface RestClient {

    /**
     * Disconnect the user identified by the sessionId from the scheduler
     * 
     * @param sessionId the session id of the user
     * @return a ClientResponse containing the response status
     */
    @PUT
    @Path("disconnect")
    ClientResponse<Void> disconnect(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Gets the list of jobs in a JSON array
     *
     * @param sessionId the session id of the user
     * @return a ClientResponse containing the response status and the JSON array including the job list, in case of success.
     */
    @GET
    @Path("jobsinfo")
    @Produces({ "application/json", "application/xml" })
    ClientResponse<InputStream> jobs(@HeaderParam("sessionid")
    String sessionId);

    /**
     * Submits a job to the Scheduler.
     * @param sessionId the session id of the user that performs the submission
     * @param multipart the multipart message that encodes the job descriptor file 
     * @return a ClientResponse containing the response status and the job id generated, in case of success. 
     */
    @POST
    @Path("submit")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    ClientResponse<String> jobs(@HeaderParam("sessionid")
    String sessionId, MultipartInput multipart);

    /**
     * Submit a flat command job
     * Each line in the file is a command that will be run on a different node
     * @param sessionId
     * @param commandFileContent content of the flat command file, one task per line
     * @param jobName name of the job
     * @param selectionScriptContent selection script or null
     * @return
     */
    @POST
    @Path("submitflat")
    @Produces("application/json")
    ClientResponse<String> submitFlat(@HeaderParam("sessionid")
    String sessionId, @FormParam("commandFileContent")
    String commandFileContent, @FormParam("jobName")
    String jobName, @FormParam("selectionScriptContent")
    String selectionScriptContent, @FormParam("selectionScriptExtension")
    String selectionScriptExtension);

    /**
     * Deletes a job from the Scheduler.
     * @param sessionId the session id of the user that performs the deletion
     * @param jobId the id of the job that will be deleted
     * @return a ClientResponse containing the response status and true - if the removed was successfully, false - otherwise.
     */
    @DELETE
    @Path("jobs/{jobid}")
    ClientResponse<InputStream> removeJob(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Pauses a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job that will be deleted
     * @return a ClientResponse containing the response status and true - if the job was successfully paused, false - otherwise. 
     */
    @PUT
    @Path("jobs/{jobid}/pause")
    ClientResponse<InputStream> pauseJob(@HeaderParam("sessionid")
    final String sessionId, @PathParam("jobid")
    final String jobId);

    /**
     * Resumes a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the job id which will be resumed
     * @return a ClientResponse containing the response status and true - if the job was successfully resumed, false - otherwise.
     */
    @PUT
    @Path("jobs/{jobid}/resume")
    ClientResponse<InputStream> resumeJob(@HeaderParam("sessionid")
    final String sessionId, @PathParam("jobid")
    final String jobId);

    /**
     * Kills a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the job id which will be resumed
     * @return a ClientResponse containing the response status.
     */
    @PUT
    @Path("jobs/{jobid}/kill")
    ClientResponse<InputStream> killJob(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Kill a task
     * @param sessionId
     * @param jobId
     * @param taskName
     * @return
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/kill")
    ClientResponse<InputStream> killTask(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskname")
    String taskName);

    /**
     * Preempt a task
     * @param sessionId
     * @param jobId
     * @param taskName
     * @return
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/preempt")
    ClientResponse<InputStream> preemptTask(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskname")
    String taskName);

    /**
     * Restart a task
     * @param sessionId
     * @param jobId
     * @param taskName
     * @return
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/restart")
    ClientResponse<InputStream> restartTask(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskname")
    String taskName);

    /**
     * Gets the list of tasks in a JSON array for a given job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the job id for which the tasks are asked.
     * @return a ClientResponse containing the response status and the JSON array including the task list, in case of success.
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates")
    @Produces("application/json")
    ClientResponse<InputStream> getJobTaskStates(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Gets the state of a certain job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @return a ClientResponse containing the response status and an InputStream containing information about the state of the job.
     */
    @GET
    @Path("jobs/{jobid}")
    @Produces("application/json")
    ClientResponse<InputStream> job(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Changes the priority of a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @param priorityName the new priority of the job 
     * @return a ClientResponse containing the response status.
     */
    @PUT
    @Path("jobs/{jobid}/priority/byname/{name}")
    ClientResponse<InputStream> schedulerChangeJobPriorityByName(@HeaderParam("sessionid")
    final String sessionId, @PathParam("jobid")
    final String jobId, @PathParam("name")
    String priorityName);

    /**
     * Pauses the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully paused and false in case of a
     * failure
     */
    @PUT
    @Path("pause")
    ClientResponse<InputStream> pauseScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Resumes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully resumed and false in case of a
     * failure
     */
    @PUT
    @Path("resume")
    ClientResponse<InputStream> resumeScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Freezes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully resumed and false in case of a
     * failure
     */
    @PUT
    @Path("freeze")
    ClientResponse<InputStream> freezeScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Kills the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully killed and false in case of a
     * failure
     */
    @PUT
    @Path("kill")
    ClientResponse<InputStream> killScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Starts the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully started and false in case of a
     * failure
     */
    @PUT
    @Path("start")
    ClientResponse<InputStream> startScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Stops the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was stopped paused and false in case of a
     * failure
     */
    @PUT
    @Path("stop")
    ClientResponse<InputStream> stopScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Gets the tasks ids for a job. 
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job for which the list of task ids is asked for
     * @return a ClientResponse containing the response status and a list of strings which represent the list of tasks ids
     */
    @GET
    @Path("jobs/{jobid}/tasks")
    @Produces("application/json")
    ClientResponse<InputStream> getJobTasksIds(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Gets all the logs for a finished task.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task corresponds to
     * @param taskId the id of the task 
     * @return a ClientResponse containing the response status and a string with the logs
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/log/all")
    @Produces("application/json")
    ClientResponse<String> tasklog(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskid")
    String taskId);

    /**
     * Gets the logs for a finished task, only on stdout
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task corresponds to
     * @param taskId the id of the task 
     * @return a ClientResponse containing the response status and a string with the logs
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/log/out")
    @Produces("application/json")
    ClientResponse<String> taskStdout(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskid")
    String taskId);

    /**
     * Gets the logs for a finished task, only on stderr
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task corresponds to
     * @param taskId the id of the task 
     * @return a ClientResponse containing the response status and a string with the logs
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/log/err")
    @Produces("application/json")
    ClientResponse<String> taskStderr(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskid")
    String taskId);

    /**
     * Stream the output of job identified by the id <code>jobid</code>
     * only stream currently available logs, call this method several times
     * to get the complete output.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/livelog")
    @Produces("application/json")
    ClientResponse<String> getLiveLogJob(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * number of available bytes in the stream or -1 if the stream does not exist.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @GET
    @Path("jobs/{jobid}/livelog/available")
    @Produces("application/json")
    ClientResponse<String> getLiveLogJobAvailable(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * remove the live log object.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @DELETE
    @Path("jobs/{jobid}/livelog")
    @Produces("application/json")
    ClientResponse<InputStream> deleteLiveLogJob(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Gets server logs for a given task.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task corresponds to
     * @param taskId the id of the task 
     * @return a ClientResponse containing the response status and a string with the server logs
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/log/server")
    @Produces("application/json")
    ClientResponse<String> taskServerLogs(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskid")
    String taskId);

    /**
     * Gets server logs for a given job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task corresponds to
     * @return a ClientResponse containing the response status and a string with the server logs
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/log/server")
    @Produces("application/json")
    ClientResponse<String> jobServerLogs(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Gets the result of a task.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task belongs
     * @param taskId the id of the task to which the result is asked
     * @return the result the result of the task
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/value")
    @Produces("*/*")
    ClientResponse<InputStream> taskresult(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId, @PathParam("taskid")
    String taskId);

    /**
     * returns statistics about the scheduler
     * @param sessionId the session id associated to this new connection
     * @return a string containing the statistics
     */
    @GET
    @Path("stats")
    @Produces("application/json")
    ClientResponse<String> getStatistics(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * returns a string containing some data regarding the user's account
     * @param sessionId the session id associated to this new connection
     * @return a string containing some data regarding the user's account
     */
    @GET
    @Path("stats/myaccount")
    @Produces("application/json")
    ClientResponse<String> getStatisticsOnMyAccount(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Returns the revision number of the scheduler state
     * @param sessionId a valid session id.
     * @return the revision of the scheduler state
     */
    @GET
    @Path("state/revision")
    @Produces({ "application/json", "application/xml" })
    ClientResponse<String> schedulerStateRevision(@HeaderParam("sessionid")
    String sessionId);

    /**
     * Returns a map containing one entry with the revision id as key and the
     * list of UserJobInfo as value.
     * each jobs is described using
     *   - its id
     *   - its owner
     *   - the JobInfo class
     * @param sessionId a valid session id
     * @param myJobs only my jobs when true, all jobs when false
     * @param pending fetch pending jobs
     * @param running fetch running jobs
     * @param finished fetch finished jobs
     * @return a map containing one entry with the revision id as key and the 
     * list of UserJobInfo as value.
     */
    @GET
    @GZIP
    @Path("revisionjobsinfo")
    @Produces({ "application/json", "application/xml" })
    ClientResponse<InputStream> revisionAndjobsinfo(@HeaderParam("sessionid")
    String sessionId, @QueryParam("index")
    int index, @QueryParam("range")
    int range, @QueryParam("myjobs")
    boolean myJobs, @QueryParam("pending")
    boolean pending, @QueryParam("running")
    boolean running, @QueryParam("finished")
    boolean finished);

    /**
     * Returns an html visualization corresponding of a jobid
     * @param sessionId a valid session id
     * @param jobId the job id
     * @return an html visualization corresponding of a <code>jobId</code>
     */
    @GET
    @Path("jobs/{jobid}/html")
    @Produces("text/html")
    ClientResponse<InputStream> getJobHtml(@HeaderParam("sessionid")
    String sessionId, @PathParam("jobid")
    String jobId);

    /**
     * Users currently connected to the scheduler
     * 
     * @param sessionId the session id associated to this new connection
     */
    @GET
    @GZIP
    @Path("users")
    @Produces({ "application/json", "application/xml" })
    ClientResponse<InputStream> getSchedulerUsers(@HeaderParam("sessionid")
    String sessionId);

    /**
     * Users having jobs in the scheduler
     * 
     * @param sessionId the session id associated to this new connection
     */
    @GET
    @GZIP
    @Path("userswithjobs")
    @Produces({ "application/json", "application/xml" })
    ClientResponse<InputStream> getSchedulerUsersWithJobs(@HeaderParam("sessionid")
    String sessionId);

    /**
     * Returns the Scheduler status as a String, 
     * ie org.ow2.proactive.scheduler.common.SchedulerStatus.toString()
     * @param sessionId a valid session id
     * @return a String describing the current scheduler status
     */
    @GET
    @Path("status")
    ClientResponse<String> schedulerStatus(@HeaderParam("sessionid")
    String sessionId);

    @GET
    @Path("version")
    ClientResponse<InputStream> getVersion();

    @GET
    @Path("usage/myaccount")
    @Produces("application/json")
    ClientResponse<InputStream> getUsageOnMyAccount(@HeaderParam("sessionid") String sessionId,
                                                    @QueryParam("startdate") String startDate,
                                                    @QueryParam("enddate") String endDate);
    @GET
    @Path("usage/account")
    @Produces("application/json")
    ClientResponse<InputStream> getUsageOnAccount(@HeaderParam("sessionid") String sessionId,
                                                    @QueryParam("user") String user,
                                                    @QueryParam("startdate") String startDate,
                                                    @QueryParam("enddate") String endDate);

    @POST
    @Path("/credentials/{key}")
    ClientResponse<Void> putThirdPartyCredential(@HeaderParam("sessionid") String sessionId, @PathParam("key") String key,
                                 @FormParam(
                                         "value") String value);

    @DELETE
    @Path("/credentials/{key}")
    ClientResponse<Void> removeThirdPartyCredential(@HeaderParam("sessionid") String sessionId, @PathParam("key") String key);

    @GET
    @Path("/credentials/")
    @Produces("application/json")
    ClientResponse<InputStream> thirdPartyCredentialsKeySet(@HeaderParam("sessionid") String sessionId);

}
