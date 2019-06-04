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

import java.io.InputStream;
import java.util.Map;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

import org.jboss.resteasy.annotations.GZIP;
import org.jboss.resteasy.plugins.providers.multipart.MultipartInput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricController;


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
     */
    @PUT
    @Path("disconnect")
    void disconnect(@HeaderParam("sessionid")
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
    InputStream jobs(@HeaderParam("sessionid") String sessionId);

    /**
     * Submits a job to the Scheduler.
     * @param sessionId the session id of the user that performs the submission
     * @param multipart the multipart message that encodes the job descriptor file 
     * @return a ClientResponse containing the response status and the job id generated, in case of success. 
     */
    @POST
    @Path("submit")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    String jobs(@HeaderParam("sessionid") String sessionId, MultipartInput multipart);

    /**
     * Submit a flat command job
     * Each line in the file is a command that will be run on a different node
     * @param commandFileContent content of the flat command file, one task per line
     * @param jobName name of the job
     * @param selectionScriptContent selection script or null
     */
    @POST
    @Path("submitflat")
    @Produces("application/json")
    String submitFlat(@HeaderParam("sessionid") String sessionId,
            @FormParam("commandFileContent") String commandFileContent, @FormParam("jobName") String jobName,
            @FormParam("selectionScriptContent") String selectionScriptContent,
            @FormParam("selectionScriptExtension") String selectionScriptExtension);

    /**
     * Deletes a job from the Scheduler.
     * @param sessionId the session id of the user that performs the deletion
     * @param jobId the id of the job that will be deleted
     * @return a ClientResponse containing the response status and true - if the removed was successfully, false - otherwise.
     */
    @DELETE
    @Path("jobs/{jobid}")
    InputStream removeJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Pauses a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job that will be deleted
     * @return a ClientResponse containing the response status and true - if the job was successfully paused, false - otherwise. 
     */
    @PUT
    @Path("jobs/{jobid}/pause")
    InputStream pauseJob(@HeaderParam("sessionid")
    final String sessionId, @PathParam("jobid")
    final String jobId);

    /**
     * Restart all in error tasks in a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the job id which will be resumed
     * @return a ClientResponse containing the response status and true - if the job was successfully restarted, false - otherwise.
     */
    @PUT
    @Path("jobs/{jobid}/restartAllInErrorTasks")
    InputStream restartAllTasksInError(@HeaderParam("sessionid")
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
    InputStream resumeJob(@HeaderParam("sessionid")
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
    InputStream killJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Kill a task
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/kill")
    InputStream killTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Preempt a task
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/preempt")
    InputStream preemptTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Mark as finished and resume
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/finishInErrorTask")
    InputStream markAsFinishedAndResume(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Restart a running task.
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/restart")
    InputStream restartTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Restart a task paused on error.
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/restartInErrorTask")
    InputStream restartInErrorTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

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
    InputStream getJobTaskStates(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Returns a list of taskState with pagination
     * @param sessionId a valid session id
     * @param jobId the job id
     * @return a list of task' states of the job <code>jobId</code>
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates/paginated")
    @Produces("application/json")
    InputStream getJobTaskStatesPaginated(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("50") int limit);

    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates/filtered/paginated")
    @Produces("application/json")
    InputStream getJobTaskStatesPaginated(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("50") int limit,
            @QueryParam("statusFilter") @DefaultValue("") String statusFilter);

    /**
     * Gets the list of tasks in a JSON array for a given job and filtered by a given tag.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the job id for which the tasks are asked.
     * @param tag the tag used to filter the tasks.
     * @return a ClientResponse containing the response status and the JSON array including the task list, in case of success.
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates/{tasktag}")
    @Produces("application/json")
    InputStream getJobTaskStatesByTag(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("tasktag") String tag);

    /**
     * Returns a list of taskState of the tasks filtered by a given tag and paginated.
     * @param sessionId a valid session id.
     * @param jobId the job id.
     * @param taskTag the tag used to filter the tasks.
     * @param offset the number of the first task to fetch
     * @param limit the number of the last task to fetch (non inclusive)
     * @return a list of task' states of the job <code>jobId</code> filtered by a given tag, for a given pagination.
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates/{tasktag}/paginated")
    @Produces("application/json")
    InputStream getJobTaskStatesByTagPaginated(@HeaderParam("sessionid") String sessionId,
            @PathParam("jobid") String jobId, @PathParam("tasktag") String taskTag,
            @QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("50") int limit);

    /**
     * Returns a list of taskState of the tasks filtered by a given tag and paginated.
     * @param sessionId a valid session id.
     * @param jobId the job id.
     * @param offset the number of the first task to fetch
     * @param limit the number of the last task to fetch (non inclusive)
     * @param taskTag the tag used to filter the tasks.
     * @param statusFilter aggregation status to apply in filter
     * @return a list of task' states of the job <code>jobId</code> filtered by a given tag, for a given pagination.
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates/{tasktag}/{statusFilter}/paginated")
    @Produces("application/json")
    InputStream getJobTaskStatesByTagAndStatusPaginated(@HeaderParam("sessionid") String sessionId,
            @PathParam("jobid") String jobId, @QueryParam("offset") @DefaultValue("0") int offset,
            @QueryParam("limit") @DefaultValue("50") int limit, @PathParam("tasktag") String taskTag,
            @PathParam("statusFilter") String statusFilter);

    /**
     * Returns a paginated list of <code>TaskStateData</code> regarding the given parameters (decoupled from the associated jobs).
     * The result is paginated using the optional <code>offset</code> and <code>limit</code> parameters.
     * If those parameters are not specified, the following values will be used: [0, DEFAULT_VALUE[
     * The DEFAULT_VALUE can be set in the scheduler config file as the <code>pa.scheduler.tasks.page.size</code> parameter.
     * 
     * @param sessionId  a valid session id.
     * @param from  the scheduled date to which we start fetching tasks. The format is in Epoch time.
     * @param to  the end scheduled end date to stop fetching tasks. The format is in Epoch time.
     * @param mytasks  <code>True</code> if you want to fetch only the user's tasks. Default value is <code>False</code>.
     * @param running  fetch running tasks. Default value is <code>True</code>.
     * @param pending  fetch pending tasks. Default value is <code>True</code>.
     * @param finished  fetch finished tasks. Default value is <code>True</code>.
     * @param offset  the index of the first task to fetch (for pagination).
     * @param limit  the index of the last (excluded) task to fetch (for pagination).
     * @param sortParameters  the tasks sorting parameters.
     * @return a list of <code>TaskStateData</code>  and the total number of them.
     */
    @GET
    @GZIP
    @Path("taskstates")
    @Produces("application/json")
    InputStream getTaskStates(@HeaderParam("sessionid") String sessionId,
            @QueryParam("from") @DefaultValue("0") long from, @QueryParam("to") @DefaultValue("0") long to,
            @QueryParam("mytasks") @DefaultValue("false") boolean mytasks,
            @QueryParam("running") @DefaultValue("true") boolean running,
            @QueryParam("pending") @DefaultValue("true") boolean pending,
            @QueryParam("finished") @DefaultValue("true") boolean finished,
            @QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("-1") int limit,
            @QueryParam("sortparameters") TasksCentricController.SortSpecifierRestContainer sortParameters);

    /**
     * Returns a paginated list of <code>TaskStateData</code> regarding the given parameters (decoupled from the associated jobs).
     * The result is paginated using the optional <code>offset</code> and <code>limit</code> parameters.
     * If those parameters are not specified, the following values will be used: [0, DEFAULT_VALUE[
     * The DEFAULT_VALUE can be set in the scheduler config file as the <code>pa.scheduler.tasks.page.size</code> parameter.
     * 
     * @param sessionId  a valid session id.
     * @param taskTag  tag to filter the tasks. The tag should be complete as the criteria is strict.
     * @param from  the scheduled date to which we start fetching tasks. The format is in Epoch time.
     * @param to  the end scheduled end date to stop fetching tasks. The format is in Epoch time.
     * @param mytasks <code>True</code> if you want to fetch only the user's tasks. <code>False</code> will fetch everything.
     * @param running  fetch running tasks. Default value is <code>True</code>.
     * @param pending  fetch pending tasks. Default value is <code>True</code>.
     * @param finished  fetch finished tasks. Default value is <code>True</code>.
     * @param offset  the index of the first task to fetch (for pagination).
     * @param limit  the index of the last (excluded) task to fetch (for pagination).
     * @param sortParameters  the tasks sorting parameters.
     * @return a list of <code>TaskStateData</code>  and the total number of them.
     */
    @GET
    @GZIP
    @Path("taskstates/tag/{tasktag}")
    @Produces("application/json")
    InputStream getTaskStatesByTag(@HeaderParam("sessionid") String sessionId, @PathParam("tasktag") String taskTag,
            @QueryParam("from") @DefaultValue("0") long from, @QueryParam("to") @DefaultValue("0") long to,
            @QueryParam("mytasks") @DefaultValue("false") boolean mytasks,
            @QueryParam("running") @DefaultValue("true") boolean running,
            @QueryParam("pending") @DefaultValue("true") boolean pending,
            @QueryParam("finished") @DefaultValue("true") boolean finished,
            @QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("-1") int limit,
            @QueryParam("sortparameters") TasksCentricController.SortSpecifierRestContainer sortParameters);

    /**
     * Returns a list of the tags of the tasks belonging to job <code>jobId</code> and filtered by a prefix pattern
     * @param sessionId a valid session id
     * @param jobId jobid one wants to list the tasks' tags
     * @param prefix the prefix used to filter tags
     * @return a list of tasks' name
     */
    @GET
    @Path("jobs/{jobid}/tasks/tags/startsWith/{prefix}")
    @Produces("application/json")
    InputStream getJobTaskTagsPrefix(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("prefix") String prefix);

    /**
     * Gets the state of a certain job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @return a ClientResponse containing the response status and an InputStream containing information about the state of the job.
     */
    @GET
    @Path("jobs/{jobid}")
    @Produces("application/json")
    InputStream job(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Returns the job info associated to the job referenced by the 
     * id <code>jobid</code>
     * @param sessionId a valid session id
     * @return  a ClientResponse containing the job info of the corresponding job
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/info")
    @Produces("application/json")
    InputStream jobInfo(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Returns the job's original workflow as XML.
     * id <code>jobid</code>
     * @param sessionId a valid session id
     * @return  a ClientResponse containing the Workflow's XML of the corresponding job
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/xml")
    @Produces("application/xml")
    InputStream getJobXML(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Changes the priority of a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @param priorityName the new priority of the job 
     */
    @PUT
    @Path("jobs/{jobid}/priority/byname/{name}")
    void schedulerChangeJobPriorityByName(@HeaderParam("sessionid")
    final String sessionId, @PathParam("jobid")
    final String jobId, @PathParam("name") String priorityName);

    /**
     * Pauses the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully paused and false in case of a
     * failure
     */
    @PUT
    @Path("pause")
    InputStream pauseScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Resumes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully resumed and false in case of a
     * failure
     */
    @PUT
    @Path("resume")
    InputStream resumeScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Freezes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully resumed and false in case of a
     * failure
     */
    @PUT
    @Path("freeze")
    InputStream freezeScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Kills the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully killed and false in case of a
     * failure
     */
    @PUT
    @Path("kill")
    InputStream killScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Shutdown the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully shutdown and false in case of a
     * failure
     */
    @PUT
    @Path("shutdown")
    InputStream shutdownScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Starts the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was successfully started and false in case of a
     * failure
     */
    @PUT
    @Path("start")
    InputStream startScheduler(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Stops the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return a ClientResponse containing the response status and true if the Scheduler was stopped paused and false in case of a
     * failure
     */
    @PUT
    @Path("stop")
    InputStream stopScheduler(@HeaderParam("sessionid")
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
    InputStream getJobTasksIds(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

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
    String tasklog(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

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
    String taskStdout(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

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
    String taskStderr(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

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
    String getLiveLogJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * number of available bytes in the stream or -1 if the stream does not exist.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @GET
    @Path("jobs/{jobid}/livelog/available")
    @Produces("application/json")
    String getLiveLogJobAvailable(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * remove the live log object.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @DELETE
    @Path("jobs/{jobid}/livelog")
    @Produces("application/json")
    InputStream deleteLiveLogJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

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
    String taskServerLogs(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

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
    String jobServerLogs(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Gets the result of a task.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task belongs
     * @param taskId the id of the task to which the result is asked
     * @return the result of the task
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/value")
    @Produces("*/*")
    InputStream taskresult(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

    /**
     * Gets the result of a task.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task belongs
     * @param taskId the id of the task to which the result is asked
     * @return the result of the task
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/metadata")
    @Produces("application/json")
    InputStream taskResultMetadata(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

    /**
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job to which the task belongs
     * @return all precious task results' metadata associated to the <code>jobId</code>
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/results/precious/metadata")
    @Produces("application/json")
    InputStream getPreciousTaskName(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Gets the serialized result of a task.
     *
     * @param sessionId the session id of the user which is logged in
     * @param jobId     the id of the job to which the task belongs
     * @param taskId    the id of the task to which the result is asked
     * @return the serialized result of the task
     */
    @GET
    @GZIP
    @Path("jobs/{jobid}/tasks/{taskid}/result/serializedvalue")
    @Produces("*/*")
    InputStream taskSerializedResult(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

    /**
     * returns statistics about the scheduler
     * @param sessionId the session id associated to this new connection
     * @return a string containing the statistics
     */
    @GET
    @Path("stats")
    @Produces("application/json")
    String getStatistics(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * returns a string containing some data regarding the user's account
     * @param sessionId the session id associated to this new connection
     * @return a string containing some data regarding the user's account
     */
    @GET
    @Path("stats/myaccount")
    @Produces("application/json")
    String getStatisticsOnMyAccount(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Returns the revision number of the scheduler state
     * @param sessionId a valid session id.
     * @return the revision of the scheduler state
     */
    @GET
    @Path("state/revision")
    @Produces({ "application/json", "application/xml" })
    String schedulerStateRevision(@HeaderParam("sessionid") String sessionId);

    /**
     * Returns an html visualization corresponding of a jobid
     * @param sessionId a valid session id
     * @param jobId the job id
     * @return an html visualization corresponding of a <code>jobId</code>
     */
    @GET
    @Path("jobs/{jobid}/html")
    @Produces("text/html")
    InputStream getJobHtml(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Users currently connected to the scheduler
     * 
     * @param sessionId the session id associated to this new connection
     */
    @GET
    @GZIP
    @Path("users")
    @Produces({ "application/json", "application/xml" })
    InputStream getSchedulerUsers(@HeaderParam("sessionid") String sessionId);

    /**
     * Users having jobs in the scheduler
     * 
     * @param sessionId the session id associated to this new connection
     */
    @GET
    @GZIP
    @Path("userswithjobs")
    @Produces({ "application/json", "application/xml" })
    InputStream getSchedulerUsersWithJobs(@HeaderParam("sessionid") String sessionId);

    /**
     * Returns the Scheduler status as a String, 
     * ie org.ow2.proactive.scheduler.common.SchedulerStatus.toString()
     * @param sessionId a valid session id
     * @return a String describing the current scheduler status
     */
    @GET
    @Path("status")
    String schedulerStatus(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("version")
    InputStream getVersion();

    @GET
    @Path("usage/myaccount")
    @Produces("application/json")
    InputStream getUsageOnMyAccount(@HeaderParam("sessionid") String sessionId,
            @QueryParam("startdate") String startDate, @QueryParam("enddate") String endDate);

    @GET
    @Path("usage/account")
    @Produces("application/json")
    InputStream getUsageOnAccount(@HeaderParam("sessionid") String sessionId, @QueryParam("user") String user,
            @QueryParam("startdate") String startDate, @QueryParam("enddate") String endDate);

    @POST
    @Path("/credentials/{key}")
    void putThirdPartyCredential(@HeaderParam("sessionid") String sessionId, @PathParam("key") @Encoded String key,
            @FormParam("value") String value);

    @DELETE
    @Path("/credentials/{key}")
    void removeThirdPartyCredential(@HeaderParam("sessionid") String sessionId, @PathParam("key") @Encoded String key);

    @GET
    @Path("/credentials/")
    @Produces("application/json")
    InputStream thirdPartyCredentialsKeySet(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("configuration/portal")
    @Produces("application/json")
    Map<Object, Object> getSchedulerPortalDisplayProperties(@HeaderParam("sessionid") String sessionId);

}
