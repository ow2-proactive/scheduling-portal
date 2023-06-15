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
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.PathSegment;

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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
    String submitFlat(@HeaderParam("sessionid") String sessionId,
            @FormParam("commandFileContent") String commandFileContent, @FormParam("jobName") String jobName,
            @FormParam("selectionScriptContent") String selectionScriptContent,
            @FormParam("selectionScriptExtension") String selectionScriptExtension);

    /**
     * Re-submit a list of jobs to the scheduler
     *
     * @param sessionId
     *          a valid session id
     * @param jobsId
     *          a list of job ids of already submitted jobs
     * @return a list of job ids of the newly created jobs. If a job submission fails, it will be discarded with a log message.
     */
    @POST
    @Path("jobs/resubmit")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream reSubmitAll(@HeaderParam("sessionid") String sessionId, @QueryParam("jobsid") List<String> jobsId);

    /**
     * Deletes a job from the Scheduler.
     * @param sessionId the session id of the user that performs the deletion
     * @param jobId the id of the job that will be deleted
     * @return a ClientResponse containing the response status and true - if the removed was successfully, false - otherwise.
     */
    @DELETE
    @Path("jobs/{jobid}")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream removeJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    @DELETE
    @Path("jobs")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream removeJobs(@HeaderParam("sessionid") String sessionId, @QueryParam("jobsid") List<String> jobsId,
            @QueryParam("olderThan") long olderThan);

    /**
     * Pauses a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job that will be deleted
     * @return a ClientResponse containing the response status and true - if the job was successfully paused, false - otherwise.
     */
    @PUT
    @Path("jobs/{jobid}/pause")
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
    InputStream killJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    @PUT
    @Path("jobs/kill")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream killJobs(@HeaderParam("sessionid") String sessionId, @QueryParam("jobsid") List<String> jobsId);

    /**
     * Kill a task
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/kill")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream killTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Preempt a task
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/preempt")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream preemptTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Mark as finished and resume
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/finishInErrorTask")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream markAsFinishedAndResume(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Restart a running task.
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/restart")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream restartTask(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskname") String taskName);

    /**
     * Restart a task paused on error.
     */
    @PUT
    @Path("jobs/{jobid}/tasks/{taskname}/restartInErrorTask")
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getJobTaskStatesPaginated(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @QueryParam("offset") @DefaultValue("0") int offset, @QueryParam("limit") @DefaultValue("50") int limit);

    @GET
    @GZIP
    @Path("jobs/{jobid}/taskstates/filtered/paginated")
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getJobTaskStatesByTagAndStatusPaginated(@HeaderParam("sessionid") String sessionId,
            @PathParam("jobid") String jobId, @QueryParam("offset") @DefaultValue("0") int offset,
            @QueryParam("limit") @DefaultValue("50") int limit, @PathParam("tasktag") String taskTag,
            @PathParam("statusFilter") PathSegment statusFilter);

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
     * @param offset  the index of the first task to fetch (for pagination).
     * @param limit  the index of the last (excluded) task to fetch (for pagination).
     * @param sortParameters  the tasks sorting parameters.
     * @return a list of <code>TaskStateData</code>  and the total number of them.
     */
    @GET
    @GZIP
    @Path("taskstates")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getTaskStates(@HeaderParam("sessionid") String sessionId,
            @QueryParam("from") @DefaultValue("0") long from, @QueryParam("to") @DefaultValue("0") long to,
            @QueryParam("mytasks") @DefaultValue("false") boolean mytasks,
            @QueryParam("statusFilter") @DefaultValue("") String statusFilter,
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
     * @param offset  the index of the first task to fetch (for pagination).
     * @param limit  the index of the last (excluded) task to fetch (for pagination).
     * @param sortParameters  the tasks sorting parameters.
     * @return a list of <code>TaskStateData</code>  and the total number of them.
     */
    @GET
    @GZIP
    @Path("taskstates/tag/{tasktag}")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getTaskStatesByTag(@HeaderParam("sessionid") String sessionId, @PathParam("tasktag") String taskTag,
            @QueryParam("from") @DefaultValue("0") long from, @QueryParam("to") @DefaultValue("0") long to,
            @QueryParam("mytasks") @DefaultValue("false") boolean mytasks,
            @QueryParam("statusFilter") @DefaultValue("") String statusFilter,
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_XML)
    InputStream getJobXML(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Changes the priority of a job.
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @param priorityName the new priority of the job
     */
    @PUT
    @Path("jobs/{jobid}/priority/byname/{name}")
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
    String getLiveLogJob(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * number of available bytes in the stream or -1 if the stream does not exist.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @GET
    @Path("jobs/{jobid}/livelog/available")
    @Produces(MediaType.APPLICATION_JSON)
    String getLiveLogJobAvailable(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * remove the live log object.
     * @param sessionId a valid session id
     * @param jobId the id of the job to retrieve
     */
    @DELETE
    @Path("jobs/{jobid}/livelog")
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
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
    @Produces({ MediaType.TEXT_PLAIN, MediaType.APPLICATION_JSON })
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.APPLICATION_JSON)
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
    @Produces(MediaType.WILDCARD)
    InputStream taskSerializedResult(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("taskid") String taskId);

    /**
     * returns statistics about the scheduler
     * @param sessionId the session id associated to this new connection
     * @return a string containing the statistics
     */
    @GET
    @Path("stats")
    @Produces(MediaType.APPLICATION_JSON)
    String getStatistics(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * returns a string containing some data regarding the user's account
     * @param sessionId the session id associated to this new connection
     * @return a string containing some data regarding the user's account
     */
    @GET
    @Path("stats/myaccount")
    @Produces(MediaType.APPLICATION_JSON)
    String getStatisticsOnMyAccount(@HeaderParam("sessionid")
    final String sessionId);

    /**
     * Returns the revision number of the scheduler state
     * @param sessionId a valid session id.
     * @return the revision of the scheduler state
     */
    @GET
    @Path("state/revision")
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
    String schedulerStateRevision(@HeaderParam("sessionid") String sessionId);

    /**
     * Returns an html visualization corresponding of a jobid
     * @param sessionId a valid session id
     * @param jobId the job id
     * @return an html visualization corresponding of a <code>jobId</code>
     */
    @GET
    @Path("jobs/{jobid}/html")
    @Produces("application/json;charset=utf-8")
    InputStream getJobHtml(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId);

    /**
     * Users currently connected to the scheduler
     *
     * @param sessionId the session id associated to this new connection
     */
    @GET
    @GZIP
    @Path("users")
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
    InputStream getSchedulerUsers(@HeaderParam("sessionid") String sessionId);

    /**
     * Users having jobs in the scheduler
     *
     * @param sessionId the session id associated to this new connection
     */
    @GET
    @GZIP
    @Path("userswithjobs")
    @Produces({ MediaType.APPLICATION_JSON, MediaType.APPLICATION_XML })
    InputStream getSchedulerUsersWithJobs(@HeaderParam("sessionid") String sessionId);

    /**
     * Returns the Scheduler status as a String,
     * ie org.ow2.proactive.scheduler.common.SchedulerStatus.toString()
     * @param sessionId a valid session id
     * @return a String describing the current scheduler status
     */
    @GET
    @Path("status")
    @Produces(MediaType.APPLICATION_JSON)
    String schedulerStatus(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("version")
    @Produces(MediaType.TEXT_PLAIN)
    InputStream getVersion();

    @GET
    @Path("usage/myaccount")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getUsageOnMyAccount(@HeaderParam("sessionid") String sessionId,
            @QueryParam("startdate") String startDate, @QueryParam("enddate") String endDate);

    @GET
    @Path("usage/account")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getUsageOnAccount(@HeaderParam("sessionid") String sessionId, @QueryParam("user") String user,
            @QueryParam("startdate") String startDate, @QueryParam("enddate") String endDate);

    @POST
    @Path("/credentials/{key}")
    @Produces(MediaType.APPLICATION_JSON)
    void putThirdPartyCredential(@HeaderParam("sessionid") String sessionId, @PathParam("key") @Encoded String key,
            @FormParam("value") String value);

    @DELETE
    @Path("/credentials/{key}")
    @Produces(MediaType.APPLICATION_JSON)
    void removeThirdPartyCredential(@HeaderParam("sessionid") String sessionId, @PathParam("key") @Encoded String key);

    @GET
    @Path("/credentials/")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream thirdPartyCredentialsKeySet(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("configuration/portal")
    @Produces(MediaType.APPLICATION_JSON)
    Map<Object, Object> getSchedulerPortalDisplayProperties(@HeaderParam("sessionid") String sessionId);

    @GET
    @Path("job/{jobid}/permission/{method}")
    @Produces(MediaType.APPLICATION_JSON)
    InputStream checkJobPermissionMethod(@HeaderParam("sessionid") String sessionId, @PathParam("jobid") String jobId,
            @PathParam("method") String method);

    @POST
    @Path("job/{jobid}/signals")
    @Consumes(value = MediaType.APPLICATION_OCTET_STREAM)
    @Produces(MediaType.APPLICATION_JSON)
    Set<String> addJobSignal(@HeaderParam("sessionid") String sessionId, @QueryParam("signal") String signal,
            @PathParam("jobid") String jobId);

    @POST
    @Path("job/{jobid}/signals")
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    InputStream addJobSignalWithVariables(@HeaderParam("sessionid") String sessionId,
            @QueryParam("signal") String signal, @PathParam("jobid") String jobId,
            Map<String, String> updatedVariables);

    @POST
    @Path("job/{jobid}/signals/validate")
    @Consumes(value = MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    String validateJobSignal(@HeaderParam("sessionid") String sessionId, @QueryParam("signal") String signal,
            @PathParam("jobid") String jobId, Map<String, String> updatedVariables);

    @GET
    @Path("labels")
    @Consumes(value = MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    InputStream getLabels(@HeaderParam("sessionid") String sessionId);

    @PUT
    @Path("labels/jobs/setlabel")
    @Consumes(value = MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    void setLabelOnJobs(@HeaderParam("sessionid") String sessionId, @QueryParam("labelId") String labelId,
            List<String> jobIds);

    @PUT
    @Path("labels/jobs/removelabel")
    @Consumes(value = MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    void removeJobLabel(@HeaderParam("sessionid") String sessionId, List<String> jobIds);

    @POST
    @Path("labels/set")
    @Consumes(value = MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    InputStream setLabels(@HeaderParam("sessionid") String sessionId, List<String> labels);

    @GET
    @Path("properties")
    @Produces(MediaType.APPLICATION_JSON)
    Map<String, Object> getSchedulerPropertiesFromSessionId(@HeaderParam("sessionid")
    final String sessionId);

}
