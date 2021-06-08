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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.HeaderParam;

import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;


/**
 * The client side stub for the RPC service in which there are listed all the RPC methods.
 */
@RemoteServiceRelativePath("scheduler")
public interface SchedulerService extends RemoteService {

    /**
     * Logout from the scheduler.
     *
     * @param sessionId the user session id used to logout from the scheduler.
     * @throws RestServerException exception thrown if problems occurred during the logout process.
     */
    void logout(String sessionId) throws RestServerException;

    /**
     * Removes several jobs from the scheduler.
     * @param sessionId the session id of the user that removes the job
     * @param jobIdList the list of the job ids that are to be removed
     * @return number of removed jobs
     */
    int removeJobs(String sessionId, List<Integer> jobIdList) throws RestServerException, ServiceException;

    /**
     * Pauses a job.
     * @param sessionId the session id of the user that pauses the job
     * @param list the list of the job ids that are to be paused
     * @return number of paused jobs
     */
    int pauseJobs(String sessionId, List<Integer> list) throws RestServerException, ServiceException;

    /**
     * By making an asynchronous call to the server, all in error tasks from the selected jobs are restarted.
     *
     * @param sessionId     the session id of the user which is logged in
     * @param list          the list of jobs which are to be resumed
     *                      or not.
     */
    int restartAllInErrorTasks(String sessionId, List<Integer> list) throws RestServerException, ServiceException;

    /**
     * Resumes a job.
     * @param sessionId the session id of the user that resumes the job
     * @param list the list of the job ids that are to be resumed
     * @return number of resumed jobs
     */
    int resumeJobs(String sessionId, List<Integer> list) throws RestServerException, ServiceException;

    /**
     * resbumit several jobs.
     * @param sessionId the session id of the user that resubmits the jobs
     * @param jobIdList the list of the job ids that are to be resubmitted
     * @return number of resubmitted jobs
     */
    int resubmitAllJobs(final String sessionId, List<Integer> jobIdList) throws RestServerException, ServiceException;

    /**
     * Kills several jobs.
     * @param sessionId the session id of the user that resumes the job
     * @param list the list of the job ids that are to be resumed
     * @return number of killed jobs
     */
    int killJobs(String sessionId, List<Integer> list) throws RestServerException, ServiceException;

    /**
     * Kill a task
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to kill within that job
     * @return true on success
     */
    boolean killTask(String sessionId, Integer jobId, String taskName) throws RestServerException, ServiceException;

    /**
     * Preempt a task
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to preempt within that job
     * @return true on success
     */
    boolean preemptTask(String sessionId, Integer jobId, String taskName) throws RestServerException, ServiceException;

    /**
     * Mark a task as finished and resume job
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to mark as finished within that job
     * @return true on success
     */
    boolean markAsFinishedAndResume(String sessionId, Integer jobId, String taskName)
            throws RestServerException, ServiceException;

    /**
     * Restart a task.
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to restart within that job
     * @return true on success
     */
    boolean restartRunningTask(String sessionId, Integer jobId, String taskName)
            throws RestServerException, ServiceException;

    /**
     * Restart a task paused on error.
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to restart within that job
     * @return true on success
     */
    boolean restartInErrorTask(String sessionId, Integer jobId, String taskName)
            throws RestServerException, ServiceException;

    /**
     * Gets the list of tasks that correspond to a job.
     * @param sessionId the session if of the user that asks for the tasks
     * @param jobId the id of the job for which its task list is asked 
     * @param offset the first task to show
     * @param limit the last task to show
     * @return the list of tasks as raw json
     */
    String getTasks(String sessionId, String jobId, int offset, int limit) throws RestServerException, ServiceException;

    /**
     *
     * @param jobId the id of the job for which its task list is asked
     * @param offset the first task to show
     * @param limit the last task to show
     * @param statusFilter represent a sequence of filters separated by semicolon
     * @return the list of tasks as raw json
     */
    String getTasks(String sessionId, String jobId, int offset, int limit, String statusFilter)
            throws RestServerException, ServiceException;

    /**
     * Gets the list of tasks that correspond to a job and filtered by a given tag.
     * @param sessionId the session if of the user that asks for the tasks
     * @param jobId the id of the job for which its task list is asked 
     * @param offset the first task to show
     * @param limit the last task to show
     * @param tag the tag used to filter the tasks.
     * @param statusFilter aggregation status to apply in filter
     * @return the list of tasks as raw json
     */
    String getTasksByTagAndStatus(String sessionId, String jobId, int offset, int limit, String tag,
            String statusFilter) throws RestServerException, ServiceException;

    String getTaskCentric(String sessionId, long fromDate, long toDate, boolean myTasks, String statusFilter,
            int offset, int limit, TasksCentricController.SortSpecifierRestContainer sortParameters)
            throws RestServerException, ServiceException;

    String getTaskCentricByTag(String sessionId, String tag, long fromDate, long toDate, boolean myTasks,
            String statusFilter, int offset, int limit,
            TasksCentricController.SortSpecifierRestContainer sortParameters)
            throws RestServerException, ServiceException;

    /**
     * Returns a list of the tags of the tasks belonging to job <code>jobId</code> and filtered by a prefix pattern
     * @param sessionId a valid session id
     * @param jobId jobid one wants to list the tasks' tags
     * @param prefix the prefix used to filter tags
     * @return a list tags
     */
    String getJobTaskTagsPrefix(String sessionId, String jobId, String prefix)
            throws RestServerException, ServiceException;

    /**
     * Gets the properties needed by the client. When loading properties we can only
     * change properties value on the server side. For changing properties on the client
     * side, the client has to send a request for getting these values and then set
     * them on his side.
     * @return the list of properties
     */
    Map<String, String> getProperties();

    /**
     * Get the display properties read by the scheduler
     * @return the list of properties
     */
    Map<String, String> getSchedulerPortalDisplayProperties(String sessionId);

    /**
     * Gets the job details.
     * @param sessionId the session id of the user which is logged in.
     * @param jobId the job id for which the details are asked.
     * @return the job detailed information as json
     */
    String getJobInfo(String sessionId, String jobId) throws RestServerException, ServiceException;

    /**
     * Gets the job info details.
     * @param sessionId the session id of the user which is logged in.
     * @param jobId the job id for which the details are asked.
     * @return the job detailed information as json
     */
    String getJobInfoDetails(String sessionId, String jobId) throws RestServerException, ServiceException;

    /**
     * Changes the priority for a job.
     * @param sessionId the session id of the user which is logged in
     * @param list the list of the job ids which are going to have their priority changed
     * @param priorityName the name of the new priority
     */
    void setPriorityByName(String sessionId, List<Integer> list, String priorityName)
            throws RestServerException, ServiceException;

    /**
     * Pauses the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the pause was successfully, false otherwise.
     */
    boolean pauseScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Resumes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the resume was successfully, false otherwise.
     */
    boolean resumeScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Removes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the freezing was successfully, false otherwise.
     */
    boolean freezeScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Kills the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the scheduler was successfully killed, false otherwise.
     */
    boolean killScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Shutdown the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the scheduler was successfully shutdown, false otherwise.
     */
    boolean shutdownScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Starts the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the scheduler was successfully started, false otherwise.
     */
    boolean startScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Stops the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @return true if the scheduler was successfully stopped, false otherwise.
     */
    boolean stopScheduler(final String sessionId) throws RestServerException, ServiceException;

    /**
     * Gets the output of a single task in a job
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @param taskName the name of the task, not the id
     * @param logMode one of {@link SchedulerServiceAsync#LOG_ALL}, {@link SchedulerServiceAsync#LOG_STDERR},
     * 	 {@link SchedulerServiceAsync#LOG_STDOUT}
     * @return the output of the task
     */
    String getTaskOutput(final String sessionId, final String jobId, final String taskName, final OutputMode logMode)
            throws RestServerException, ServiceException;

    /**
     * Gets the output of a job even for tasks that have not terminated yet
     * @param sessionId current session id
     * @param jobId id of the job for which logs should be fetched
     * @return console output for the whole job
     */
    String getLiveLogJob(final String sessionId, final String jobId) throws RestServerException, ServiceException;

    /**
     * Gets the number of bytes available in the job output stream for the given job id,
     * might be used to determine if fetch is necessary
     * @param sessionId current session id
     * @param jobId id of the job for which logs should be fetched
     * @return number of bytes available in the log for the given job
     */
    int getLiveLogJobAvailable(final String sessionId, final String jobId) throws RestServerException;

    /**
     * Clean the remote live log object
     * @param sessionId current session id
     * @param jobId id of the job for which live logs should be cleaned
     */
    boolean deleteLiveLogJob(final String sessionId, final String jobId) throws RestServerException, ServiceException;

    String getStatistics(String sessionId) throws RestServerException, ServiceException;

    String getStatisticsOnMyAccount(String sessionId) throws RestServerException, ServiceException;

    /**
    * Returns the revision number of the scheduler state
    * @param sessionId a valid session id.
    * @return the revision of the scheduler state
    */
    long schedulerStateRevision(String sessionId) throws RestServerException;

    /**
     * Returns a map containing one entry with the revision id as key and the
     * list of UserJobInfo as value.
     * each jobs is described using
     *   - its id
     *   - its owner
     *   - the JobInfo class
     * @param sessionId a valid session id
     * @param startCursor start cursor
     * @param endCursor end cursor
     * @param pending fetch pending jobs
     * @param running fetch running jobs
     * @param finished fetch finished jobs
     * @return the raw json jobinfo from the rest api
     */

    String revisionAndjobsinfo(@HeaderParam("sessionid") String sessionId, String startCursor, String endCursor,
            int pageSize, boolean first, String user, boolean pending, boolean running, boolean finished,
            FilterModel filterModel) throws RestServerException, ServiceException;

    /**
     * Image representing the job as designed in the WF studio when applicable
     * @param sessionId current session
     * @param jobId id of the job
     * @return path of the image on the server
     */
    String getJobImage(@HeaderParam("sessionid") String sessionId, String jobId)
            throws RestServerException, ServiceException;

    /**
     * Html representing the job as designed in the WF studio when applicable
     * @param sessionId current session
     * @param jobId id of the job
     * @return path of the image on the server
     */
    String getJobHtml(@HeaderParam("sessionid") String sessionId, String jobId)
            throws RestServerException, ServiceException;

    /**
     * Check job variables access permission.
     * @param sessionId the session id of the user which is logged in.
     * @param jobId the job id for which the details are asked.
     * @param method method to request access
     * @return true if the user can access the variables, false otherwise
     */
    String checkJobPermissionMethod(String sessionId, String jobId, String method)
            throws RestServerException, ServiceException;;

    /**
     * Returns the Scheduler status as a JSON String
     * @param sessionId a valid session id
     * @return the current scheduler status
     */
    String getSchedulerStatus(@HeaderParam("sessionid") String sessionId) throws RestServerException;

    /**
     * returns the list of users currently connected to the scheduler
     * @param sessionId current session id
     * @return list of users as json array
     */
    String getSchedulerUsers(@HeaderParam("sessionid") String sessionId) throws RestServerException, ServiceException;

    /**
     * returns the list of users having jobs in the scheduler
     * @param sessionId current session id
     * @return list of users as json array
     */
    String getSchedulerUsersWithJobs(@HeaderParam("sessionid") String sessionId)
            throws RestServerException, ServiceException;

    /**
     * @return version string of the REST api
     */
    String getVersion() throws RestServerException, ServiceException;

    /**
     * Get server logs for a given task
     *
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to restart within that job
     * @return task logs
     */
    String getTaskServerLogs(String sessionId, Integer jobId, String taskName)
            throws RestServerException, ServiceException;

    /**
     * Get server logs for a given job
     *
     * @param sessionId current session
     * @param jobId id of a job
     * @return job logs
     */
    String getJobServerLogs(String sessionId, Integer jobId) throws RestServerException, ServiceException;

    List<JobUsage> getUsage(String sessionId, String user, Date startDate, Date endDate)
            throws RestServerException, ServiceException;

    void putThirdPartyCredential(String sessionId, String key, String value) throws RestServerException;

    Set<String> thirdPartyCredentialKeySet(String sessionId) throws ServiceException, RestServerException;

    void removeThirdPartyCredential(String sessionId, String key) throws RestServerException;

    String getPreciousTaskName(String sessionId, String jobId) throws ServiceException, RestServerException;

    /**
     * @param sessionId d of the current session
     * @return true if user has right to access RM portal
     */
    String portalAccess(String sessionId) throws ServiceException, RestServerException;

    /**
     *
     * @param sessionId the current session id
     * @param signal the signal to be sent to the job
     * @param jobId id of the job
     * @throws RestServerException exception thrown if problems occurred during the addJobSignal process.
     */
    Set<String> addJobSignal(final String sessionId, String signal, String jobId) throws RestServerException;

}
