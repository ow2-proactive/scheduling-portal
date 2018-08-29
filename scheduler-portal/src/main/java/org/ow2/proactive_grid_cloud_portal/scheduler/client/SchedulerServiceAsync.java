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

import org.ow2.proactive_grid_cloud_portal.common.shared.RestServerException;
import org.ow2.proactive_grid_cloud_portal.common.shared.ServiceException;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.filter.FilterModel;

import com.google.gwt.http.client.Request;
import com.google.gwt.user.client.rpc.AsyncCallback;


/**
 * The async counterpart of <code>SchedulerService</code> that is called from the client-side code.
 */
public interface SchedulerServiceAsync {

    public static final int LOG_ALL = 1;

    public static final int LOG_STDOUT = 2;

    public static final int LOG_STDERR = 3;

    public static final int LOG_FULL = 4;

    /**
     * Method used for making an asynchronous call to the server for logout.
     * @param sessionId the sessionId used for logout
     * @param callback the object used for notifying the caller when the asynchronous call is completed.
     */
    void logout(String sessionId, AsyncCallback<Void> callback);

    /**
     * Method used for making an asynchronous call to the server for removing a job.
     * @param sessionId the user's session id 
     * @param list the list of jobs which are to be removed
     * @param isJobRemoved the result showing whether the removed was successfully or not. 
     */
    void removeJobs(String sessionId, List<Integer> list, AsyncCallback<Integer> isJobRemoved);

    /**
     * Pausing a job by making an asynchronous call to the server. 
     * @param sessionId the session id of the user which is logged in
     * @param jobIdList the list of jobs which are to be paused
     * @param asyncCallback the result retrieved from the server which shows if the paused was successfully
     * or not.
     */
    void pauseJobs(String sessionId, List<Integer> jobIdList, AsyncCallback<Integer> asyncCallback);

    /**
     * By making an asynchronous call to the server, all in error tasks from the selected jobs are restarted.
     * @param sessionId the session id of the user which is logged in
     * @param list the list of jobs which are to be resumed
     * @param asyncCallback the result retrieved from the server which shows if the in error tasks were successfully
     * or not.
     */
    void restartAllInErrorTasks(String sessionId, List<Integer> list, AsyncCallback<Integer> asyncCallback);

    /**
     * By making an asynchronous call to the server, several jobs are resumed.
     * @param sessionId the session id of the user which is logged in
     * @param list the list of jobs which are to be resumed
     * @param asyncCallback the result retrieved from the server which shows if the jobs were resumed successfully
     * or not.
     */
    void resumeJobs(String sessionId, List<Integer> list, AsyncCallback<Integer> asyncCallback);

    /**
     * By making an asynchronous call to the server, several jobs are killed.
     * @param sessionId the session id of the user which is logged in
     * @param list the list of jobs which are to be killed
     * @param asyncCallback the result retrieved from the server which shows if the jobs were killed successfully
     * or not. 
     */
    void killJobs(String sessionId, List<Integer> list, AsyncCallback<Integer> asyncCallback);

    /**
     * Kill a task within a given job
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of the task to kill
     * @param cb
     */
    void killTask(String sessionId, Integer jobId, String taskName, AsyncCallback<Boolean> cb);

    /**
     * Preempt a task within a given job
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of the task to preempt
     * @param cb
     */
    void preemptTask(String sessionId, Integer jobId, String taskName, AsyncCallback<Boolean> cb);

    /**
     * Mark a task a finished and resume job
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of the task to preempt
     * @param cb
     */
    void markAsFinishedAndResume(String sessionId, Integer jobId, String taskName, AsyncCallback<Boolean> cb);

    /**
     * Restart a task within a given job
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of the task to restart
     * @param cb
     */
    void restartRunningTask(String sessionId, Integer jobId, String taskName, AsyncCallback<Boolean> cb);

    /**
     * Restart a task paused on error.
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to restart within that job
     * @return true on success
     * @throws RestServerException
     * @throws ServiceException
     */
    void restartInErrorTask(String sessionId, Integer jobId, String taskName, AsyncCallback<Boolean> cb);

    /**
     * Method used for making an asynchronous call to the server for returning a list of
     * tasks that correspond to a certain job.
     * @param sessionId the session if of the user that asks for the tasks
     * @param jobId the id of the job for which its task list is asked 
     * @param callback the object used for notifying the caller when the asynchronous call is completed.
     */
    Request getTasks(String sessionId, String jobId, int offset, int limit, AsyncCallback<String> callback);

    /**
     * Method used for making an asynchronous call to the server for returning a list of 
     * tasks that correspond to a job and filtered by a given tag.
     * @param sessionId the session if of the user that asks for the tasks
     * @param jobId the id of the job for which its task list is asked 
     * @param tag the tag used to filter the tasks.
     * @param callback the object used for notifying the caller when the asynchronous call is completed.
     */
    Request getTasksByTag(String sessionId, String jobId, String tag, int offset, int limit,
            AsyncCallback<String> callback);

    Request getTaskCentric(String sessionId, long fromDate, long toDate, boolean myTasks, boolean pending,
            boolean running, boolean finished, int offset, int limit,
            TasksCentricController.SortSpecifierRestContainer sortParameters, AsyncCallback<String> callback);

    Request getTaskCentricByTag(String sessionId, String tag, long fromDate, long toDate, boolean myTasks,
            boolean pending, boolean running, boolean finished, int offset, int limit,
            TasksCentricController.SortSpecifierRestContainer sortParameters, AsyncCallback<String> callback);

    /**
     * Returns a list of the tags of the tasks belonging to job <code>jobId</code> and filtered by a prefix pattern
     * @param sessionId a valid session id
     * @param jobId jobid one wants to list the tasks' tags
     * @param prefix the prefix used to filter tags
     * @param callback the object used for notifying the caller when the asynchronous call is completed.
     */
    Request getJobTaskTagsPrefix(String sessionId, String jobId, String prefix, AsyncCallback<String> callback);

    /**
     * Gets the properties needed by the client. When loading properties we can only
     * change properties value on the server side. For changing properties on the client
     * side, the client has to send a request for getting these values and then set
     * them on his side.
     *
     * @param propertyList list of properties
     */
    void getProperties(AsyncCallback<Map<String, String>> propertyList);

    /**
     * Get the display properties read by the scheduler
     * @param sessionId the session id of the user which is logged in
     * @param asyncCallback
     */
    void getSchedulerPortalDisplayProperties(String sessionId, AsyncCallback<Map<String, String>> asyncCallback);

    /**
     * Gets the job details by making an asynchronous call to the server. 
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job for which the request is done.
     * @param callback the result returned.
     */
    void getJobInfo(String sessionId, String jobId, AsyncCallback<String> callback);

    /**
     * Gets the job info details by making an asynchronous call to the server. 
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job for which the request is done.
     * @param callback the result returned.
     */
    void getJobInfoDetails(String sessionId, String jobId, AsyncCallback<String> callback);

    /**
     * Sets the priority of a job.
     * @param sessionId the session id of the user which is logged in
     * @param list the list of the job ids which are going to have their priority changed
     * @param priorityName the name of the new priority
     * @param callback the object used for notifying the caller when the asynchronous call is completed.
     */
    void setPriorityByName(String sessionId, List<Integer> list, String priorityName, AsyncCallback<Void> callback);

    /**
     * Pauses the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @param callback the object used for notifying the caller when the asynchronous call is completed.
     */
    void pauseScheduler(String sessionId, AsyncCallback<Boolean> callback);

    /**
     * Resumes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @param callback the object used for notifying the caller when the asynchronous call is completed. 
     */
    void resumeScheduler(String sessionId, AsyncCallback<Boolean> callback);

    /**
     * Freezes the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @param callback the object used for notifying the caller when the asynchronous call is completed. 
     */
    void freezeScheduler(String sessionId, AsyncCallback<Boolean> callback);

    /**
     * Kills the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @param callback the object used for notifying the caller when the asynchronous call is completed. 
     */
    void killScheduler(String sessionId, AsyncCallback<Boolean> callback);

    /**
     * Starts the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @param callback the object used for notifying the caller when the asynchronous call is completed. 
     */
    void startScheduler(String sessionId, AsyncCallback<Boolean> callback);

    /**
     * Stops the Scheduler.
     * @param sessionId the session id of the user which is logged in
     * @param callback the object used for notifying the caller when the asynchronous call is completed. 
     */
    void stopScheduler(String sessionId, AsyncCallback<Boolean> callback);

    /**
     * Gets the output of a task
     * @param sessionId the session id of the user which is logged in
     * @param jobId the id of the job
     * @param taskName the name of the task, not the ID
     * @param logMode one of {@link #LOG_ALL}, {@link #LOG_STDOUT}, {@link #LOG_STDERR}
     * @param callback async callback for the client, containing the task output or the error message
     */
    Request getTaskOutput(String sessionId, String jobId, String taskName, OutputMode logMode,
            AsyncCallback<String> callback);

    /**
     * Gets the output of a job even for tasks that have not terminated yet
     * @param sessionId current session id
     * @param jobId id of the job for which logs should be fetched
     * @param callback async callback for the client, containing the job output or the error message
     */
    Request getLiveLogJob(String sessionId, String jobId, AsyncCallback<String> callback);

    /**
     * Gets the number of bytes available in the job output stream for the given job id,
     * might be used to determine if fetch is necessary
     * @param sessionId current session id
     * @param jobId id of the job for which logs should be fetched
     * @param callback async callback for the client, containing the number of bytes
     */
    Request getLiveLogJobAvailable(final String sessionId, final String jobId, AsyncCallback<Integer> callback);

    /**
     * Clean the remote live log object
     * @param sessionId current session id
     * @param jobId id of the job for which live logs should be cleaned
     */
    Request deleteLiveLogJob(final String sessionId, final String jobId, AsyncCallback<Boolean> callback);

    void getStatistics(String sessionId, AsyncCallback<String> callBack);

    /**
     * returns a string containing some data regarding the user's account
     * @param sessionId the session id associated to this new connection
     * @return a string containing some data regarding the user's account
     */
    public void getStatisticsOnMyAccount(String sessionId, AsyncCallback<String> callBack);

    /**
     * A set of jobs representing the current scheduler state
     * @param sessionId id of the current session
     * @param startCursor start cursor
     * @param endCursor end cursor
     * @param myJobs true to fetch only the jobs of the user making the request
     * @param pending fetch pending jobs
     * @param running fetch running jobs
     * @param finished fetch finished jobs 
     * @param callback
     */
    void revisionAndjobsinfo(String sessionId, String startCursor, String endCursor, int pageSize, boolean first,
            String user, boolean pending, boolean running, boolean finished, FilterModel filterModel,
            AsyncCallback<String> callback);

    void schedulerStateRevision(String sessionId, AsyncCallback<Long> callback);

    /**
     * Image representing the job as designed in the WF studio when applicable
     * @param sessionId current session
     * @param jobId id of the job
     * @param callback the callback for to handle the returned PNG image path on the server
     */
    void getJobImage(String sessionId, String jobId, AsyncCallback<String> callback);

    /**
     * Returns the Scheduler status as a JSON String 
     * @param sessionId a valid session id
     * @param callback the callback for returning the current scheduler status
     */
    void getSchedulerStatus(String sessionId, AsyncCallback<String> callback);

    /**
     * Returns the list of users currently connected to the scheduler as a json array 
     * @param sessionId current session id
     * @param callback
     */
    void getSchedulerUsers(String sessionId, AsyncCallback<String> callback);

    /**
     * Returns the list of users having jobs the scheduler as a json array 
     * @param sessionId current session id
     * @param callback
     */
    void getSchedulerUsersWithJobs(String sessionId, AsyncCallback<String> callback);

    /**
     * returns the version string of the REST api
     */
    void getVersion(AsyncCallback<String> callback);

    /**
     * Get server logs for a given task
     * 
     * @param sessionId current session
     * @param jobId id of a job
     * @param taskName name of a task to restart within that job
     * @return server task logs
     * @throws RestServerException
     * @throws ServiceException
     */
    Request getTaskServerLogs(String sessionId, Integer jobId, String taskName, AsyncCallback<String> callback);

    /**
     * Get server logs for a given job
     * 
     * @param sessionId current session
     * @param jobId id of a job
     * @return server job logs
     * @throws RestServerException
     * @throws ServiceException
     */
    Request getJobServerLogs(String sessionId, Integer jobId, AsyncCallback<String> callback);

    void getUsage(String sessionId, String user, Date startDate, Date endDate,
            AsyncCallback<List<JobUsage>> asyncCallback);

    void getJobHtml(String sessionId, String jobId, AsyncCallback<String> asyncCallback);

    void putThirdPartyCredential(String sessionId, String key, String value, AsyncCallback<Void> async);

    void thirdPartyCredentialKeySet(String sessionId, AsyncCallback<Set<String>> asyncCallback);

    void removeThirdPartyCredential(String sessionId, String key, AsyncCallback<Void> asyncCallback);

}
