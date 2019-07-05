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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel.RemoteHint;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;


/**
 * Contains all the interfaces for event dispatch
 * <p>
 * All in one class to limit the number of files
 *
 * @author mschnoor
 *
 */
public class SchedulerListeners {

    public interface SchedulerStatusListener {

        /**
         * Called when the scheduler status changed
         * 
         * @param status the new status
         */
        public void statusChanged(SchedulerStatus status);

    }

    public interface TaskResultListener {
        void preciousTaskNamesLoaded(List<String> preciousTaskNames);

        void preciousTaskNamesNotAuthorized();
    }

    public interface JobSelectedListener {

        /**
         * Called when a Job is selected in the main Scheduler View Grid
         *
         * @param job the selected job
         */
        public void jobSelected(Job job);

        /**
         * Called when the Job selection in the main Scheduler View Grid is canceled
         */
        public void jobUnselected();

        /**
         * Called when the selected job has been updated because of jobs revision.
         */
        public void selectedJobUpdated(Job job);

    }

    public interface JobsUpdatedListener {

        /**
         * Called when the Model receives a new JobSet revision
         *
         * @param jobs the last JobSet received by the model
         */
        public void jobsUpdated(Map<Integer, Job> jobs);

        /**
         * The job list has changed, and the new version is currently being fetched
         * <p>
         * Views displaying the jobs list should switch to an undeterminate 'loading' state
         * and wait for the next call to {@link #jobsUpdated(JobSet)} to actually display jobs
         */
        public void jobsUpdating();

        /**
         * A new job has been submitted by this client
         * no info related to this job has been fetched from the server yet,
         * still we need to display something to notify the user that the job was submitted
         * @param j a fake job displaying some known characteristics of the submitted job
         */
        public void jobSubmitted(Job j);

    }

    public interface ExecutionDisplayModeListener {
        public void modeSwitched(ExecutionListMode mode);
    }

    public interface TaskSelectedListener {
        /**
         * Called when a task is selected in the main Scheduler View Grid
         *
         * @param job the selected job
         */
        public void taskSelected(Task task);

        /**
         * Called when the task selection in the main Scheduler View Grid is canceled
         */
        public void taskUnselected();

    }

    public interface TasksUpdatedListener {

        /**
         * The tasks list has changed, and the new one is currently being fetched.
         * <p>
         * {@link #tasksUpdated(TaskSet)} will be called when the new tasks are available,
         * but in the meantime clients should not rely on the model's task list as it may
         * not be coherent with the other views
         * 
         *  @param jobChanged false when the tasks in the next {@link #tasksUpdated(TaskSet)} event
         *  	are part of the same job as the tasks in the last {@link #tasksUpdated(TaskSet)}
         */
        public void tasksUpdating();

        /**
         * The tasks list has been updated and new values are available
         * 
         * @param tasks the latest TaskSet received by the model
         */
        public void tasksUpdated(List<Task> tasks, long totalTasks);

        /**
         * Task update was requested but failed
         * <p>
         * Failure to update the tasks list is not critical and can happen during normal operation,
         * i.e. when user has insufficient rights
         * 
         * @param message error message explaining failure to the user
         */
        public void tasksUpdatedFailure(String message);

    }

    public interface JobOutputListener {

        /**
         * The output of a job has been updated, views that are currently
         * displaying the output of this job or waiting for it can use it directly
         * 
         * @param output the output of a job
         */
        public void jobOutputUpdated(JobOutput output, SelectionTarget target);

        public void liveToggled(boolean newValue);

        public void liveEnabled(boolean newValue);

    }

    public interface UsersListener {

        /**
         * The list of users connected to the scheduler has been updated
         * 
         * @param users users currently connected to the scheduler
         */
        public void usersUpdated(List<SchedulerUser> users);

    }

    public interface StatisticsListener {

        /**
         * Stats regarding the whole scheduler have been updated
         * 
         * @param stats new stats
         */
        public void schedulerStatsUpdated(HashMap<String, String> stats);

        /**
         * Stats regarging the current user have been updated
         * 
         * @param stats new stats
         */
        public void accountStatsUpdated(HashMap<String, String> stats);

    }

    public interface RemoteHintListener {

        /**
         * a log entry containing PA_REMOTE_CONNECTION was read
         * 
         * @param hint the log entry, must be parsed
         */
        public void remoteHintRead(RemoteHint hint);
    }

    public interface VisualizationListener {

        /**
         * The html used for job visualization has been updated (web studio)
         *
         * @param jobId id of the job
         * @param path Relative path of the image on the server
         */
        public void htmlUpdated(String jobId, String path);

        /**
         * The image used for job visualization has been updated
         * 
         * @param jobId id of the job
         * @param path Relative path of the image on the server
         */
        public void imageUpdated(String jobId, String path);

        /**
         * The coordinate map for job visu has been updated
         * 
         * @param jobId id of the job
         * @param map position and size of tasks on the image sent through {@link #imageUpdated(String, String)}
         */
        public void mapUpdated(String jobId, JobVisuMap map);

        /**
         * No visu available for this job
         * 
         * @param jobId id of the job
         */
        public void visualizationUnavailable(String jobId);

    }

    public interface UsageListener {

        public void usageUpdated(List<JobUsage> usage);

    }

    public interface ThirdPartyCredentialsListener {
        void keysUpdated(Set<String> thirdPartyCredentialsKeys);
    }

    /**
     * Listener for tag suggestions events.
     * @author the activeeon team
     *
     */
    public interface TagSuggestionListener {

        /**
         * When the list of tag suggestion has been updated.
         */
        void tagSuggestionListUpdated();
    }

    /**
     * Listener for pagination events.
     * @author the activeeon team.
     *
     */
    public interface PaginationListener {

        /**
         * when the displayed page changed.
         */
        void pageChanged();

        /**
         * when the total number of paginated items changed.
         */
        void totalItemChanged();
    }

    public interface ServerLogsListener {

        public void logsUpdated(String logs, String jobId);
    }

}
