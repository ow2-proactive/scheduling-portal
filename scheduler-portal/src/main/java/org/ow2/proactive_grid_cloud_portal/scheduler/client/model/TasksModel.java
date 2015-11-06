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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.RemoteHintListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TaskSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;

public class TasksModel {

    public static final String PA_REMOTE_CONNECTION = "PA_REMOTE_CONNECTION";

    protected List<Task> tasks = null;
    protected boolean tasksDirty = false;
    protected List<RemoteHint> remoteHints = null;

    protected Task selectedTask;

    protected ArrayList<TaskSelectedListener> tasksSelectedListeners = null;
    protected ArrayList<TasksUpdatedListener> tasksUpdatedListeners = null;
    protected ArrayList<RemoteHintListener> remoteHintListeners = null;


    protected SchedulerModelImpl parentModel;

    protected TasksNavigationModel tasksNavigationModel;

    public static class RemoteHint {
        public String taskId;
        public String jobId;
        public String type;
        public String argument;
    }

    public TasksModel(SchedulerModelImpl parentModel) {
        this.parentModel = parentModel;
        this.parentModel.setTasksModel(this);

        this.initNavigationModel();

        this.remoteHints = new ArrayList<RemoteHint>();
        this.tasksUpdatedListeners = new ArrayList<TasksUpdatedListener>();
        this.remoteHintListeners = new ArrayList<RemoteHintListener>();
        this.tasksSelectedListeners = new ArrayList<TaskSelectedListener>();
    }


    protected void initNavigationModel(){
        this.tasksNavigationModel = new TasksNavigationModel(this);
    }



    public SchedulerModelImpl getParentModel() {
        return parentModel;
    }


    public void notifyTasksChanging(boolean emptyTaskList){
        for (TasksUpdatedListener list : this.tasksUpdatedListeners) {
            if (emptyTaskList)
                list.tasksUpdated(new ArrayList<Task>(), 0);
            else
                list.tasksUpdating();
        }
    }


    /**
     * Modifies the tasks set
     * triggers TasksUpdated event
     * 
     * @param tasks the new TaskSet
     */
    public void setTasks(List<Task> tasks, long totalTasks) {
        this.tasks = tasks;
        this.tasksNavigationModel.getPaginationModel().setTotalItems(totalTasks);
        for (TasksUpdatedListener list : this.tasksUpdatedListeners) {
            list.tasksUpdated(tasks, totalTasks);
        }
    }


    /**
     * Notify task updated listeners that updating failed
     * 
     * @param message the error message
     */
    public void taskUpdateError(String message) {
        this.tasks = new ArrayList<Task>();
        for (TasksUpdatedListener list : this.tasksUpdatedListeners) {
            list.tasksUpdatedFailure(message);
        }
    }

    /**
     * @return the list of tasks corresponding the currently selected job
     */
    public List<Task> getTasks() {
        return this.tasks;
    }

    /**
     * @return true if the current tasks list does not match the selected job
     */
    public boolean isTasksDirty() {
        return this.tasksDirty;
    }

    public void setTasksDirty(boolean b) {
        this.tasksDirty = b;
    }


    public void selectTask(Task task){
        this.selectedTask = task;
        for(TaskSelectedListener listener: this.tasksSelectedListeners){
            if(task == null){
                listener.taskUnselected();
            }
            else{
                listener.taskSelected(task);
            }
        }
    }


    public Task getSelectedTask() {
        return selectedTask;
    }


    /**
     * Add a remote hint
     * will notify listeners if it is well formed
     * 
     * @param remoteHint a string containing PA_REMOTE_CONNECTION
     */
    public void addRemoteHint(String remoteHint) {
        String[] expl = remoteHint.split(PA_REMOTE_CONNECTION);
        if (expl.length < 2)
            return;

        expl = expl[1].split(";");
        if (expl.length < 5)
            return;

        RemoteHint rh = new RemoteHint();
        rh.jobId = expl[1];
        rh.taskId = expl[2];
        rh.type = expl[3];
        rh.argument = expl[4];

        this.remoteHints.add(rh);

        for (RemoteHintListener rhl : this.remoteHintListeners) {
            rhl.remoteHintRead(rh);
        }
    }


    /**
     * Return all the remote hints that have been read so far by the model
     * this corresponds to all log lines containing 'PA_REMOTE_CONNECTION' that
     * were fetched in logs
     * If logs for the task were not fetched, remote hints won't be stored here
     * 
     * @return all remote hints read so far
     */
    public List<RemoteHint> getRemoteHints() {
        return this.remoteHints;
    }



    public void addTasksUpdatedListener(TasksUpdatedListener listener) {
        this.tasksUpdatedListeners.add(listener);
    }


    public void addRemoteHintListener(RemoteHintListener listener) {
        this.remoteHintListeners.add(listener);
    }


    public void addTaskSelectedListener(TaskSelectedListener listener){
        this.tasksSelectedListeners.add(listener);
    }

    public TasksNavigationModel getTasksNavigationModel() {
        return tasksNavigationModel;
    }

    public void setTasksNavigationModel(TasksNavigationModel tasksNavigationModel) {
        this.tasksNavigationModel = tasksNavigationModel;
    }
}
