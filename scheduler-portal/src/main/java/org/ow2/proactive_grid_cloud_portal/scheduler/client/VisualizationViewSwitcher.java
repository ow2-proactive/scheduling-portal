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

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.VisualizationListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;

import com.google.gwt.event.dom.client.LoadEvent;
import com.google.gwt.event.dom.client.LoadHandler;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.Layout;


/**
 * Displays an image of the currently selected job if available 
 * 
 * 
 * @author mschnoor
 *
 */
public class VisualizationViewSwitcher
        implements JobSelectedListener, VisualizationListener, LoadHandler, TasksUpdatedListener {

    private VisualizationViewHtml htmlView;

    public VisualizationViewHtml getHtmlView() {
        return htmlView;
    }

    private VisualizationViewImage imageView;

    private VisualizationView activeVisualization;

    private Layout layout;

    public VisualizationViewSwitcher(SchedulerController controller) {
        controller.getEventDispatcher().addVisualizationListener(this);
        JobsModel jobsModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel().getJobsModel();
        jobsModel.addJobSelectedListener(this);
        ((SchedulerModelImpl) controller.getModel()).getTasksModel().addTasksUpdatedListener(this);

        this.htmlView = new VisualizationViewHtml();
        this.imageView = new VisualizationViewImage(controller);

        this.activeVisualization = this.htmlView;
    }

    @Override
    public void jobSelected(Job job) {
        this.activeVisualization.jobSelected(job);
    }

    @Override
    public void jobUnselected() {
        this.activeVisualization.jobUnselected();
    }

    @Override
    public void onLoad(LoadEvent event) {
        this.activeVisualization.onLoad(event);
    }

    @Override
    public void tasksUpdating() {
        this.activeVisualization.tasksUpdating();
    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.activeVisualization.tasksUpdated(tasks, totalTasks);
    }

    @Override
    public void tasksUpdatedFailure(String message) {
        this.activeVisualization.tasksUpdatedFailure(message);
    }

    @Override
    public void htmlUpdated(String jobId, String path) {
        layout.hideMember(layout.getMembers()[1]);
        layout.showMember(layout.getMembers()[0]);
        this.activeVisualization = this.htmlView;

        this.imageView.htmlUpdated(jobId, path);
        this.htmlView.htmlUpdated(jobId, path);
    }

    @Override
    public void imageUpdated(String jobId, String path) {
        layout.hideMember(layout.getMembers()[0]);
        layout.showMember(layout.getMembers()[1]);
        this.activeVisualization = this.imageView;

        this.imageView.imageUpdated(jobId, path);
        this.htmlView.imageUpdated(jobId, path);
    }

    @Override
    public void mapUpdated(String jobId, JobVisuMap map) {
        layout.hideMember(layout.getMembers()[0]);
        layout.showMember(layout.getMembers()[1]);
        this.activeVisualization = this.imageView;

        this.imageView.mapUpdated(jobId, map);
        this.htmlView.mapUpdated(jobId, map);
    }

    @Override
    public void visualizationUnavailable(String jobId) {
        this.imageView.visualizationUnavailable(jobId);
        this.htmlView.visualizationUnavailable(jobId);
    }

    public Canvas build() {
        layout = new Layout();
        layout.setWidth100();
        layout.setHeight100();

        this.htmlView.setRoot(layout);
        this.imageView.setRoot(layout);
        layout.hideMember(layout.getMembers()[1]);

        return layout;
    }

    @Override
    public void selectedJobUpdated(Job job) {
    }

}
