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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksPaginationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksCentricView;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.widgets.layout.Layout;


public class TasksCentricController extends TasksController {

    private boolean isHeaderClickHandler = false;

    private TasksCentricSortChangedHandler tasksCentricSortChangedHandler;

    public TasksCentricController(SchedulerController parentController) {
        super(parentController);
        tasksCentricSortChangedHandler = new TasksCentricSortChangedHandler(this);
    }

    public TasksCentricView getView() {
        return (TasksCentricView) this.view;
    }

    @Override
    public Layout buildView() {
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) this.parentController.getModel();
        this.model = new TasksCentricModel(schedulerModel);
        schedulerModel.getExecutionsModel().setTasksModel((TasksCentricModel) this.model);
        this.taskNavigationController = new TasksCentricNavigationController(this);
        this.view = new TasksCentricView(this);
        return this.view.build();
    }

    public void tasksStateRevision(boolean forceRefresh) {
        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model.getTasksNavigationModel();
        if (navigationModel.getTaskAutoRefreshOption() || forceRefresh) {
            this.updateTasks(false);
            if (!isHeaderClickHandler) {
                this.view.addSortChangedHandler(tasksCentricSortChangedHandler);
                isHeaderClickHandler = true;
            }
        }
    }

    /**
     * Updates the current task list depending the current job selection in the model 
     */
    @Override
    public void updateTasks(boolean showUpdating) {
        if (showUpdating) {
            this.model.notifyTasksChanging(false);
        }

        AsyncCallback<String> callback = new TasksAsyncUpdater(model, parentController);

        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) this.model.getTasksNavigationModel();
        String tagFilter = navigationModel.getCurrentTagFilter();
        long fromDate = navigationModel.getFromDate();
        long toDate = navigationModel.getToDate();

        TasksPaginationModel paginationModel = navigationModel.getPaginationModel();
        int offset = paginationModel.getOffset();
        int limit = paginationModel.getPageSize();
        String sessionId = LoginModel.getInstance().getSessionId();
        SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();

        ExecutionsModel executionsModel = this.model.getParentModel().getExecutionsModel();
        boolean myTasksOnly = executionsModel.isFetchMyExecutionsOnly();

        if (tagFilter.isEmpty()) {
            this.taskUpdateRequest = scheduler.getTaskCentric(sessionId,
                                                              fromDate,
                                                              toDate,
                                                              myTasksOnly,
                                                              this.model.getTasksNavigationModel().getStatusFilter(),
                                                              offset,
                                                              limit,
                                                              getSortParameters(),
                                                              callback);
        } else {
            this.taskUpdateRequest = scheduler.getTaskCentricByTag(sessionId,
                                                                   tagFilter,
                                                                   fromDate,
                                                                   toDate,
                                                                   myTasksOnly,
                                                                   this.model.getTasksNavigationModel()
                                                                             .getStatusFilter(),
                                                                   offset,
                                                                   limit,
                                                                   getSortParameters(),
                                                                   callback);
        }
    }

    private SortSpecifierRestContainer getSortParameters() {
        SortSpecifierRestContainer sortParameters = null;
        SortSpecifier[] sorts = this.view.getSort();
        if (sorts != null && sorts.length > 0) {
            sortParameters = new SortSpecifierRestContainer(sorts.length);
            for (SortSpecifier sortSpecifier : sorts) {
                sortParameters.add(sortSpecifier.getField(), sortSpecifier.getSortDirection().getValue());
            }
        }
        return sortParameters;
    }

    public TasksPaginationController getPaginationController() {
        return this.taskNavigationController.getPaginationController();
    }

    /**
     * Select another task.
     *
     * @param task of the new task selection. you can use null to cancel the current selection
     */
    @Override
    public void selectTask(final Task task) {
        super.selectTask(task);
        if (task != null) {
            final String jobId = Long.toString(task.getJobId());
            String sessionId = LoginModel.getInstance().getSessionId();
            SchedulerServiceAsync scheduler = Scheduler.getSchedulerService();
            AsyncCallback<String> callback = new TasksCentricAsyncSelector((TasksCentricModel) model, jobId);
            scheduler.getJobInfoDetails(sessionId, jobId, callback);
        } else {
            ((TasksCentricModel) model).setTaskSelectedJob(null);
        }
    }

    public static class SortSpecifierRestContainer implements Serializable {

        private List<SortSpecifierRestItem> sortParameters;

        public SortSpecifierRestContainer() {
            sortParameters = new ArrayList<>();
        }

        public static class SortSpecifierRestItem implements Serializable {

            protected String field;

            protected String order;

            SortSpecifierRestItem(String field, String order) {
                this.field = field;
                this.order = order;
            }

            public SortSpecifierRestItem() {
                this.field = "NOTSET";
                this.order = "ascending";
            }

            public String toString() {
                return field + "," + order;
            }
        }

        SortSpecifierRestContainer(int size) {
            sortParameters = new ArrayList<>(size);
        }

        SortSpecifierRestContainer(String values) {
            sortParameters = new ArrayList<>();
            for (String term : values.split(";")) {
                String[] sortParam = term.split(",");
                add(sortParam[0], sortParam[1]);
            }
        }

        public void add(String field, String order) {
            sortParameters.add(new SortSpecifierRestItem(field, order));
        }

        public String toString() {
            StringBuilder sb = new StringBuilder();
            int paddedSize = sortParameters.size() - 1;
            for (int i = 0; i < sortParameters.size(); i++) {
                sb.append(sortParameters.get(i).toString());
                if (i < paddedSize) {
                    sb.append(";");
                }
            }
            return sb.toString();
        }

    }

}
