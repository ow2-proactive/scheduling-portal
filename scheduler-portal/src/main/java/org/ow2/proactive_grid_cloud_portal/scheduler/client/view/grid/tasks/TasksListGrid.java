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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks;

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TaskRecord.getTask;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TasksColumnsFactory.COLUMNS_TO_ALIGN;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TasksColumnsFactory.EXEC_DURATION_ATTR;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TasksColumnsFactory.NAME_ATTR;
import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TasksColumnsFactory.STATUS_ATTR;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.RemoteHintListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel.RemoteHint;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ItemsListGrid;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.RecordList;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.DrawEvent;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.SortNormalizer;
import com.smartgwt.client.widgets.grid.events.FieldStateChangedEvent;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.grid.events.SelectionUpdatedEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;


/**
 * A grid that shows tasks
 *
 * @author The actieveeon team.
 */
public class TasksListGrid extends ItemsListGrid<Task> implements TasksUpdatedListener, RemoteHintListener {

    /**
     * The controller for this grid.
     */
    protected TasksController controller;

    /**
     * The buttons to be shown when remote visu is available for a task.
     */
    private HashMap<String, ImgButton> visuButtons = null;

    /**
     * To avoid opening several popup on button's click
     */
    private Map<ImgButton, HandlerRegistration> visuButtonsClickHandlers;

    protected final boolean usedWithTaskCentricView;

    public TasksListGrid(TasksController controller, TasksColumnsFactory factory, String datasourceNamePrefix,
            boolean usedWithTaskCentricView) {
        super(factory, datasourceNamePrefix);
        this.usedWithTaskCentricView = usedWithTaskCentricView;
        this.controller = controller;
        this.visuButtons = new HashMap<String, ImgButton>();
        this.visuButtonsClickHandlers = new HashMap<ImgButton, HandlerRegistration>();
        this.controller.getModel().addTasksUpdatedListener(this);
        this.controller.getModel().addRemoteHintListener(this);
        this.emptyMessage = "No tasks to show.";
    }

    @Override
    public void build() {
        super.build();
        this.setSelectionType(SelectionStyle.SINGLE);
        this.setSelectionProperty("isSelected");
        this.sort(TasksCentricColumnsFactory.JOB_ID_ATTR.getName(), SortDirection.DESCENDING);
        //These settings are needed to show the remote visu, but in the task-centric view, they break task centric scrolling
        if (!usedWithTaskCentricView) {
            this.setShowRecordComponents(true);
            this.setShowRecordComponentsByCell(true);
        }
    }

    protected Map<GridColumns, ListGridField> buildListGridField() {
        Map<GridColumns, ListGridField> fields = super.buildListGridField();

        ListGridField idField = fields.get(TasksColumnsFactory.ID_ATTR);
        idField.setType(ListGridFieldType.INTEGER);

        alignCells(fields);

        ListGridField execDuration = fields.get(EXEC_DURATION_ATTR);
        execDuration.setCellFormatter(new CellFormatter() {
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                if (value != null) {
                    return Job.formatDuration(value.toString());
                } else {
                    return "";
                }
            }
        });

        ListGridField startTime = fields.get(TasksColumnsFactory.START_TIME_ATTR);
        startTime.setSortNormalizer(customDateSorting("startTime"));

        ListGridField finishTime = fields.get(TasksColumnsFactory.FINISHED_TIME_ATTR);
        finishTime.setSortNormalizer(customDateSorting("finishTime"));

        return fields;
    }

    private void alignCells(Map<GridColumns, ListGridField> fields) {
        GridColumns[] columnsToAlign = COLUMNS_TO_ALIGN;

        if (usedWithTaskCentricView) {
            columnsToAlign = TasksCentricColumnsFactory.COLUMNS_TO_ALIGN;
        }

        for (GridColumns column : columnsToAlign) {
            ListGridField listGridField = fields.get(column);
            listGridField.setAlign(Alignment.CENTER);
            listGridField.setCellAlign(Alignment.CENTER);
        }
    }

    @Override
    public void remoteHintRead(RemoteHint hint) {
        for (ListGridRecord rec : this.getRecords()) {
            loadRemoteHint(hint, rec);
        }
    }

    protected TaskRecord updateTaskRecord(Task task) {
        return new TaskRecord(task);
    }

    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.visuButtons.clear();
        Task selectedTask = this.controller.getModel().getSelectedTask();

        this.ds.invalidateCache();
        RecordList data = new RecordList();
        for (Task t : tasks) {
            TaskRecord record = new TaskRecord(t);
            this.columnsFactory.buildRecord(t, record);
            data.add(record);

            if (t.equals(selectedTask)) {
                record.setAttribute("isSelected", true);
            }
        }

        this.ds.setCacheData(data.toArray());
        data.destroy();
        applyCurrentLocalFilter();
    }

    @Override
    public void tasksUpdating() {
        //Nothing to do
    }

    @Override
    public void tasksUpdatedFailure(String message) {
        //Nothing to do
    }

    @Override
    protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
        String base = super.getCellCSSText(record, rowNum, colNum);

        String fieldName = this.getFieldName(colNum);

        if (fieldName.equals(STATUS_ATTR.getName())) {
            String st = record.getAttribute(STATUS_ATTR.getName());
            if (st.equals(TaskStatus.PENDING.toString()) || st.equals(TaskStatus.SUBMITTED.toString())) {
                return "color:#1a8bba;" + base;
            } else if (st.equals(TaskStatus.RUNNING.toString())) {
                return "color:#176925;font-weight:bold;" + base;
            } else if (st.equals(TaskStatus.ABORTED.toString()) || st.equals(TaskStatus.FAILED.toString())) {
                return "color:#d37a11;font-weight:bold;" + base;
            } else if (st.equals(TaskStatus.FAULTY.toString()) || st.equals(TaskStatus.NOT_STARTED.toString()) ||
                       st.equals(TaskStatus.NOT_RESTARTED.toString()) || st.equals(TaskStatus.IN_ERROR.toString())) {
                return "color:#c50000;font-weight:bold;" + base;
            } else {
                return base;
            }
        }

        return base;
    }

    @Override
    protected Canvas createRecordComponent(final ListGridRecord record, Integer colNum) {
        String fieldName = this.getFieldName(colNum);
        if ("visu".equals(fieldName)) {
            ImgButton button = new ImgButton();
            button.setSrc("transp.gif");
            button.setWidth(16);
            button.setHeight(16);
            button.setShowFocused(false);
            button.setShowHover(false);
            button.setShowRollOver(false);
            button.setShowOverCanvas(false);
            button.setShowDown(false);
            visuButtons.put(record.getAttributeAsString(TasksColumnsFactory.ID_ATTR.getName()), button);

            for (RemoteHint rh : controller.getModel().getRemoteHints()) {
                loadRemoteHint(rh, record);
            }

            return button;
        } else {
            return null;
        }
    }

    private void loadRemoteHint(final RemoteHint hint, final ListGridRecord rec) {
        String taskId = rec.getAttributeAsString(TasksColumnsFactory.ID_ATTR.getName());
        String jobId = this.controller.getModel()
                                      .getParentModel()
                                      .getExecutionsModel()
                                      .getJobsModel()
                                      .getSelectedJob()
                                      .getId()
                                      .toString();
        final String taskName = rec.getAttributeAsString(NAME_ATTR.getName());
        if (taskId.equals(hint.taskId) && jobId.equals(hint.jobId)) {
            ImgButton button = visuButtons.get(taskId);
            button.setSrc(SchedulerImages.instance.visu_16().getSafeUri().asString());
            if (visuButtonsClickHandlers.containsKey(button)) {
                visuButtonsClickHandlers.get(button).removeHandler();
            }
            HandlerRegistration clickHandler = button.addClickHandler(new ClickHandler() {
                public void onClick(ClickEvent event) {
                    showRemoteVisuChoices(hint, taskName);
                }
            });
            visuButtonsClickHandlers.put(button, clickHandler);
        }
    }

    private void showRemoteVisuChoices(final RemoteHint hint, final String taskName) {
        final Window window = new Window();

        window.setTitle("Show Remote Visualization");
        window.setShowMinimizeButton(false);
        window.setIsModal(true);
        window.setShowModalMask(true);
        window.setAutoSize(true);
        window.setAutoCenter(true);

        final Label label = new Label("Remote visualization can be performed using your web browser " +
                                      "if it supports Websocket and Canvas or using VNC client application.");
        label.setWidth(300);

        final IButton cancelButton = new IButton("Cancel");
        cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                window.destroy();
            }
        });

        final IButton noVncButton = new IButton("Use web browser");
        noVncButton.setIcon(SchedulerImages.instance.visu_16().getSafeUri().asString());
        noVncButton.setAutoFit(true);

        if (!(isWebSocketSupported() && isCanvasSupported())) {
            noVncButton.disable();
            noVncButton.setTooltip("Not supported");
        }
        noVncButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                openNoVncPage(taskName);
                window.destroy();
            }
        });

        final IButton appletButton = new IButton("Use VNC client");
        appletButton.setAutoFit(true);
        appletButton.setIcon(SchedulerImages.instance.visu_16().getSafeUri().asString());
        appletButton.setTooltip("Run a Java applet to launch VNC client application on your desktop.");
        appletButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                openRemoteVisuServlet(hint);
                window.destroy();
            }
        });

        final HLayout buttonBar = new HLayout();
        buttonBar.setMembersMargin(5);
        buttonBar.setMembers(noVncButton, appletButton, cancelButton);

        VLayout layout = new VLayout();
        layout.setMembersMargin(10);
        layout.setMargin(5);
        layout.setMembers(label, buttonBar);

        window.addItem(layout);
        window.show();
    }

    private void openNoVncPage(String taskName) {
        String httpsRedirectUrl = this.controller.computeNoVncPageUrl(taskName);
        com.google.gwt.user.client.Window.open(httpsRedirectUrl, "_blank", "");
    }

    private void openRemoteVisuServlet(RemoteHint hint) {
        String url = GWT.getModuleBaseURL() + "visu?";
        url += "&codebase=" + GWT.getHostPageBaseURL();
        url += "&apptype=" + hint.type;
        url += "&argument=" + hint.argument;
        com.google.gwt.user.client.Window.open(url, "_blank", "");
    }

    private native boolean isWebSocketSupported() /*-{
                                                  return "WebSocket" in $wnd;
                                                  }-*/;

    private native boolean isCanvasSupported() /*-{
                                               var elem = document.createElement('canvas');
                                               return !!(elem.getContext && elem.getContext('2d'));
                                               }-*/;

    /**
     * A custom sort for task start, and finish times:
     * dates are sorted according to the task epoch time and not
     * according to the String representation of the date
     * given by JSUtil.getTime(long Time)
     */
    private SortNormalizer customDateSorting(String date) {
        return (record, fieldName) -> {
            Task task = getTask(record);
            if (date.equals("startTime")) {
                return task.getStartTime();
            }
            return task.getFinishTime();

        };
    }

    @Override
    protected void buildCellContextualMenu(Menu menu) {
        final String taskName = this.getSelectedRecord().getAttributeAsString(NAME_ATTR.getName());
        final String taskStatusName = this.getSelectedRecord().getAttributeAsString(STATUS_ATTR.getName());
        final Integer jobId = (int) getTask(this.getSelectedRecord()).getJobId();

        MenuItem restartInErrorTask = new MenuItem("Restart In-Error Task");
        restartInErrorTask.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.restartInErrorTask(taskName, jobId);
            }
        });

        MenuItem restartRunningTask = new MenuItem("Restart Running Task");
        restartRunningTask.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.restartRunningTask(taskName, jobId);
            }
        });

        MenuItem preempt = new MenuItem("Preempt");
        preempt.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.preemptTask(taskName, jobId);
            }
        });
        MenuItem kill = new MenuItem("Kill");
        kill.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.killTask(taskName, jobId);
            }
        });
        MenuItem markAsFinishedAndResume = new MenuItem("Mark as finished and Resume");
        markAsFinishedAndResume.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.markAsFinishedAndResume(taskName, jobId);
            }
        });

        TaskStatus status = TaskStatus.from(taskStatusName);

        boolean enableKill;
        boolean enablePreempt;
        boolean enableRestartRunningTask;
        boolean enableRestartInErrorTask;
        boolean enableMarkAsFinishedAndResume;

        switch (status) {
            case FAILED:
            case ABORTED:
            case FAULTY:
            case FINISHED:
            case NOT_RESTARTED:
            case NOT_STARTED:
            case PAUSED:
            case PENDING:
            case SKIPPED:
            case SUBMITTED:
            case WAITING_ON_ERROR:
            case WAITING_ON_FAILURE:
                enableKill = false;
                enablePreempt = false;
                enableRestartInErrorTask = false;
                enableRestartRunningTask = false;
                enableMarkAsFinishedAndResume = false;
                break;
            case RUNNING:
                enableKill = true;
                enablePreempt = true;
                enableRestartInErrorTask = false;
                enableRestartRunningTask = true;
                enableMarkAsFinishedAndResume = false;
                break;
            case IN_ERROR:
                enableKill = false;
                enablePreempt = false;
                enableRestartInErrorTask = true;
                enableRestartRunningTask = false;
                enableMarkAsFinishedAndResume = true;
                break;
            default:
                enableKill = false;
                enablePreempt = false;
                enableRestartInErrorTask = false;
                enableRestartRunningTask = false;
                enableMarkAsFinishedAndResume = false;
        }

        kill.setEnabled(enableKill);
        preempt.setEnabled(enablePreempt);
        restartInErrorTask.setEnabled(enableRestartInErrorTask);
        restartRunningTask.setEnabled(enableRestartRunningTask);
        markAsFinishedAndResume.setEnabled(enableMarkAsFinishedAndResume);

        menu.setItems(restartInErrorTask, restartRunningTask, preempt, kill, markAsFinishedAndResume);
    }

    @Override
    protected void selectionChangedHandler(SelectionEvent event) {
        if (event.getState() && !fetchingData) {
            ListGridRecord record = event.getRecord();
            Task task = TaskRecord.getTask(record);
            controller.selectTask(task);
        }
    }

    @Override
    protected void selectionUpdatedHandler(SelectionUpdatedEvent event) {
        // Nothing to do
    }

    @Override
    protected void fieldStateChangedHandler(FieldStateChangedEvent event) {
        // Nothing to do
    }

    @Override
    protected void drawHandler(DrawEvent event) {
        // Nothing to do
    }

}
