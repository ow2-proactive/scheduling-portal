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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.JSUtil;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.RemoteHintListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.TaskStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksModel.RemoteHint;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.TasksColumns;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerRegistration;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.DataSourceField;
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
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


/**
 * Contains the ListGrid that displays tasks
 *
 * @author mschnoor
 */
public class TasksView extends AbstractGridItemsView implements TasksUpdatedListener, RemoteHintListener {


    /**
     * Entries in the Tasks Grid
     */
    private class TaskRecord extends ListGridRecord {

        private Task task = null;

        public TaskRecord(Task t) {
            setAttribute(TasksColumns.ID_ATTR.getName(), t.getId().longValue());
            this.task = t;
            update(t);
        }

        /**
         * Updates the attributes of this record to reflect the fields of the task <code>t</code>
         *
         * @param t a task
         */
        public void update(Task t) {
            String execs = (t.getMaxNumberOfExec() - t.getNumberOfExecLeft()) + " / " +
                    t.getMaxNumberOfExec();
            if (t.getNumberOfExecLeft() > 0)
                execs = (t.getMaxNumberOfExec() - t.getNumberOfExecLeft() + 1) + " / " +
                        t.getMaxNumberOfExec();

            String fails = (t.getMaxNumberOfExecOnFailure() - t.getNumberOfExecOnFailureLeft()) + " / " +
                    t.getMaxNumberOfExecOnFailure();

            setAttribute(TasksColumns.NAME_ATTR.getName(), t.getName());
            setAttribute(TasksColumns.TAG_ATTR.getName(), t.getTag());
            setAttribute(TasksColumns.STATUS_ATTR.getName(), t.getStatus().toString());
            setAttribute(TasksColumns.EXEC_DURATION_ATTR.getName(), t.getExecutionTime());
            setAttribute(TasksColumns.EXECUTIONS_ATTR.getName(), execs);
            setAttribute(TasksColumns.NODE_FAILURE_ATTR.getName(), fails);
            setAttribute(TasksColumns.NODE_COUNT_ATTR.getName(), t.getNodeCount());

            if (t.getStartTime() > 0)
                setAttribute(TasksColumns.START_TIME_ATTR.getName(), JSUtil.getTime(t.getStartTime()));
            if (t.getFinishTime() > t.getStartTime())
                setAttribute(TasksColumns.FINISHED_TIME_ATTR.getName(), JSUtil.getTime(t.getFinishTime()));
        }

        public Task getTask() {
            return this.task;
        }
    }

    /**
     * DataStore for the Tasks Grid
     */
    private class TaskDS extends DataSource {

        public TaskDS(String id) {
            setID(id);
            setClientOnly(true);
            DataSourceField [] fields = buildDatasourceFields();
            setFields(fields);
        }
    }

    /**
     * the Grid widget displayed in the view
     */
    private ListGrid tasksGrid = null;
    /**
     * shown when loading
     */
    private Label loadingLabel = null;
    /**
     * shown upon error
     */
    private Label errorLabel = null;
    /**
     * datasource : contains the actual data
     */
    private TaskDS ds = null;

    private HashMap<String, ImgButton> visuButtons = null;

    /** To avoid opening severial popup on button's click */
    private Map<ImgButton, HandlerRegistration> visuButtonsClickHandlers;
    
    
    protected TasksController controller;


    public TasksView(TasksController controller) {
        this.controller = controller;

        this.controller.getModel().addTasksUpdatedListener(this);
        this.controller.getModel().addRemoteHintListener(this);
        this.visuButtons = new HashMap<String, ImgButton>();
        visuButtonsClickHandlers = new HashMap<ImgButton, HandlerRegistration>();
    }


    public void tasksUpdating(boolean jobChanged) {
        if (jobChanged) {
            this.errorLabel.hide();
            this.tasksGrid.hide();
            this.loadingLabel.show();
            this.expandRecord = null;
        }
    }

    public void tasksUpdatedFailure(String message) {
        this.errorLabel.setContents(message);
        this.tasksGrid.hide();
        this.loadingLabel.hide();
        this.errorLabel.show();
    }

    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.visuButtons.clear();

        TaskRecord[] data = new TaskRecord[tasks.size()];
        int i = 0;
        for (Task t : tasks) {
            data[i] = new TaskRecord(t);
            String idAttr = TasksColumns.ID_ATTR.getName();
            if (this.expandRecord != null &&
                    data[i].getAttribute(idAttr).equals(this.expandRecord.getAttribute(idAttr))) {
                this.expandRecord = data[i];
            }
            i++;
        }

        this.tasksGrid.setData(data);
        this.tasksGrid.invalidateCache();
        //this.ds.setTestData(data);
        //this.tasksGrid.fetchData();

        this.errorLabel.hide();
        this.loadingLabel.hide();
        this.tasksGrid.show();
        if (this.expandRecord != null) {
            this.tasksGrid.expandRecord(this.expandRecord);
        }
        //this.controller.updateTaskPagination();
    }

    private ListGridRecord expandRecord;

    
    protected EnumMap<TasksColumns, ListGridField> getColumnsForListGridField(){
        EnumMap<TasksColumns, ListGridField> columns = new EnumMap<TasksColumns, ListGridField>(TasksColumns.class);
        for(TasksColumns col: TasksColumns.values()){
            if(!col.getDetail()){
                columns.put(col, null);
            }
        }
        return columns;
    }
    
    
    protected EnumMap<TasksColumns, ListGridField> buildListGridField(){
        EnumMap<TasksColumns, ListGridField> fields = super.<TasksColumns>buildListGridField();
        
        ListGridField idField = fields.get(TasksColumns.ID_ATTR);
        idField.setType(ListGridFieldType.INTEGER);
        idField.setAlign(Alignment.LEFT);
        idField.setCellAlign(Alignment.LEFT);

        ListGridField execDuration = fields.get(TasksColumns.EXEC_DURATION_ATTR);
        execDuration.setCellFormatter(new CellFormatter() {
            public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
                long l = Long.parseLong(value.toString());
                return Job.formatDuration(l);
            }
        });

        return fields;
    }
    
    
    protected DetailViewerField buildDetailViewer(TasksColumns column){
        return new DetailViewerField(column.getName(), column.getTitle());
    }
    
    
    public Layout build() {
        this.tasksGrid = new ListGrid() {

            @Override
            protected Canvas getExpansionComponent(final ListGridRecord record) {
                if (expandRecord != null && expandRecord != record) {
                    this.collapseRecord(expandRecord);
                }
                TasksView.this.expandRecord = record;

                TaskRecord rec = (TaskRecord) record;
                Task t = rec.getTask();

                DetailViewer detail = new DetailViewer();
                detail.setWidth100();
                detail.setHeight100();
                detail.setCanSelectText(true);

                DetailViewerField [] fields = new DetailViewerField[4];
                fields[0] = buildDetailViewer(TasksColumns.HOST_ATTR);
                fields[1] = buildDetailViewer(TasksColumns.START_TIME_ATTR);
                fields[2] = buildDetailViewer(TasksColumns.FINISHED_TIME_ATTR);
                fields[3] = buildDetailViewer(TasksColumns.DESCRIPTION_ATTR);

                detail.setFields(fields);

                DetailViewerRecord r1 = new DetailViewerRecord();
                r1.setAttribute(TasksColumns.HOST_ATTR.getName(), (t.getHostName().equals("null") ? "" : t.getHostName()));
                r1.setAttribute(TasksColumns.DESCRIPTION_ATTR.getName(), t.getDescription());
                r1.setAttribute(TasksColumns.START_TIME_ATTR.getName(), rec.getAttribute(TasksColumns.START_TIME_ATTR.getName()));
                r1.setAttribute(TasksColumns.FINISHED_TIME_ATTR.getName(), rec.getAttribute(TasksColumns.FINISHED_TIME_ATTR.getName()));

                detail.setData(new DetailViewerRecord[]{r1});

                VLayout layout = new VLayout();
                layout.addMember(detail);

                return layout;
            }

            @Override
            protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
                String base = super.getCellCSSText(record, rowNum, colNum);
                if (colNum == 2) {
                    String st = record.getAttribute(TasksColumns.STATUS_ATTR.getName());
                    if (st.equals(TaskStatus.PENDING.toString()) ||
                            st.equals(TaskStatus.SUBMITTED.toString()))
                        return "color:#1a8bba;" + base;
                    else if (st.equals(TaskStatus.RUNNING.toString()))
                        return "color:#176925;font-weight:bold;" + base;
                    else if (st.equals(TaskStatus.ABORTED.toString()) ||
                            st.equals(TaskStatus.FAILED.toString()))
                        return "color:#d37a11;font-weight:bold;" + base;
                    else if (st.equals(TaskStatus.FAULTY.toString()) ||
                            st.equals(TaskStatus.NOT_STARTED.toString()) ||
                            st.equals(TaskStatus.NOT_RESTARTED.toString()))
                        return "color:#c50000;font-weight:bold;" + base;
                    else
                        return base;
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
                    visuButtons.put(record.getAttributeAsString(TasksColumns.ID_ATTR.getName()), button);

                    for (RemoteHint rh : controller.getModel().getRemoteHints()) {
                        loadRemoteHint(rh, record);
                    }

                    return button;
                } else {
                    return null;
                }
            }
        };
        this.tasksGrid.setWidth100();
        this.tasksGrid.setHeight100();
        this.tasksGrid.setCanExpandRecords(true);
        this.tasksGrid.setCanGroupBy(false);
        this.tasksGrid.setCanReorderFields(true);
        this.tasksGrid.setCanPickFields(true);
        this.tasksGrid.setCanFreezeFields(false);
        this.tasksGrid.setSelectionType(SelectionStyle.SINGLE);
        this.tasksGrid.setShowRecordComponents(true);
        this.tasksGrid.setShowRecordComponentsByCell(true);
        this.tasksGrid.setEmptyMessage("No tasks to show.");
        this.ds = new TaskDS("taskds_" + LoginModel.getInstance().getSessionId());
        this.tasksGrid.setDataSource(this.ds);

        this.tasksGrid.addCellContextClickHandler(new CellContextClickHandler() {
            @Override
            public void onCellContextClick(CellContextClickEvent event) {
                taskClickHandler();
            }
        });

        Collection<ListGridField> fieldsCollection = this.buildListGridField().values();
        ListGridField [] fields = new ListGridField[fieldsCollection.size()];
        this.tasksGrid.setFields(fieldsCollection.toArray(fields));
        this.tasksGrid.sort(TasksColumns.ID_ATTR.getName(), SortDirection.ASCENDING);

        this.loadingLabel = new Label("fetching tasks...");
        this.loadingLabel.setIcon("loading.gif");
        this.loadingLabel.setWidth100();
        this.loadingLabel.setHeight100();
        this.loadingLabel.setAlign(Alignment.CENTER);
        this.loadingLabel.hide();

        this.errorLabel = new Label("");
        this.errorLabel.setWidth100();
        this.errorLabel.setAlign(Alignment.CENTER);
        this.errorLabel.hide();
        
        Layout navTools = this.controller.getTaskNavigationController().buildView();
        Layout paginationBar = this.controller.getTaskNavigationController().getPaginationController().buildView();

        VLayout tasksViewLayout = new VLayout();
        tasksViewLayout.addMember(navTools);
        tasksViewLayout.addMember(this.tasksGrid);
        tasksViewLayout.addMember(this.loadingLabel);
        tasksViewLayout.addMember(this.errorLabel);
        tasksViewLayout.addMember(paginationBar);

        return tasksViewLayout;
    }



    protected void taskClickHandler(){
        final String taskName = tasksGrid.getSelectedRecord().getAttributeAsString(TasksColumns.NAME_ATTR.getName());

        Menu menu = new Menu();
        menu.setShowShadow(true);
        menu.setShadowDepth(10);

        MenuItem restart = new MenuItem("Restart");
        restart.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.restartTask(taskName);
            }
        });
        MenuItem preempt = new MenuItem("Preempt");
        preempt.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.preemptTask(taskName);
            }
        });
        MenuItem kill = new MenuItem("Kill");
        kill.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            @Override
            public void onClick(MenuItemClickEvent event) {
                controller.killTask(taskName);
            }
        });

        boolean enabled;
        ListGridRecord jr = this.tasksGrid.getSelectedRecord();
        TaskStatus status = TaskStatus.valueOf(jr.getAttribute(TasksColumns.STATUS_ATTR.getName()).toUpperCase()); 
        switch (status) {
        case SUBMITTED:
        case WAITING_ON_ERROR:
        case WAITING_ON_FAILURE:
            enabled = false;
            break;
        case PENDING:
        case PAUSED:
        case RUNNING:
            enabled = true;
            break;
        case SKIPPED:
        case FINISHED:
        case FAULTY:
        case FAILED:
        case ABORTED:
        case NOT_STARTED:
        case NOT_RESTARTED:
            enabled = false;
            break;
        default:
            enabled = false;
        }

        restart.setEnabled(enabled);
        kill.setEnabled(enabled);
        preempt.setEnabled(enabled);

        menu.setItems(restart, preempt, kill);
        tasksGrid.setContextMenu(menu);
    }


    /*
     * (non-Javadoc)
     * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.RemoteHintListener#remoteHintRead(java.lang.String)
     */
    public void remoteHintRead(final RemoteHint hint) {
        for (ListGridRecord rec : this.tasksGrid.getRecords()) {
            loadRemoteHint(hint, rec);
        }
    }

    private void loadRemoteHint(final RemoteHint hint, final ListGridRecord rec) {
        String taskId = rec.getAttributeAsString(TasksColumns.ID_ATTR.getName());
        String jobId = this.controller.getModel().getParentModel().getJobsModel().getSelectedJob().getId().toString();
        final String taskName = rec.getAttributeAsString(TasksColumns.NAME_ATTR.getName());
        if (taskId.equals(hint.taskId) && jobId.equals(hint.jobId)) {
            ImgButton button = visuButtons.get(taskId);
            button.setSrc(SchedulerImages.instance.visu_16().getSafeUri().asString());
            if(visuButtonsClickHandlers.containsKey(button)){
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

}
