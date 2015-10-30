package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import java.util.EnumMap;
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

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerRegistration;
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
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

public class TasksListGrid extends ItemsListGrid implements TasksUpdatedListener, RemoteHintListener{

    
    protected TasksController controller;
    
    private HashMap<String, ImgButton> visuButtons = null;

    /** To avoid opening severial popup on button's click */
    private Map<ImgButton, HandlerRegistration> visuButtonsClickHandlers;
    
    public TasksListGrid(TasksController controller) {
        this.controller = controller;
        this.visuButtons = new HashMap<String, ImgButton>();
        this.visuButtonsClickHandlers = new HashMap<ImgButton, HandlerRegistration>();
        this.controller.getModel().addTasksUpdatedListener(this);
        this.controller.getModel().addRemoteHintListener(this);
        this.emptyMessage = "No tasks to show.";
        this.datasourceNamePrefix = "taskds_";
    }
    
    
    @Override
    public void build() {
        super.build();
        this.setSelectionType(SelectionStyle.SINGLE);
        this.sort(TasksColumns.ID_ATTR.getName(), SortDirection.ASCENDING);
        this.setShowRecordComponents(true);
        this.setShowRecordComponentsByCell(true);
    }
    
    
    protected EnumMap<TasksColumns, ListGridField> getColumnsForListGridField(){
        EnumMap<TasksColumns, ListGridField> columns = new EnumMap<TasksColumns, ListGridField>(TasksColumns.class);
        for(TasksColumns col: TasksColumns.values()){
            columns.put(col, null);
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
    
    
    
    @Override
    public void remoteHintRead(RemoteHint hint) {
        for (ListGridRecord rec : this.getRecords()) {
            loadRemoteHint(hint, rec);
        }
    }


    protected TaskRecord updateTaskRecord(Task task){
        return new TaskRecord(task);
    }
    
    
    @Override
    public void tasksUpdated(List<Task> tasks, long totalTasks) {
        this.visuButtons.clear();

        TaskRecord[] data = new TaskRecord[tasks.size()];
        int i = 0;
        for (Task t : tasks) {
            data[i] = new TaskRecord(t);
            i++;
        }

        this.setData(data);
        this.invalidateCache();
        //this.ds.setTestData(data);
        //this.tasksGrid.fetchData();
        
    }

    
    @Override
    public void tasksUpdating() {
    }
    
    @Override
    public void tasksUpdatedFailure(String message) {
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
    
    
    private void loadRemoteHint(final RemoteHint hint, final ListGridRecord rec) {
        String taskId = rec.getAttributeAsString(TasksColumns.ID_ATTR.getName());
        String jobId = this.controller.getModel().getParentModel().getExecutionsModel().getJobsModel().getSelectedJob().getId().toString();
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

   

    @Override
    protected void buildCellContextualMenu(Menu menu) {
        final String taskName = this.getSelectedRecord().getAttributeAsString(TasksColumns.NAME_ATTR.getName());
        
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
        ListGridRecord jr = this.getSelectedRecord();
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
        
    }

    @Override
    protected void selectionChangedHandler(SelectionEvent event) {
    }
    
}
