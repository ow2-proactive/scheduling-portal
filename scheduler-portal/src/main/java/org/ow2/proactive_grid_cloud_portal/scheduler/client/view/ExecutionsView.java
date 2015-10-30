package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionsController;

import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStackSection;

public class ExecutionsView implements ExecutionDisplayModeListener{

    protected ExecutionsController controller;
    
    protected Layout jobsPane;
    
    protected Layout tasksPane;
    
    protected CheckboxItem chkMy;
    
    protected CheckboxItem chkPending;
    
    protected CheckboxItem chkRunning;
    
    protected CheckboxItem chkFinished;
    
    public ExecutionsView(ExecutionsController controller) {
        this.controller = controller;
        this.controller.getModel().addExecutionsDisplayModeListener(this);
    }
    
    public SectionStackSection build(){
        this.jobsPane = this.controller.buildJobsView();
        this.tasksPane = this.controller.buildTasksView();

        HLayout panesLayout = new HLayout();
        panesLayout.addMember(jobsPane);
        panesLayout.addMember(tasksPane);
        
        SectionStackSection executionsSection = new SectionStackSection();
        executionsSection.setTitle("Executions list");
        executionsSection.setExpanded(true);
        executionsSection.setItems(panesLayout);
        
        this.tasksPane.hide();

        chkMy = new CheckboxItem("myjobs", "My jobs");
        chkMy.setValue(false);
        chkMy.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchMyJobsOnly(chkMy.getValueAsBoolean());
            }
        });
        chkPending = new CheckboxItem("pending", "Pending");
        chkPending.setValue(true);
        chkPending.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchPending(chkPending.getValueAsBoolean());
            }
        });
        chkRunning = new CheckboxItem("running", "Running");
        chkRunning.setValue(true);
        chkRunning.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchRunning(chkRunning.getValueAsBoolean());
            }
        });
        chkFinished = new CheckboxItem("finished", "Finished");
        chkFinished.setValue(true);
        chkFinished.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchFinished(chkFinished.getValueAsBoolean());
            }
        });
        
        final SelectItem modeSelect = new SelectItem();
        modeSelect.setValueMap(ExecutionListMode.JOB_CENTRIC.name, ExecutionListMode.TASK_CENTRIC.name);
        modeSelect.setValue(ExecutionListMode.JOB_CENTRIC.name);
        modeSelect.setShowTitle(false);
        modeSelect.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                controller.switchMode((String) modeSelect.getValue());
            }
        });

        // for some reason IE9 standards fails to detect the right width
        if (SC.isIE()) {
            chkMy.setWidth(60);
            chkPending.setWidth(60);
            chkRunning.setWidth(60);
            chkFinished.setWidth(60);
        }

        DynamicForm checkBoxes = new DynamicForm();
        checkBoxes.setNumCols(10);
        checkBoxes.setItems(chkMy, chkPending, chkRunning, chkFinished, modeSelect);

        String user = LoginModel.getInstance().getLogin();
        // login unknown: credentials login; fetching only my jobs will be impossible server side
        if (user == null || user.trim().length() == 0) {
            chkMy.setDisabled(true);
        }

        Canvas fill = new Canvas();
        fill.setWidth(5);
        executionsSection.setControls(checkBoxes, fill);

        return executionsSection;
    }
    
    
    @Override
    public void modeSwitched(ExecutionListMode mode) {
        switch(mode){
        case JOB_CENTRIC:
            this.tasksPane.hide();
            this.jobsPane.show();
            chkMy.setTitle("My jobs");
            chkMy.redraw();
            break;
        case TASK_CENTRIC:
            this.jobsPane.hide();
            this.tasksPane.show();
            chkMy.setTitle("My tasks");
            chkMy.redraw();
        }
        
    }
}
