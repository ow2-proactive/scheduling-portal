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
        
        this.jobsPane.hide();

        final CheckboxItem c1 = new CheckboxItem("myjobs", "My jobs");
        c1.setValue(false);
        c1.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchMyJobsOnly(c1.getValueAsBoolean());
            }
        });
        final CheckboxItem c2 = new CheckboxItem("pending", "Pending");
        c2.setValue(true);
        c2.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchPending(c2.getValueAsBoolean());
            }
        });
        final CheckboxItem c3 = new CheckboxItem("running", "Running");
        c3.setValue(true);
        c3.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchRunning(c3.getValueAsBoolean());
            }
        });
        final CheckboxItem c4 = new CheckboxItem("finished", "Finished");
        c4.setValue(true);
        c4.addChangedHandler(new ChangedHandler() {
            public void onChanged(ChangedEvent event) {
                controller.getJobsController().fetchFinished(c4.getValueAsBoolean());
            }
        });
        
        final SelectItem modeSelect = new SelectItem();
        modeSelect.setValueMap(ExecutionListMode.JOB_CENTRIC.name, ExecutionListMode.TASK_CENTRIC.name);
        modeSelect.setValue(ExecutionListMode.TASK_CENTRIC.name);
        modeSelect.setShowTitle(false);
        modeSelect.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                controller.switchMode((String) modeSelect.getValue());
            }
        });

        // for some reason IE9 standards fails to detect the right width
        if (SC.isIE()) {
            c1.setWidth(60);
            c2.setWidth(60);
            c3.setWidth(60);
            c4.setWidth(60);
        }

        DynamicForm checkBoxes = new DynamicForm();
        checkBoxes.setNumCols(10);
        checkBoxes.setItems(c1, c2, c3, c4, modeSelect);

        String user = LoginModel.getInstance().getLogin();
        // login unknown: credentials login; fetching only my jobs will be impossible server side
        if (user == null || user.trim().length() == 0) {
            c1.setDisabled(true);
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
            break;
        case TASK_CENTRIC:
            this.jobsPane.hide();
            this.tasksPane.show();
        }
        
    }
}
