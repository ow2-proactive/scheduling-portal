package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.layout.Layout;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ResultModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView;

public class ResultController {

    protected SchedulerController parentController;
    
    protected ResultModel model;
    
    protected ResultView view;
    
    public ResultController(SchedulerController parentController){
        this.parentController = parentController;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) this.parentController.getModel();
        this.model = new ResultModel(schedulerModel);
    }

    public Layout buildView(){
        this.view = new ResultView(this);
        return this.view.build();
    }


    public void doDownload(DynamicForm form, String contentType, String target) {
        Task task = this.parentController.getSelectedTask();
        if(task != null){
            String taskId = task.getName();
            form.getField(ResultView.TASK_ID_FIELD_NAME).setValue(taskId);

            String jobId = Long.toString(task.getJobId());
            form.getField(ResultView.JOB_ID_FIELD_NAME).setValue(jobId);

            form.getField(ResultView.DESTINATION_FIELD_NAME).setValue(contentType);

            form.setTarget(target);

            form.submitForm();
        }
    }


    public SchedulerController getParentController() {
        return parentController;
    }
    
    
}
