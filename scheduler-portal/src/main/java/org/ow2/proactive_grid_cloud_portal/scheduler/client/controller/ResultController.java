package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.DownloadOption;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ResultModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ResultView;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.layout.Layout;

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
    
    
    
    public void changeDownloadType(String value){
        if(value.equals(DownloadOption.OPT_TEXT.label)){
            this.model.setDownloadOption(DownloadOption.OPT_TEXT);
        }
        else{
            this.model.setDownloadOption(DownloadOption.OPT_BIN);
        }
    }
    
    
    public void doDownload(DynamicForm form){
        Task task = this.parentController.getSelectedTask();
        if(task != null){
            String taskId = task.getName();
            form.getField(ResultView.taskIdFieldName).setValue(taskId);

            String jobId = Long.toString(task.getJobId());
            form.getField(ResultView.jobIdFieldName).setValue(jobId);

            DownloadOption dlOption = this.model.getDownloadOption();
            form.getField(ResultView.mediaFieldName).setValue(dlOption.formDownload);

            form.submitForm();
        }
    }


    public SchedulerController getParentController() {
        return parentController;
    }
    
    
}
