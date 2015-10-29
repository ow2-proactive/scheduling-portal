package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.ExecutionsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.ExecutionsView;

import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStackSection;

public class ExecutionsController {

    protected ExecutionsView view;
    
    protected JobsController jobsController;
    
    protected TasksCentricController tasksController;
    
    protected ExecutionsModel model;
    
    protected SchedulerController parentController;
    
    
    public ExecutionsController(SchedulerController controller) {
        this.parentController = controller;
        SchedulerModelImpl schedulerModel = (SchedulerModelImpl) controller.getModel(); 
        this.model = new ExecutionsModel(schedulerModel);
        schedulerModel.setExecutionsModel(this.model);
    }
    
    
    public SectionStackSection buildView(){
        this.view = new ExecutionsView(this);
        return this.view.build();
    }
    
    
    public Layout buildJobsView(){
        this.jobsController = new JobsController(this);
        return this.jobsController.buildView();
    }
    
    
    public Layout buildTasksView(){
        this.tasksController = new TasksCentricController(this.parentController);
        return this.tasksController.buildView();
    }


    public JobsController getJobsController() {
        return jobsController;
    }


    public void setJobsController(JobsController jobsController) {
        this.jobsController = jobsController;
    }


    public TasksController getTasksController() {
        return tasksController;
    }


    public void setTasksController(TasksCentricController tasksController) {
        this.tasksController = tasksController;
    }
    
    
    public void switchMode(String mode){
        if(mode.equals(ExecutionListMode.JOB_CENTRIC.name)){
            this.model.setMode(ExecutionListMode.JOB_CENTRIC);
        }
        else{
            this.model.setMode(ExecutionListMode.TASK_CENTRIC);
        }
    }


    public ExecutionsModel getModel() {
        return model;
    }


    public SchedulerController getParentController() {
        return parentController;
    }
    
}
