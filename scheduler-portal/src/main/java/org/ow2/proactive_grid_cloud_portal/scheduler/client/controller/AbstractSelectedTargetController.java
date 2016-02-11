package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.AbstractSelectedTargetModel;

public abstract class AbstractSelectedTargetController<M extends AbstractSelectedTargetModel> {

    protected SchedulerController parentController;
    
    protected M model;
    
    
    public AbstractSelectedTargetController(SchedulerController parentController){
        this.parentController = parentController;
    }
    
    public SchedulerController getParentController() {
        return parentController;
    }
    
    
    public void switchMode(ExecutionListMode mode){
        SelectionTarget target = this.model.getSelectionTarget();
        if(target == SelectionTarget.JOB_TARGET){
            Job job = this.parentController.getSelectedJob();
            this.changeJobOutputContext(job);
        }
        else{
            Task task = this.parentController.getSelectedTask();
            this.changeTaskOutputContext(task);
        }
    }
    
    
    public void changeSelectedJob(Job job){
        if(this.model.getSelectionTarget() == SelectionTarget.JOB_TARGET){
            this.changeJobOutputContext(job);
        }
    }
    
    public void changeSelectedTask(Task task){
        if(this.model.getSelectionTarget() == SelectionTarget.TASK_TARGET){
            this.changeTaskOutputContext(task);
        }
    }
    
    
    public abstract void changeJobOutputContext(Job job);
    
    
    public abstract void changeTaskOutputContext(Task task);
    
    
    public String getNoTargetLabelContent(){
        SelectionTarget target = this.model.getSelectionTarget();
        return target.noSelectionLabel;
    }
    
    public M getModel() {
        return model;
    }
    
    
    public void changeTargetOutput(SelectionTarget target){
        this.model.setSelectionTarget(target);
        if(target == SelectionTarget.JOB_TARGET){
            Job job = this.parentController.getSelectedJob();
            this.changeJobOutputContext(job);
        }
        else{
            Task task = this.parentController.getSelectedTask();
            this.changeTaskOutputContext(task);
        }
    }
    
    
    public abstract void refreshOutput();
}
