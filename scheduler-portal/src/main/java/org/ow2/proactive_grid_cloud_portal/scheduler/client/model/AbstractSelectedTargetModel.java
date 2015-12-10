package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SelectionTarget;

public abstract class AbstractSelectedTargetModel {

    protected SelectionTarget selectionTarget = SelectionTarget.JOB_TARGET;
    
    protected SchedulerModelImpl parentModel; 
    
    public AbstractSelectedTargetModel(SchedulerModelImpl parentModel){
        this.parentModel = parentModel;
    }
    
    
    public SelectionTarget getSelectionTarget() {
        return selectionTarget;
    }
    
    
    public boolean setSelectionTarget(SelectionTarget selectionTarget) {
        if(this.selectionTarget != selectionTarget){
            this.selectionTarget = selectionTarget;
            return true;
        }
        else{
            return false;
        }
    }
    
    public SchedulerModelImpl getParentModel() {
        return parentModel;
    }
}
