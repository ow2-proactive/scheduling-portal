package org.ow2.proactive_grid_cloud_portal.scheduler.client;

public enum SelectionTarget {

    TASK_TARGET("Selected tasks", "No task selected"),
    JOB_TARGET("Selected job", "No job selected");
    
    public final String label;
    
    public final String noSelectionLabel;
    
    private SelectionTarget(String label, String noSelectionLabel){
        this.label = label;
        this.noSelectionLabel = noSelectionLabel;
    }
    
    
    public static String [] toStringArray(){
        SelectionTarget [] modes = SelectionTarget.values();
        String [] result = new String[modes.length];
        for(int i = 0; i < modes.length; i++){
            result[i] = modes[i].label;
        }
        return result;
    }
}
