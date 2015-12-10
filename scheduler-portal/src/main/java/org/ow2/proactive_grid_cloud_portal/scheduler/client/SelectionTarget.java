package org.ow2.proactive_grid_cloud_portal.scheduler.client;

public enum SelectionTarget {

    TASK_TARGET("Selected tasks"),
    JOB_TARGET("Selected job");
    
    public final String label;
    
    private SelectionTarget(String label){
        this.label = label;
    }
    
    
    public static String [] getValuesString(){
        SelectionTarget [] modes = SelectionTarget.values();
        String [] result = new String[modes.length];
        for(int i = 0; i < modes.length; i++){
            result[i] = modes[i].label;
        }
        return result;
    }
}
