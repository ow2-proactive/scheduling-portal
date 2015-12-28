package org.ow2.proactive_grid_cloud_portal.scheduler.client;

public enum OutputMode {
    
    LOG_OUT_ERR("Out & Err (1024 lines)"),
    LOG_ERR("Std Err"),
    LOG_OUT("Std Out"),
    LOG_FULL("Full logs (download)");
    
    public final String label;
    
    private OutputMode(String label){
        this.label = label;
    }
 
    
    public static String [] toStringArray(){
        OutputMode [] modes = OutputMode.values();
        String [] result = new String[modes.length];
        for(int i = 0; i < modes.length; i++){
            result[i] = modes[i].label;
        }
        return result;
    }
}
