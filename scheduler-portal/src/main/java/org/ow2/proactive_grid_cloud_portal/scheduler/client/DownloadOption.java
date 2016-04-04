package org.ow2.proactive_grid_cloud_portal.scheduler.client;

public enum DownloadOption {

    OPT_TEXT("View as text", "text/plain"),
    OPT_BIN("Download as binary", "application/octet-stream");
    
    public String label;
    
    public String formDownload;
    
    DownloadOption(String label, String formDownload){
        this.label = label;
        this.formDownload = formDownload;
    }

}
