package org.ow2.proactive_grid_cloud_portal.scheduler.client.suggestions;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Controller;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.IsSerializable;
import com.google.gwt.user.client.ui.SuggestOracle;

public class PrefixWordSuggestOracle extends SuggestOracle implements JobSelectedListener {


    protected SchedulerServiceAsync scheduler;

    protected SchedulerModelImpl model;

    protected long lastRequestTime = -1;

    protected String lastRequest = "";


    protected static final long DELAY = 1000 * 60 * 5; // 5 mins



    public PrefixWordSuggestOracle(SchedulerModelImpl model, SchedulerServiceAsync scheduler) {
        this.model = model;
        this.scheduler = scheduler;
        this.model.addJobSelectedListener(this);
    }


    public static class TagSuggestion implements Suggestion, IsSerializable {
        private String displayString;
        private String replacementString;


        public TagSuggestion() {
        }


        public TagSuggestion(String replacementString, String displayString) {
            this.replacementString = replacementString;
            this.displayString = displayString;
        }

        public String getDisplayString() {
            return displayString;
        }

        public String getReplacementString() {
            return replacementString;
        }
    }



    protected boolean needToRefresh(String query){
        if(query.length() < 2){
            return false;
        }


        if(this.lastRequest.equals("") || !query.startsWith(lastRequest)){
            return true;
        }

        JobStatus status = this.model.getSelectedJob().getStatus();
        boolean finishedJob = (status == JobStatus.FINISHED || 
                status == JobStatus.FAILED || 
                status == JobStatus.KILLED || 
                status == JobStatus.CANCELED);
        long requestAge = new Date().getTime() - this.lastRequestTime;
        return (!finishedJob && requestAge > this.DELAY);
    }


    protected void refresh(final String query){
        this.lastRequestTime = new Date().getTime();
        this.lastRequest = query;

        final String jobId = this.model.getSelectedJob().getId().toString();

        this.scheduler.getJobTaskTagsPrefix(model.getSessionId(), jobId, query, new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                String msg = Controller.getJsonErrorMessage(caught);

                PrefixWordSuggestOracle.this.model.logImportantMessage("Failed to update tags for job " +
                        jobId + " and prefix tag " + query + ": " + msg);
            }

            public void onSuccess(String result) {
                List<String> tags;

                JSONValue val = parseJSON(result);
                JSONArray arr = val.isArray();
                if (arr == null) {
                    model.logCriticalMessage("Expected JSON Array: " + val.toString());
                }
                tags = getTagsFromJson(arr);
                model.setTagSuggestions(tags);
            }
        });

    }


    @Override
    public void requestSuggestions(Request request, Callback callback) {
        Response response = new Response();

        if(this.model.getSelectedJob() != null){
            String query = request.getQuery();
            if(this.needToRefresh(query)){
                this.refresh(query);
            }

            Collection<TagSuggestion> results = this.model.getAvailableTags(query);	
            response.setSuggestions(results);
        }
        callback.onSuggestionsReady(request, response);
    }


    /**
     * Parse a JSON string
     * <p>
     * If the input is not valid JSON or parsing fails for some reason,
     * the exception will be logged in the UI but not thrown.
     * 
     * @param jsonStr a valid JSON string
     * @return a java representation of the JSON object hierarchy,
     *     or a JSONObject representing {} if parsing fails
     */
    public JSONValue parseJSON(String jsonStr) {
        try {
            return JSONParser.parseStrict(jsonStr);
        } catch (Throwable t) {
            // only shows up in eclipse dev mode
            t.printStackTrace();

            this.model.logCriticalMessage(
                    "JSON Parser failed " + t.getClass().getName() + ": " + t.getLocalizedMessage());
            this.model.logCriticalMessage("input was: " + jsonStr);
            return new JSONObject();
        }
    }


    /**
     * @param arr list of tags as a JSON array
     * @return the list of tags
     */
    private List<String> getTagsFromJson(JSONArray arr) {
        List<String> tags = new ArrayList<String>(arr.size());

        for (int i = 0; i < arr.size(); i++) {
            tags.add(arr.get(i).isString().stringValue());
        }

        return tags;
    }


    @Override
    public void jobSelected(Job job) {
        this.lastRequest = "";
        this.lastRequestTime = -1;
    }


    @Override
    public void jobUnselected() {
        this.lastRequest = "";
        this.lastRequestTime = -1;
    }

}
