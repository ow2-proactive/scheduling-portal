/*
 *  *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2014 INRIA/University of
 *                 Nice-Sophia Antipolis/ActiveEon
 * Contact: proactive@ow2.org or contact@activeeon.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation; version 3 of
 * the License.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
 * USA
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 *
 *  Initial developer(s):               The ProActive Team
 *                        http://proactive.inria.fr/team_members.htm
 *  Contributor(s):
 *
 *  * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.scheduler.client.suggestions;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Controller;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.json.client.JSONArray;
import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONParser;
import com.google.gwt.json.client.JSONValue;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.IsSerializable;
import com.google.gwt.user.client.ui.SuggestOracle;

public class PrefixWordSuggestOracle extends SuggestOracle implements JobSelectedListener {


    protected SchedulerServiceAsync scheduler;

    protected SchedulerModelImpl schedulerModel;

    protected TasksNavigationModel model;

    protected long lastRequestTime = -1;

    protected String lastRequest = "";



    public PrefixWordSuggestOracle(SchedulerModelImpl model, SchedulerServiceAsync scheduler) {
        this.schedulerModel = model;
        this.model = model.getTasksNavigationModel();
        this.scheduler = scheduler;
        model.addJobSelectedListener(this);
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

        JobStatus status = this.schedulerModel.getSelectedJob().getStatus();
        boolean finishedJob = (status == JobStatus.FINISHED || 
                status == JobStatus.FAILED || 
                status == JobStatus.KILLED || 
                status == JobStatus.CANCELED);
        long requestAge = new Date().getTime() - this.lastRequestTime;
        return (!finishedJob && requestAge > SchedulerConfig.get().getTagSuggestionDelay());
    }


    protected void refresh(final String query){
        this.lastRequestTime = new Date().getTime();
        this.lastRequest = query;

        final String jobId = this.schedulerModel.getSelectedJob().getId().toString();

        this.scheduler.getJobTaskTagsPrefix(this.schedulerModel.getSessionId(), jobId, query, new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                String msg = Controller.getJsonErrorMessage(caught);

                PrefixWordSuggestOracle.this.schedulerModel.logImportantMessage("Failed to update tags for job " +
                        jobId + " and prefix tag " + query + ": " + msg);
            }

            public void onSuccess(String result) {
                List<String> tags;

                JSONValue val = parseJSON(result);
                JSONArray arr = val.isArray();
                if (arr == null) {
                    schedulerModel.logCriticalMessage("Expected JSON Array: " + val.toString());
                }
                tags = getTagsFromJson(arr);
                model.setTagSuggestions(tags);
            }
        });

    }


    @Override
    public void requestSuggestions(Request request, Callback callback) {
        Response response = new Response();

        if(this.schedulerModel.getSelectedJob() != null){
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

            this.schedulerModel.logCriticalMessage(
                    "JSON Parser failed " + t.getClass().getName() + ": " + t.getLocalizedMessage());
            this.schedulerModel.logCriticalMessage("input was: " + jsonStr);
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
