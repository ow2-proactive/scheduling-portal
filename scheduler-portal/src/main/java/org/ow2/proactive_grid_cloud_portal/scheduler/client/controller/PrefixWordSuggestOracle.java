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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.controller;

import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONException;
import org.ow2.proactive_grid_cloud_portal.common.client.json.JSONUtils;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobStatus;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Scheduler;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobSelectedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerModelImpl;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerServiceAsync;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.json.SchedulerJSONUtils;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksNavigationModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.IsSerializable;
import com.google.gwt.user.client.ui.SuggestOracle;


/**
 * Controller for the tag suggestions.
 * @author the activeeon team
 *
 */
public class PrefixWordSuggestOracle extends SuggestOracle implements JobSelectedListener {

    /**
     * Scheduler service to send request to the scheduler server.
     */
    protected SchedulerServiceAsync scheduler;

    /**
     * The main scheduler model.
     */
    protected SchedulerModelImpl schedulerModel;

    /**
     * The tasks navigation model
     */
    protected TasksNavigationModel model;

    /**
     * Timestamp of the last request to the server to get the list of tag suggestions.
     */
    protected long lastRequestTime = -1;

    /**
     * The prefix used for the last request to the server to get the list of tag suggestions.
     */
    protected String lastRequest = "";


    /**
     * Builds a tag suggestions controller from the main scheduler model and scheduler service.
     * @param model the main scheduler model.
     */
    public PrefixWordSuggestOracle(TasksNavigationModel model) {
        this.schedulerModel = model.getParentModel().getParentModel();
        this.model = model;
        this.scheduler = Scheduler.getSchedulerService();
        this.schedulerModel.getExecutionsModel().getJobsModel().addJobSelectedListener(this);
    }


    /**
     * A tag suggestion
     * @author the activeeon team
     *
     */
    public static class TagSuggestion implements Suggestion, IsSerializable {
        
        /**
         * The string displayed as a suggestion.
         */
        private String displayString;
        
        /**
         * The string that replace the current text if the suggestion is selected. 
         */
        private String replacementString;



        /**
         * Builds a tag suggestion from a displayed string and a replacement string.
         * @param replacementString the string displayed as a suggestion.
         * @param displayString the string that replace the current text if the suggestion is selected. 
         */
        public TagSuggestion(String replacementString, String displayString) {
            this.replacementString = replacementString;
            this.displayString = displayString;
        }

        /**
         * Gets the string displayed as a suggestion.
         * @return the string displayed as a suggestion.
         */
        public String getDisplayString() {
            return displayString;
        }

        /**
         * Gets the string that replace the current text if the suggestion is selected. 
         * @return the string that replace the current text if the suggestion is selected. 
         */
        public String getReplacementString() {
            return replacementString;
        }
    }


    /**
     * Computes if the set of tag suggestions in local need to be refreshed by a new request to the server,
     * according to the current prefix query.
     * @param query the prefix query
     * @return true if the set of tag suggestions need to refreshed, false otherwise.
     */
    protected boolean needToRefresh(String query){
        if(query.length() < 2){
            return false;
        }


        if(this.lastRequest.equals("") || !query.startsWith(lastRequest)){
            return true;
        }

        JobStatus status = this.schedulerModel.getExecutionsModel().getJobsModel().getSelectedJob().getStatus();
        boolean finishedJob = (status == JobStatus.FINISHED || 
                status == JobStatus.FAILED || 
                status == JobStatus.KILLED || 
                status == JobStatus.CANCELED);
        long requestAge = new Date().getTime() - this.lastRequestTime;
        return (!finishedJob && requestAge > SchedulerConfig.get().getTagSuggestionDelay());
    }


    /**
     * Refresh the set of tag suggestions in local by a request to server, with the given prefix query.
     * @param query the prefix query.
     */
    protected void refresh(final String query){
        this.lastRequestTime = new Date().getTime();
        this.lastRequest = query;

        final String jobId = this.schedulerModel.getExecutionsModel().getJobsModel().getSelectedJob().getId().toString();

        this.scheduler.getJobTaskTagsPrefix(LoginModel.getInstance().getSessionId(), jobId, query, new AsyncCallback<String>() {

            public void onFailure(Throwable caught) {
                String msg = JSONUtils.getJsonErrorMessage(caught);

                LogModel.getInstance().logImportantMessage("Failed to update tags for job " +
                        jobId + " and prefix tag " + query + ": " + msg);
            }

            public void onSuccess(String result) {
                try {
                    List<String> tags = SchedulerJSONUtils.getTagsFromJson(result);
                    model.setTagSuggestions(tags);
                } catch (JSONException e) {
                    LogModel.getInstance().logCriticalMessage(e.getMessage());
                }
            }
        });

    }


    /**
     * Provides in the given callback, a set of tag suggestions.
     * @param request the tag request as a prefix query.
     * @param callback the callback that gets the resulting set of tag suggestions.
     */
    @Override
    public void requestSuggestions(Request request, Callback callback) {
        Response response = new Response();

        if(this.schedulerModel.getExecutionsModel().getJobsModel().getSelectedJob() != null){
            String query = request.getQuery();
            if(this.needToRefresh(query)){
                this.refresh(query);
            }

            Collection<TagSuggestion> results = this.model.getAvailableTags(query);	
            response.setSuggestions(results);
        }
        callback.onSuggestionsReady(request, response);
    }


   
    @Override
    public void jobSelected(Job job) {
        this.resetTagSuggestions();
    }


    @Override
    public void jobUnselected() {
        this.resetTagSuggestions();
    }
    
    
    public void resetTagSuggestions(){
        this.lastRequest = "";
        this.lastRequestTime = -1;
        this.model.clearTagSuggestions();
    }
    
    
    @Override
    public void selectedJobUpdated() {   
    }

}
