/*
 * ProActive Parallel Suite(TM):
 * The Open Source library for parallel and distributed
 * Workflows & Scheduling, Orchestration, Cloud Automation
 * and Big Data Analysis on Enterprise Grids & Clouds.
 *
 * Copyright (c) 2007 - 2017 ActiveEon
 * Contact: contact@activeeon.com
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation: version 3 of
 * the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 *
 * If needed, contact us to obtain a release under GPL Version 2 or 3
 * or a different license than the AGPL.
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;

import org.apache.commons.collections4.trie.PatriciaTrie;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TagSuggestionListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.TasksUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.PrefixWordSuggestOracle.TagSuggestion;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;


/**
 * Model part for the tasks navigation.
 * @author activeeon team
 *
 */
public class TasksNavigationModel {

    /**
     * Tag filter applied to filter the tasks.
     */
    private String tasksTagFilter = "";

    /**
     * Available tags suggestions for filtering.
     */
    private PatriciaTrie<String> availableTags = null;

    /**
     * Indicates if the tasks are automatically refreshed.
     */
    private boolean taskAutoRefreshOption = true;

    /**
     * Model for the pagination of the tasks.
     */
    private PaginationModel paginationModel;

    /**
     * Listeners about the tags suggestions.
     */
    private ArrayList<TagSuggestionListener> tagSuggestionListeners = null;

    /**
     * Listeners about the task updating.
     */
    private ArrayList<TasksUpdatedListener> tasksUpdatedListeners = null;

    protected TasksModel parentModel;

    public TasksNavigationModel(TasksModel parentModel) {
        this.parentModel = parentModel;
        this.parentModel.setTasksNavigationModel(this);

        this.availableTags = new PatriciaTrie<String>();
        this.paginationModel = new PaginationModel(PaginatedItemType.TASK);

        this.tagSuggestionListeners = new ArrayList<TagSuggestionListener>();
        this.tasksUpdatedListeners = new ArrayList<TasksUpdatedListener>();
    }

    public TasksModel getParentModel() {
        return parentModel;
    }

    /**
     * Computes the sets of tags suggestions that matches a given prefix.
     * @param query the prefix to be matched.
     * @return the set of available tags suggestions that matches a prefix.
     */
    public Collection<TagSuggestion> getAvailableTags(String query) {
        SortedMap<String, String> mapSuggestions = this.availableTags.prefixMap(query);
        int size = SchedulerConfig.get().getTagSuggestionSize();
        ArrayList<TagSuggestion> suggestions = new ArrayList<TagSuggestion>(size);
        Iterator<Map.Entry<String, String>> it = mapSuggestions.entrySet().iterator();
        for (int i = 0; i < size && it.hasNext(); i++) {
            Map.Entry<String, String> current = it.next();
            TagSuggestion suggestion = new TagSuggestion(current.getValue(), current.getKey());
            suggestions.add(suggestion);
        }

        return suggestions;
    }

    /**
     * Sets the available tags suggestions.
     * @param tags the available tags suggestions.
     */
    public void setTagSuggestions(Collection<String> tags) {
        for (String currentTag : tags) {
            int index = currentTag.indexOf("<index>");
            if (index >= 0) {
                this.availableTags.put(currentTag, currentTag.substring(0, index));
            } else {
                this.availableTags.put(currentTag, currentTag);
            }
        }

        for (TagSuggestionListener currentListener : this.tagSuggestionListeners) {
            currentListener.tagSuggestionListUpdated();
        }
    }

    public PaginationModel getPaginationModel() {
        return paginationModel;
    }

    /**
     * Set the current tag used to filter the list of tasks.
     * @param tag the tag used to filter the list of tasks.
     * @return true if the tag value has changed, false otherwise.
     */
    public boolean setCurrentTagFilter(String tag) {
        boolean result = !this.tasksTagFilter.equals(tag);
        if (result) {
            this.tasksTagFilter = tag;
            for (TasksUpdatedListener list : this.tasksUpdatedListeners) {
                list.tasksUpdating();
            }
        }
        return result;
    }

    public String getCurrentTagFilter() {
        return this.tasksTagFilter;
    }

    public void resetTagFilter() {
        this.tasksTagFilter = "";
    }

    /**
     * Add a listener to the tags suggestions modifications.
     * @param listener the listener.
     */
    public void addTagSuggestionListener(TagSuggestionListener listener) {
        this.tagSuggestionListeners.add(listener);
    }

    public boolean getTaskAutoRefreshOption() {
        return this.taskAutoRefreshOption;
    }

    public void setTaskAutoRefreshOption(boolean value) {
        this.taskAutoRefreshOption = value;
    }

    /**
     * Clears the set of available tags suggestions.
     */
    public void clearTagSuggestions() {
        this.availableTags.clear();
    }
}
