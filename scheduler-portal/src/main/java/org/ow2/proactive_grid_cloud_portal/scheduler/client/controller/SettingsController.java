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

import org.ow2.proactive_grid_cloud_portal.common.client.Settings;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.smartgwt.client.widgets.form.DynamicForm;


/**
 * Controller for the dynamic settings view.
 * @author the activeeon team.
 *
 */
public class SettingsController {

    /**
     * The scheduler controller.
     */
    protected SchedulerController mainController;
    
    /**
     * The config of the scheduler portal, read from config files.
     */
    protected SchedulerConfig config = SchedulerConfig.get();
    
    
    public SettingsController(SchedulerController mainController){
        this.mainController = mainController;
    }
    
    /**
     * Apply new settings.
     * @param newSettings the form that contains the new settings to be applied.
     * @param forceRefresh true if the scheduler portal should refresh even if the value has not been modified.
     */
    public void applySettings(DynamicForm newSettings, boolean forceRefresh){
        if (!newSettings.validate())
            return;

        
        if(setSetting(SchedulerConfig.CLIENT_REFRESH_TIME, 
                     newSettings.getValueAsString("refreshTime")) || forceRefresh){
            this.mainController.restartTimer();
        }
        
        if(setSetting(SchedulerConfig.LIVELOGS_REFRESH_TIME, 
                newSettings.getValueAsString("logTime")) || forceRefresh){
            this.mainController.restartLiveTimer();
        }
        
        if(setSetting(SchedulerConfig.JOBS_PAGE_SIZE, 
                newSettings.getValueAsString("jobPageSize")) || forceRefresh){
            this.mainController.getJobsPaginationController().firstPage();
        }
        
        if(setSetting(SchedulerConfig.TASKS_PAGE_SIZE, 
                newSettings.getValueAsString("taskPageSize")) || forceRefresh){
            this.mainController.getTaskNavigationController().getPaginationController().firstPage();
        }
        
        
        boolean tagSuggestionSizeChanged = setSetting(SchedulerConfig.TAG_SUGGESTIONS_SIZE, 
                newSettings.getValueAsString("tagSuggestionsSize"));
        boolean tagSuggestionsDelayChanged = setSetting(SchedulerConfig.TAG_SUGGESTIONS_DELAY, 
                newSettings.getValueAsString("tagSuggestionsDelay"));
        if(tagSuggestionSizeChanged || tagSuggestionsDelayChanged || forceRefresh){
            this.mainController.getTaskNavigationController().getTagSuggestionOracle().resetTagSuggestions();
        }
    }
    
    
    /**
     * Reset the scheduler portal settings
     * @param formSettings the form that contains the settings to be reset.
     */
    public void resetSettings(DynamicForm formSettings){
        SchedulerConfig.get().reload();
        initForm(formSettings);
        applySettings(formSettings, true);
    }
    
    
    
    public void initForm(DynamicForm formSettings){
        formSettings.setValue("refreshTime", config.getClientRefreshTime());
        formSettings.setValue("logTime", config.getLivelogsRefreshTime());
        formSettings.setValue("jobPageSize", config.getPageSize(PaginatedItemType.JOB));
        formSettings.setValue("taskPageSize", config.getPageSize(PaginatedItemType.TASK));
        formSettings.setValue("tagSuggestionsSize", config.getTagSuggestionSize());
        formSettings.setValue("tagSuggestionsDelay", config.getTagSuggestionDelay());
    }
    
    
    /**
     * Set the new value of a setting.
     * @param settingName the name of the setting to be set.
     * @param settingValue the new value for the setting.
     * @return true if the value has been changed, false otherwise.
     */
    protected boolean setSetting(String settingName, String settingValue){
        boolean result = !settingValue.equals("" + SchedulerConfig.get().getProperties().get(settingName));
        if(result){
            SchedulerConfig.get().set(settingName, settingValue);
            Settings.get().setSetting(settingName, settingValue);
        }
        return result;
    }
}
