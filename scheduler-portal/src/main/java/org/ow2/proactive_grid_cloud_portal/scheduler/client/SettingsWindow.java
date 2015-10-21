/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.SettingsController;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PaginatedItemType;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.validator.IntegerRangeValidator;
import com.smartgwt.client.widgets.form.validator.IsIntegerValidator;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * Client-side settings edition
 * Effective immediately, stored in a cookie,
 * lasts until cookie expires or is removed
 * 
 * 
 * @author mschnoor
 *
 */
public class SettingsWindow {

    private Window window;
    
    private SettingsController settingsController;

    public SettingsWindow(SchedulerController controller) {
        this.settingsController = new SettingsController(controller);
        this.build();
    }

    public void show() {
        this.window.show();
    }

    public void destroy() {
        this.window.destroy();
    }

    private void build() {

        DataSourceIntegerField refreshTime = new DataSourceIntegerField("refreshTime",
            "Job list refresh rate (ms)");
        refreshTime.setRequired(true);
        IntegerRangeValidator refreshValidator = new IntegerRangeValidator();
        refreshValidator.setMin(500);
        refreshValidator.setMax(60000);
        refreshTime.setValidators(new IsIntegerValidator(), refreshValidator);

        DataSourceIntegerField liveLogTime = new DataSourceIntegerField("logTime",
            "Live log refresh rate (ms)");
        liveLogTime.setRequired(true);
        IntegerRangeValidator liveLogValidator = new IntegerRangeValidator();
        liveLogValidator.setMin(100);
        liveLogValidator.setMax(10000);
        liveLogTime.setValidators(new IsIntegerValidator(), liveLogValidator);

        DataSourceIntegerField jobPageSize = new DataSourceIntegerField("jobPageSize", "Jobs page size");
        jobPageSize.setRequired(true);
        DataSourceIntegerField taskPageSize = new DataSourceIntegerField("taskPageSize", "Tasks page size");
        taskPageSize.setRequired(true);
        IntegerRangeValidator pageValidator = new IntegerRangeValidator();
        pageValidator.setMin(5);
        pageValidator.setMax(5000);
        jobPageSize.setValidators(new IsIntegerValidator(), pageValidator);
        taskPageSize.setValidators(new IsIntegerValidator(), pageValidator);
        
        DataSourceIntegerField tagSuggestionSize = new DataSourceIntegerField("tagSuggestionsSize", "Max tag suggestions");
        tagSuggestionSize.setRequired(true);
        IntegerRangeValidator tagSizeValidator = new IntegerRangeValidator();
        tagSizeValidator.setMin(5);
        tagSizeValidator.setMax(100);
        tagSuggestionSize.setValidators(new IsIntegerValidator(), tagSizeValidator);
        
        DataSourceIntegerField tagSuggestionDelay = new DataSourceIntegerField("tagSuggestionsDelay", "Tag suggestions refresh time (ms)");
        tagSuggestionDelay.setRequired(true);
        IntegerRangeValidator tagDelayValidator = new IntegerRangeValidator();
        tagDelayValidator.setMin(100);
        tagDelayValidator.setMax(60000);
        tagSuggestionDelay.setValidators(new IsIntegerValidator(), tagDelayValidator);

        final DataSource ds = new DataSource();
        ds.setFields(refreshTime, liveLogTime, jobPageSize, taskPageSize, tagSuggestionSize, tagSuggestionDelay);

        final DynamicForm form = new DynamicForm();
        form.setDataSource(ds);
        form.setColWidths("300", "230");
        form.setWidth(420);
        form.setMargin(10);
        settingsController.initForm(form);
        

        final IButton applyButton = new IButton("Ok");
        applyButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        applyButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                settingsController.applySettings(form, false);
                window.hide();
            }
        });

        final IButton resetButton = new IButton("Reset");
        resetButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                settingsController.resetSettings(form);
            }
        });

        final IButton cancelButton = new IButton("Cancel");
        cancelButton.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        cancelButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                window.hide();
            }
        });

        HLayout vl = new HLayout();
        vl.setAlign(Alignment.RIGHT);
        vl.setLayoutLeftMargin(5);
        vl.setHeight(applyButton.getHeight());
        vl.setWidth100();
        vl.setMembersMargin(5);
        vl.setMargin(5);
        vl.setMembers(resetButton, applyButton, cancelButton);

        VLayout layout = new VLayout();
        layout.setMembersMargin(20);
        layout.addMember(form);
        layout.addMember(vl);

        this.window = new Window();
        this.window.setTitle("Settings");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setWidth(440);
        this.window.setHeight(265);
        this.window.setCanDragResize(true);
        this.window.centerInPage();

    }

}
