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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

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

    private RMController controller;

    public SettingsWindow(RMController controller) {
        this.controller = controller;
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
                                                                        "Node Sources refresh rate (ms)");
        refreshTime.setRequired(true);
        IntegerRangeValidator refreshValidator = new IntegerRangeValidator();
        refreshValidator.setMin(500);
        refreshValidator.setMax(60000);
        refreshTime.setValidators(new IsIntegerValidator(), refreshValidator);

        final DataSource ds = new DataSource();
        ds.setFields(refreshTime);

        final DynamicForm form = new DynamicForm();
        form.setDataSource(ds);
        form.setColWidths("120", "230");
        form.setWidth(350);
        form.setMargin(10);
        form.setValue("refreshTime", RMConfig.get().getClientRefreshTime());

        final IButton applyButton = new IButton("Ok");
        applyButton.setIcon(Images.instance.ok_16().getSafeUri().asString());
        applyButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                if (!form.validate())
                    return;

                String refreshTime = form.getValueAsString("refreshTime");

                controller.setUserSettings(refreshTime);
                window.hide();
            }
        });

        final IButton resetButton = new IButton("Reset");
        resetButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                RMConfig.get().reload();
                int refresh = RMConfig.get().getClientRefreshTime();

                form.setValue("refreshTime", refresh);
                controller.setUserSettings("" + refresh);
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
        this.window.setWidth(380);
        this.window.setHeight(190);
        this.window.setCanDragResize(true);
        this.window.centerInPage();

    }

}
