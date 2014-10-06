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
 *  * $$ACTIVEEON_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.Set;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.data.SortSpecifier;
import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class ThirdPartyCredentialsWindow implements SchedulerListeners.ThirdPartyCredentialsListener {

    private Window window;
    private final ListGrid credentialsGrid;

    public ThirdPartyCredentialsWindow(final SchedulerController controller) {

        controller.getEventDispatcher().setThirdPartyCredentialsListener(this);

        Label label = new Label(
            "Third-party credentials are key-value pairs saved on server side. They can be accessed from tasks.");
        label.setHeight(30);

        credentialsGrid = new ListGrid();
        ListGridField keyField = new ListGridField("key", "Key");
        ListGridField valueField = new ListGridField("value", "Credential");
        credentialsGrid.setFields(keyField, valueField);
        SortSpecifier[] sortedByKeys = { new SortSpecifier("key", SortDirection.ASCENDING) };
        credentialsGrid.setInitialSort(sortedByKeys);

        credentialsGrid.setMaxHeight(600);
        credentialsGrid.setMinHeight(350);
        credentialsGrid.setAutoFitData(Autofit.VERTICAL);
        credentialsGrid.setAutoFitMaxHeight(600);

        credentialsGrid.setCanRemoveRecords(true);
        credentialsGrid.addEditCompleteHandler(new EditCompleteHandler() {
            @Override
            public void onEditComplete(EditCompleteEvent editCompleteEvent) {
                Record record = editCompleteEvent.getOldRecord();
                String key = record.getAttribute("key");
                controller.removeThirdPartyCredential(key);
            }
        });

        controller.refreshThirdPartyCredentialsKeys();

        HLayout addCredential = new HLayout();
        addCredential.setAutoHeight();

        final DynamicForm form = new DynamicForm();
        form.setAutoHeight();
        form.setWidth(390);

        TextItem key = new TextItem("key", "Key");
        key.setRequired(true);
        key.setWidth(195);
        TextItem value = new PasswordItem("value", "Credential");
        value.setRequired(true);
        value.setWidth(195);

        form.setTitleOrientation(TitleOrientation.TOP);
        form.setFields(key, value);

        IButton addButton = new IButton("Add");
        addButton.setLayoutAlign(VerticalAlignment.BOTTOM);
        addButton.setWidth(50);
        addButton.setHeight(26);
        addButton.setMargin(2);
        addButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent clickEvent) {
                if (!form.validate())
                    return;

                String key = form.getValueAsString("key");
                String value = form.getValueAsString("value");
                controller.putThirdPartyCredential(key, value);
                form.clearValues();
            }
        });

        addCredential.setMembers(form, addButton);

        VLayout layout = new VLayout();
        layout.setMembersMargin(10);
        layout.setMargin(5);
        layout.setWidth(450);
        layout.setMembers(label, credentialsGrid, addCredential);

        this.window = new Window();
        this.window.setTitle("Manage Third-Party Credentials");
        this.window.setShowMinimizeButton(false);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(layout);
        this.window.setAutoSize(true);
        this.window.setDismissOnOutsideClick(true);
        this.window.setAutoCenter(true);
    }

    public void show() {
        this.window.show();
    }

    @Override
    public void keysUpdated(Set<String> thirdPartyCredentialsKeys) {
        ListGridRecord[] records = new ListGridRecord[thirdPartyCredentialsKeys.size()];
        int idx = 0;
        for (String key : thirdPartyCredentialsKeys) {
            ListGridRecord record = new ListGridRecord();
            record.setAttribute("key", key);
            record.setAttribute("value", "******");
            records[idx++] = record;
        }
        credentialsGrid.setRecords(records);
    }
}