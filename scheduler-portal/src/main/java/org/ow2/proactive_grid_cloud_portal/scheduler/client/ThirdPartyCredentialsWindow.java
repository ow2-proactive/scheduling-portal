/*
 *  *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.EditCompleteEvent;
import com.smartgwt.client.widgets.grid.events.EditCompleteHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
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
        keyField.setWidth(142);
        ListGridField valueField = new ListGridField("value", "Credential");
        valueField.setWidth(360);
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

        final DynamicForm addEntryForm = new DynamicForm();
        addEntryForm.setAutoHeight();
        addEntryForm.setWidth(350);

        TextItem key = new TextItem("key", "Key");
        key.setRequired(true);
        key.setWidth(142);
        key.setHeight(26);
        final TextItem shortValue = new PasswordItem("shortValue", "Credential");
        shortValue.setRequired(true);
        shortValue.setWidth(208);
        shortValue.setHeight(26);
        final TextAreaItem longValue = new TextAreaItem("longValue", "Credential");
        longValue.setRequired(true);
        longValue.setWidth(208);
        longValue.setHeight(26);
        longValue.hide();

        addEntryForm.setTitleOrientation(TitleOrientation.TOP);
        addEntryForm.setFields(key, shortValue, longValue);

        final CheckboxItem longValueCheck =
                new CheckboxItem("longValueCheck", "Multiline credential");
        longValueCheck.setHeight(26);
        longValueCheck.setWidth(70);
        final DynamicForm longValueCheckForm = new DynamicForm();
        longValueCheckForm.setLayoutAlign(VerticalAlignment.BOTTOM);
        longValueCheckForm.setFields(longValueCheck);

        longValueCheck.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent changedEvent) {
                if (longValueCheck.getValueAsBoolean()) {
                    shortValue.hide();
                    longValue.show();
                } else {
                    shortValue.show();
                    longValue.hide();
                }
            }
        });

        LayoutSpacer space = new LayoutSpacer();
        space.setWidth(20);

        IButton addButton = new IButton("Add");
        addButton.setLayoutAlign(VerticalAlignment.BOTTOM);
        addButton.setHeight(26);
        addButton.setWidth(46);
        addButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent clickEvent) {
                if (!addEntryForm.validate())
                    return;

                String key = addEntryForm.getValueAsString("key");
                String value;
                if (longValueCheck.getValueAsBoolean()) {
                    value = addEntryForm.getValueAsString("longValue");
                } else {
                    value = addEntryForm.getValueAsString("shortValue");
                }
                controller.putThirdPartyCredential(key, value);
                addEntryForm.clearValues();
            }
        });

        addCredential.setMembers(
                addEntryForm, longValueCheckForm, space, addButton);

        VLayout layout = new VLayout();
        layout.setMembersMargin(10);
        layout.setMargin(5);
        layout.setWidth(550);
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