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
package org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.serialization;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class ExportToCatalogConfirmWindow extends Window {

    private ExportToCatalogConfirmWindow() {
        setTitle("Export Node Source to Catalog");
        setShowMinimizeButton(false);
        setIsModal(true);
        setShowModalMask(true);
        setWidth(380);
        setHeight(160);
        setCanDragResize(false);
        setCanDragReposition(false);
        centerInPage();

        Label label = new Label("You are about to publish the Node Source \"java_job\" to this bucket.");
        label.setHeight(40);

        // somehow the name of the checkbox is misleading compared to what it
        // means (see name vs title). Thus we have to negate the value of the
        // checkbox when to preserve meaning
        final CheckboxItem force = new CheckboxItem("force", "Wait task completion on busy nodes");
        force.setValue(true);
        final DynamicForm form = new DynamicForm();
        form.setColWidths(25, "*");
        form.setItems(force);

        Canvas fill = new Canvas();
        fill.setHeight100();

        HLayout buttons = new HLayout();
        buttons.setMembersMargin(5);
        buttons.setAlign(Alignment.RIGHT);
        buttons.setHeight(25);

        IButton ok = new IButton("OK", event -> {
            //callback.run(!force.getValueAsBoolean());
            hide();
            destroy();
        });
        ok.setIcon(Images.instance.ok_16().getSafeUri().asString());
        IButton cancel = new IButton("Cancel", event -> {
            hide();
            destroy();
        });
        cancel.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        buttons.setMembers(ok, cancel);

        VLayout layout = new VLayout();
        layout.setMembersMargin(5);
        layout.setMargin(5);
        layout.setMembers(label, form, fill, buttons);

        addItem(layout);
        show();
    }

}
