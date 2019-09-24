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

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class EditNodeTokens {

    private Window window;

    private VLayout vLayout;

    private List<TextItem> textItemList = new ArrayList<>();

    public EditNodeTokens(RMController controller, Node node) {
        VLayout rootLayout = new VLayout();
        rootLayout.setAlign(Alignment.CENTER);
        rootLayout.setMembersMargin(5);
        rootLayout.setPadding(10);
        rootLayout.setAlign(VerticalAlignment.TOP);

        vLayout = new VLayout();
        vLayout.setAlign(Alignment.CENTER);
        vLayout.setMembersMargin(10);
        vLayout.setPadding(10);
        vLayout.setHeight100();
        vLayout.setAlign(VerticalAlignment.TOP);

        HLayout buttonsLayout = new HLayout();
        buttonsLayout.setWidth100();
        buttonsLayout.setHeight(22);
        buttonsLayout.setMargin(5);
        buttonsLayout.setAlign(Alignment.RIGHT);
        buttonsLayout.setMembersMargin(5);

        window = new Window();
        window.setTitle("Edit Node Tokens");
        window.setShowMinimizeButton(false);
        window.setIsModal(true);
        window.setShowModalMask(true);
        window.setPadding(10);
        window.setMargin(10);
        window.setWidth(470);
        window.setHeight(400);
        window.setCanDragResize(true);
        window.setCanDragReposition(true);
        window.centerInPage();

        HLayout hNsname = new HLayout();
        Label leftnsName = new Label("Node Source name: ");
        leftnsName.setAlign(Alignment.RIGHT);
        leftnsName.setWidth(120);
        leftnsName.setHeight(14);
        Label nsName = new Label("<b>" + node.getSourceName() + "</b>");
        nsName.setHeight(14);
        nsName.setWidth(250);
        hNsname.setHeight(14);
        hNsname.setMembersMargin(10);
        hNsname.setMembers(leftnsName, nsName);

        HLayout hHostname = new HLayout();
        Label lefthostName = new Label("Host name: ");
        lefthostName.setAlign(Alignment.RIGHT);
        lefthostName.setWidth(120);
        lefthostName.setHeight(14);
        Label hostName = new Label("<b>" + node.getHostName() + "</b>");
        hostName.setHeight(14);
        hostName.setWidth(250);
        hHostname.setHeight(14);
        hHostname.setMembersMargin(10);
        hHostname.setMembers(lefthostName, hostName);

        HLayout hNode = new HLayout();
        Label leftnodeUrl = new Label("Node url: ");
        leftnodeUrl.setAlign(Alignment.RIGHT);
        leftnodeUrl.setWidth(120);
        leftnodeUrl.setHeight(14);
        Label nodeUrl = new Label("<b>" + node.getNodeUrl() + "</b>");
        nodeUrl.setHeight(14);
        nodeUrl.setWidth(250);
        hNode.setHeight(14);
        hNode.setMembersMargin(10);
        hNode.setMembers(leftnodeUrl, nodeUrl);

        vLayout.addMember(hNsname);
        vLayout.addMember(hHostname);
        vLayout.addMember(hNode);

        node.getTokens().forEach(this::createHLayoutForToken);

        IButton addMoreButton = new IButton("Add token");
        addMoreButton.setMargin(2);
        addMoreButton.addClickHandler(h -> {
            createHLayoutForToken("");
        });

        vLayout.addMember(addMoreButton);

        rootLayout.addMember(vLayout);

        rootLayout.addMember(buttonsLayout);

        IButton cancelButton = new IButton("Cancel");
        cancelButton.addClickHandler(h -> window.hide());

        IButton applyButton = new IButton("Apply");
        applyButton.addClickHandler(h -> {
            window.hide();
            List<String> tokens = textItemList.stream()
                                              .map(TextItem::getValueAsString)
                                              .filter(x -> !x.isEmpty())
                                              .collect(Collectors.toList());

            controller.setNodeTokens(node.getNodeUrl(), tokens);
        });

        buttonsLayout.addMember(cancelButton);
        buttonsLayout.addMember(applyButton);

        window.addItem(rootLayout);

    }

    private void createHLayoutForToken(String token) {
        HLayout hLayout = new HLayout();
        hLayout.setWidth100();
        hLayout.setHeight(20);

        DynamicForm dynamicForm = new DynamicForm();
        TextItem textItem = new TextItem();
        textItem.setValue(token);
        textItem.setHeight(20);
        textItem.setWidth(360);
        textItem.setShowTitle(false);
        dynamicForm.setFields(textItem);
        dynamicForm.setPadding(0);
        dynamicForm.setWidth("90%");
        hLayout.addMember(dynamicForm);

        textItemList.add(textItem);

        Label removeLabel = new Label();
        removeLabel.setIcon(RMImages.instance.kill().getSafeUri().asString());
        removeLabel.setWidth(20);
        removeLabel.setHeight(20);
        removeLabel.addClickHandler(t -> {
            textItemList.remove(textItem);
            vLayout.removeMember(hLayout);
        });

        hLayout.addMember(removeLabel);
        hLayout.setMembersMargin(10);
        vLayout.addMember(hLayout, textItemList.size() + 2);
    }

    public void show() {
        window.show();
    }

    public void hide() {
        window.hide();
    }

}
