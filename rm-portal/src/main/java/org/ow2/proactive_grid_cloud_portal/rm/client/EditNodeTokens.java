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

import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.NodeSource.Host.Node;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class EditNodeTokens {

    private Window window;

    private Node node;

    private VLayout vLayout;

    private HLayout buttonsLayout;

    private IButton addMoreButton;

    private List<TextItem> textItemList = new ArrayList<>();

    public EditNodeTokens(RMController controller, Node node) {
        this.node = node;

        vLayout = new VLayout();
        vLayout.setAlign(Alignment.CENTER);
        vLayout.setMembersMargin(5);

        buttonsLayout = new HLayout();
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
        window.addItem(vLayout);
        window.setWidth(300);
        window.setHeight(340);
        window.setCanDragResize(true);
        window.setCanDragReposition(true);
        window.centerInPage();

        Label nsName = new Label("Node Source name: <b>" + node.getSourceName() + "</b>");
        nsName.setHeight(14);
        Label hostName = new Label("Host name: <b>" + node.getHostName() + "</b>");
        hostName.setHeight(14);
        Label nodeUrl = new Label("Node url: <b>" + node.getNodeUrl() + "</b>");
        nodeUrl.setHeight(14);
        vLayout.addMember(nsName);
        vLayout.addMember(hostName);
        vLayout.addMember(nodeUrl);

        node.getTokens().forEach(this::createHLayoutForToken);

        addMoreButton = new IButton("Add token");
        addMoreButton.addClickHandler(h -> {
            createHLayoutForToken("");
        });

        vLayout.addMember(addMoreButton);

        vLayout.addMember(buttonsLayout);

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

    }

    private void createHLayoutForToken(String token) {
        HLayout hLayout = new HLayout();
        hLayout.setWidth100();

        DynamicForm dynamicForm = new DynamicForm();
        TextItem textItem = new TextItem();
        textItem.setValue(token);
        textItem.setShowTitle(false);
        textItem.setWidth(200);
        dynamicForm.setFields(textItem);
        dynamicForm.setWidth(200);
        hLayout.addMember(dynamicForm);

        textItemList.add(textItem);

        Label removeLabel = new Label();
        removeLabel.setIcon(RMImages.instance.kill().getSafeUri().asString());
        removeLabel.setWidth(12);
        removeLabel.setHeight(12);
        removeLabel.addClickHandler(t -> {
            textItemList.remove(textItem);
            vLayout.removeMember(hLayout);
        });

        hLayout.addMember(removeLabel);
        hLayout.setMembersMargin(10);
        hLayout.setMargin(10);
        vLayout.addMember(hLayout, textItemList.size() + 2);
    }

    public void show() {
        window.show();
    }

    public void hide() {
        window.hide();
    }

}
