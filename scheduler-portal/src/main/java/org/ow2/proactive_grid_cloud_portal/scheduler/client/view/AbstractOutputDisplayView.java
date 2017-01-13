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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.JobOutput;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.AbstractSelectedTargetController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.AbstractSelectedTargetModel;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;


public abstract class AbstractOutputDisplayView<M extends AbstractSelectedTargetModel, C extends AbstractSelectedTargetController<M>>
        extends AbstractSelectedTargetView<M, C> {

    /** displays the job output */
    protected HTMLPane text = null;

    /** click to fetch/refetch */
    protected IButton refreshButton = null;

    /** display a message */
    protected Label label = null;

    protected String noOutputMessage;

    protected String refreshButtonLabel;

    protected String refreshButtonTooltip;

    public AbstractOutputDisplayView(C controller) {
        super(controller);
    }

    /**
     * Update the view to show that the output for the current job or task is not yet available.
     */
    protected void goToUnavailableOutputState() {
        this.text.setContents(" "); // whitespace otherwise it logs are empty, they won't be replaced in text panel
        this.text.hide();

        this.label.setContents(this.noOutputMessage);
        this.label.setIcon(null);
        this.label.show();
    }

    /**
     * Update the view to show that the output view is loading some content.
     */
    protected void goToLoadingState() {
        this.text.hide();
        this.label.setContents("Please wait...");
        this.label.setIcon("loading.gif");
        this.label.show();
    }

    protected void buildRefreshButton() {
        this.refreshButton = new IButton(this.refreshButtonLabel);
        this.refreshButton.setTooltip(this.refreshButtonTooltip);
        this.refreshButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                refreshButtonHandler();
            }
        });
    }

    /**
     * Called when the user click on the refresh button.
     */
    protected void refreshButtonHandler() {
        this.goToLoadingState();
        this.controller.refreshOutput();
    }

    protected VLayout buildOutputPane() {
        this.label = new Label();
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);
        this.label.hide();

        this.text = new HTMLPane();
        this.text.setHeight100();
        this.text.setWidth100();
        this.text.setShowEdges(true);
        this.text.hide();

        VLayout textLayout = new VLayout();
        textLayout.addMember(this.label);
        textLayout.addMember(this.text);
        textLayout.setWidth100();

        return textLayout;
    }

    /**
     * Show the given content in the html pane.
     * @param content the content to be displayed.
     */
    protected void showContent(String content) {
        this.label.hide();
        this.text.show();
        if (content.isEmpty()) {
            this.text.setContents(" ");
        } else {
            this.text.setContents(content);
        }
    }

    /**
     * Update view to show the control when a job or task has been selected.
     * @param output
     */
    protected void goToTargetSelectedState() {
        this.refreshButton.show();
        this.refreshButton.enable();

        this.targetSelect.show();
        this.targetSelect.enable();

    }

    protected void goToNoTargetState() {
        this.label.setContents(this.controller.getNoTargetLabelContent());
        this.label.setIcon(null);
        this.text.hide();
        this.label.show();

        this.refreshButton.disable();
    }
}
