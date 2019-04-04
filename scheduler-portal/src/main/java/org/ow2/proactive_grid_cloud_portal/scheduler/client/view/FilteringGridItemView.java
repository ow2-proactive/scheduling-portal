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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerImages;

import com.google.gwt.user.client.ui.Widget;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.MouseOutEvent;
import com.smartgwt.client.widgets.events.MouseOutHandler;
import com.smartgwt.client.widgets.events.MouseOverEvent;
import com.smartgwt.client.widgets.events.MouseOverHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.layout.VStack;


public abstract class FilteringGridItemView<T> extends AbstractGridItemsView<T> {

    /** jobs filtering */
    protected Layout filterPane = null;

    protected Layout contentPane = null;

    protected Img filterButtonLabel = null;

    /**
     * Construct and return the pane used to filter the job's datasource
     *
     * @return a widget for filtering the grid
     */
    public Layout buildFilterPane() {
        Label label = getLabel();

        this.filterPane = new VStack();
        this.filterPane.setBackgroundColor("#fafafa");
        this.filterPane.addMember(label);

        Layout gridFilterPane = new VLayout();
        gridFilterPane.setWidth100();
        gridFilterPane.setHeight(90);

        IButton ok = new IButton("Apply");
        ok.setHeight(20);
        ok.addClickHandler(event -> applyAction());
        IButton clear = new IButton("Clear");
        clear.setHeight(20);
        clear.addClickHandler(event -> clearAction());

        HLayout buttons = new HLayout();
        buttons.setWidth100();
        buttons.setAlign(Alignment.RIGHT);
        buttons.setHeight(20);
        buttons.setMargin(10);
        buttons.setMembersMargin(5);
        buttons.setMembers(clear, ok);

        gridFilterPane.addMember(getBuiltFilterPane());
        gridFilterPane.addMember(buttons);

        this.filterPane.setPadding(5);
        this.filterPane.setMembersMargin(10);
        this.filterPane.setOverflow(Overflow.AUTO);
        this.filterPane.addMember(gridFilterPane);
        this.filterPane.setBorder("1px solid #bfbfbf");
        this.filterPane.hide();

        return this.filterPane;
    }

    protected abstract Widget getBuiltFilterPane();

    protected abstract void clearAction();

    protected abstract void applyAction();

    protected abstract Label getLabel();

    protected void toggleFilterPane() {
        if (!filterPane.isVisible()) {
            filterPane.setWidth(490);
            filterButtonLabel.setSrc(SchedulerImages.instance.section_right_10().getSafeUri().asString());
            contentPane.showMember(filterPane);
        } else {
            filterButtonLabel.setSrc(SchedulerImages.instance.section_left_10().getSafeUri().asString());
            contentPane.hideMember(filterPane);
        }
    }

    protected Layout buildFilterButton() {
        final VLayout filterButton = new VLayout();
        filterButton.setBackgroundColor("#bfbfbf");
        filterButton.setAlign(VerticalAlignment.CENTER);
        filterButton.setWidth(12);
        filterButton.setHeight100();
        filterButton.setBorder("1px solid #bfbfbf");
        filterButtonLabel = new Img(SchedulerImages.instance.section_left_10().getSafeUri().asString(), 10, 13);
        filterButton.addMember(filterButtonLabel);
        filterButton.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                toggleFilterPane();
            }
        });
        filterButton.addMouseOverHandler(new MouseOverHandler() {
            public void onMouseOver(MouseOverEvent event) {
                filterButton.setBackgroundColor("#eee");
            }
        });
        filterButton.addMouseOutHandler(new MouseOutHandler() {
            public void onMouseOut(MouseOutEvent event) {
                filterButton.setBackgroundColor("#bfbfbf");
            }
        });
        return filterButton;
    }

    /**
     * Builds and return the top pane: the jobs list and filtering options
     *
     * <pre>
     * +- HLayout -----------------------------------------------+
     * |+- ListGrid -----------++--++- VLayout -----------------+|
     * || JobsView#build()     ||  || JobsView#buildFilterPane()||
     * ||                      ||>>|| hidden/shown upon click   ||
     * ||                      ||  || on the '>>' canvas        ||
     * |+----------------------++--++---------------------------+|
     * +---------------------------------------------------------+
     * </pre>
     */
    public Layout buildContent() {
        Layout contentGridLayout = super.buildContent();
        Layout filterButton = this.buildFilterButton();
        Layout filterPane = this.buildFilterPane();

        contentPane = new HLayout();
        contentPane.setMembers(contentGridLayout, filterButton, filterPane);

        return contentPane;
    }
}
