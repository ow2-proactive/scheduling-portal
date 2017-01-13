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

import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricNavigationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;

import com.smartgwt.client.data.RelativeDate;
import com.smartgwt.client.types.RelativeDateRangePosition;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RelativeDateItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.LayoutSpacer;


public class TasksCentricNavigationView extends TasksNavigationView {

    public TasksCentricNavigationView(TasksCentricNavigationController controller) {
        super(controller);
    }

    private Canvas datesCanvas;

    private RelativeDateItem fromDateItem;

    private RelativeDateItem toDateItem;

    @Override
    public Layout build() {
        Layout layout = super.build();

        fromDateItem = new RelativeDateItem("fromDate", "From");
        fromDateItem.setValue("$yesterday");
        fromDateItem.setRangePosition(RelativeDateRangePosition.START);
        fromDateItem.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                fromDateChangedHandler(event);
            }
        });

        toDateItem = new RelativeDateItem("toDate", "To");
        toDateItem.setCellHeight(32);
        toDateItem.setValue("$tomorrow");
        toDateItem.setRangePosition(RelativeDateRangePosition.END);
        toDateItem.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                toDateChangedHandler(event);
            }
        });

        TasksCentricNavigationModel navigationModel = (TasksCentricNavigationModel) controller.getModel();
        navigationModel.setFromDate(getTime(fromDateItem));
        navigationModel.setToDate(getTime(toDateItem));

        DynamicForm form = new DynamicForm();
        form.setNumCols(4);
        form.setLayoutAlign(VerticalAlignment.CENTER);
        form.setHeight(32);
        form.setItems(fromDateItem, toDateItem);
        form.setAutoWidth();

        Img imgButton = new Img();
        imgButton.setSize(16);
        imgButton.setLayoutAlign(VerticalAlignment.CENTER);
        imgButton.setPrompt("Filtered tasks have at least one event (scheduled, started or finished) in this period of time.");
        imgButton.setSrc(Images.instance.about_16().getSafeUri().asString());

        layout.setAlign(VerticalAlignment.CENTER);
        layout.addMember(new LayoutSpacer());
        layout.addMember(form);
        layout.addMember(new LayoutSpacer(4, 32));
        layout.addMember(imgButton);
        layout.addMember(new LayoutSpacer(12, 32));

        datesCanvas = fromDateItem.getContainerWidget();

        return layout;
    }

    private long getTime(RelativeDateItem relativeDateItem) {
        Object value = relativeDateItem.getValue();

        if (value instanceof String) {
            // Cannot use relativeDateItem#getRelativeDate() otherwise web page is blank
            return DateUtil.getAbsoluteDate(new RelativeDate((String) relativeDateItem.getValue())).getTime();
        } else if (value instanceof Date) {
            return ((Date) value).getTime();
        } else {
            return -1;
        }
    }

    private long getTime(ChangedEvent event) {
        Date value = (Date) event.getValue();
        return value.getTime();
    }

    protected void fromDateChangedHandler(ChangedEvent event) {
        if (dateRangeIsValid()) {
            resetDatesCanvasBackgroundColor();
            ((TasksCentricNavigationController) this.controller).changeFromDate(getTime(event));
        } else {
            highlightDates();
        }
    }

    protected void toDateChangedHandler(ChangedEvent event) {
        if (dateRangeIsValid()) {
            resetDatesCanvasBackgroundColor();
            ((TasksCentricNavigationController) this.controller).changeToDate(getTime(event));
        } else {
            highlightDates();
        }
    }

    private boolean dateRangeIsValid() {
        long fromDate = getTime(fromDateItem);
        long toDate = getTime(toDateItem);
        return fromDate < toDate;
    }

    private void resetDatesCanvasBackgroundColor() {
        datesCanvas.setBorder("");
    }

    private void highlightDates() {
        datesCanvas.setBorder("2px solid red");
    }

    @Override
    public void jobSelected(Job job) {
    }

    @Override
    public void jobUnselected() {
    }

}
