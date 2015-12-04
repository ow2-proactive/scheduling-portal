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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */

package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Date;

import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.LayoutSpacer;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricNavigationController;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RelativeDateItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;

public class TasksCentricNavigationView extends TasksNavigationView{

    public TasksCentricNavigationView(TasksCentricNavigationController controller) {
        super(controller);
    }
    Canvas datesCanvas;
    RelativeDateItem fromDateItem;
    RelativeDateItem toDateItem;

    @Override
    public Layout build() {
        Layout layout = super.build();

        fromDateItem = new RelativeDateItem("fromDate", "from");
        fromDateItem.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                fromDateChangedHandler(event);
            }
        });
        toDateItem = new RelativeDateItem("toDate", "to");
        toDateItem.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                toDateChangedHandler(event);
            }
        });

        DynamicForm form = new DynamicForm();
        form.setNumCols(4);
        form.setItems(fromDateItem, toDateItem);
        //form.setStyleName("form");
        layout.addMember(form);

        datesCanvas = fromDateItem.getContainerWidget();
        // The far right of the canvas border isn't visible
        LayoutSpacer spacer = new LayoutSpacer(5,datesCanvas.getHeight());
        layout.addMember(spacer);

        return layout;
    }

    protected void fromDateChangedHandler(ChangedEvent event){
        Date value = (Date) event.getValue();
        long fromDate = value.getTime();
        if (dateRangeIsValid()) {
            resetDatesCanvasBGColor();
            ((TasksCentricNavigationController) this.controller).changeFromDate(fromDate);
        }
        else {
            highlightDates();
        }
    }

    protected void toDateChangedHandler(ChangedEvent event){
        Date value = (Date) event.getValue();
        long toDate = value.getTime();
        if (dateRangeIsValid()) {
            resetDatesCanvasBGColor();
            ((TasksCentricNavigationController) this.controller).changeToDate(toDate);
        }
        else {
            highlightDates();
        }
    }

    private boolean dateRangeIsValid() {
        long fromDate = RelativeDateItem.getAbsoluteDate(fromDateItem.getRelativeDate()).getTime();
        long toDate = RelativeDateItem.getAbsoluteDate(toDateItem.getRelativeDate()).getTime();
        return fromDate < toDate;
    }

    private void resetDatesCanvasBGColor() {
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
