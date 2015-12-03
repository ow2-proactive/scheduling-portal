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

import com.smartgwt.client.data.RelativeDate;
import com.smartgwt.client.types.RelativeDateRangePosition;
import com.smartgwt.client.util.DateUtil;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RelativeDateItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricNavigationController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.TasksCentricNavigationModel;

import java.util.Date;

public class TasksCentricNavigationView extends TasksNavigationView {

    public TasksCentricNavigationView(TasksCentricNavigationController controller) {
        super(controller);
    }

    @Override
    public Layout build() {
        Layout layout = super.build();

        RelativeDateItem fromDateItem = new RelativeDateItem("fromDate", "From");
        fromDateItem.setPrompt("Filter tasks based on startTime or finishedTime (i.e. return tasks having startTime or finishedTime value greater or equals than selected value)");
        fromDateItem.setValue("$yesterday");
        fromDateItem.setRangePosition(RelativeDateRangePosition.START);
        fromDateItem.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                fromDateChangedHandler(event);
            }
        });

        RelativeDateItem toDateItem = new RelativeDateItem("toDate", "To");
        toDateItem.setPrompt("Filter tasks based on startTime or finishedTime (i.e. return tasks having startTime or finishedTime value lower or equals than selected value)");
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
        form.setItems(fromDateItem, toDateItem);
        form.setStyleName("form");
        layout.addMember(form);
        
        return layout;
    }

    private long getTime(RelativeDateItem relativeDateItem) {
        // Cannot use relativeDateItem#getRelativeDate() otherwise superdev mode freeze
        return DateUtil.getAbsoluteDate(
                new RelativeDate((String) relativeDateItem.getValue())).getTime();
    }

    protected void fromDateChangedHandler(ChangedEvent event){
        long time = getTime(event);
        ((TasksCentricNavigationController) this.controller).changeFromDate(time);
    }

    protected void toDateChangedHandler(ChangedEvent event){
        long time = getTime(event);
        ((TasksCentricNavigationController) this.controller).changeToDate(time);
    }

    private long getTime(ChangedEvent event) {
        Date value = (Date) event.getValue();
        return value.getTime();
    }

    @Override
    public void jobSelected(Job job) {
    }


    @Override
    public void jobUnselected() {
    }

}
