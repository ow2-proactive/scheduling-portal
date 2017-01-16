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

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TasksColumnsFactory;

import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.viewer.DetailFormatter;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;


public class InfoView<T> {

    /** label when no job is selected */
    protected Label label = null;

    /** widget shown when a job is selected */
    protected DetailViewer details = null;

    /** currently displayed job */
    protected T displayedItem = null;

    protected ColumnsFactory<T> factory;

    protected String emptyMessage;

    public InfoView(ColumnsFactory<T> factory, String emptyMessage) {
        this.factory = factory;
        this.emptyMessage = emptyMessage;
    }

    /**
     * @return the Widget to display, ready to be added in a container
     */
    public Layout build() {
        /* widget that has been returned as the root layout */
        Layout root = new Layout();
        root.setWidth100();
        root.setHeight100();

        this.label = new Label(this.emptyMessage);
        this.label.setWidth100();
        this.label.setAlign(Alignment.CENTER);

        this.details = new DetailViewer();
        this.details.setWidth100();
        this.details.setHeight100();
        this.details.setCanSelectText(true);
        this.details.hide();

        GridColumns[] lines = this.factory.getColumns();
        DetailViewerField[] fields = new DetailViewerField[lines.length];
        for (int i = 0; i < lines.length; i++) {
            fields[i] = new DetailViewerField(lines[i].getName(), lines[i].getTitle());

            if (lines[i] == TasksColumnsFactory.EXEC_DURATION_ATTR || lines[i] == JobsColumnsFactory.DURATION_ATTR) {
                fields[i].setDetailFormatter(new DetailFormatter() {
                    @Override
                    public String format(Object duration, Record record, DetailViewerField detailViewerField) {
                        if (duration != null) {
                            return Job.formatDuration(duration.toString());
                        } else {
                            return "";
                        }
                    }
                });
            }
        }

        this.details.setFields(fields);

        root.addMember(label);
        root.addMember(details);

        return root;
    }

    public void hideDetails() {
        this.details.hide();
        this.label.show();
        this.displayedItem = null;
    }

    public void displayItem() {
        DetailViewerRecord[] records = new DetailViewerRecord[1];
        records[0] = new DetailViewerRecord();
        this.factory.buildRecord(this.displayedItem, records[0]);

        this.details.setData(records);

        this.label.hide();
        this.details.show();
    }
}
