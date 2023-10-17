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
package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import static org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.KeyValueColumnsFactory.KEY_ATTR;

import java.util.Map;
import java.util.Set;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.KeyValueColumnsFactory;

import com.smartgwt.client.types.Autofit;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VStack;


/**
 * Title + key/value grid
 * @author ActiveEon Team
 * @since Mar 13, 2017
 */
public class KeyValueGrid extends VStack {

    /** Grid label */
    private Label keyValueGridLabel = null;

    /** Grid displaying key-value data */
    private ListGrid keyValueGrid = null;

    /** Key-value column factory */
    private KeyValueColumnsFactory keyValueColumnsFactory;

    /**
     * Contructor
     * @param gridLabel the label displayed above the grid
     */
    public KeyValueGrid(String gridLabel) {

        keyValueColumnsFactory = new KeyValueColumnsFactory();

        keyValueGridLabel = new Label(gridLabel);
        keyValueGridLabel.setValign(VerticalAlignment.BOTTOM);
        keyValueGridLabel.setAutoHeight();

        keyValueGrid = new ListGrid();
        keyValueGrid.setWidth100();
        keyValueGrid.setLeaveScrollbarGap(false);
        keyValueGrid.setAutoFitData(Autofit.VERTICAL);
        keyValueGrid.setShowAllRecords(true);
        keyValueGrid.setOverflow(Overflow.VISIBLE);
        keyValueGrid.setHeight(1);
        keyValueGrid.setBodyOverflow(Overflow.VISIBLE);
        keyValueGrid.setShowHeader(false);

        GridColumns[] columns = keyValueColumnsFactory.getColumns();
        ListGridField[] fields = new ListGridField[columns.length];
        for (int i = 0; i < columns.length; i++) {
            GridColumns column = columns[i];
            fields[i] = new ListGridField(column.getName(), column.getTitle());
        }

        keyValueGrid.setFields(fields);

        //VStack methods
        setWidth100();

        addMember(keyValueGridLabel);
        addMember(keyValueGrid);

        hide();
    }

    public void setLabel(String label) {
        keyValueGridLabel.setContents("<b>" + label + "</b>");
    }

    public void showTopMargin() {
        keyValueGridLabel.setHeight(30);
    }

    /**
     * Display data as grid rows
     * @param entries the list of key-value data to display
     */
    public void buildEntries(Map<String, String> entries) {
        Set<Map.Entry<String, String>> entrySet = entries.entrySet();
        ListGridRecord[] records = new ListGridRecord[entrySet.size()];

        int index = 0;
        for (Map.Entry<String, String> entry : entrySet) {
            ListGridRecord record = new ListGridRecord();
            this.keyValueColumnsFactory.buildRecord(entry, record);
            records[index] = record;
            index++;
        }
        keyValueGrid.setData(records);
    }

    /**
     * Display job variables as grid rows. Colors the row if the variable is advanced or hidden.
     * @param jobDetailedVariables Detailed job variables
     * @param entries the list of key-value data to display
     */
    public void buildEntries(Map<String, Map<String, String>> jobDetailedVariables, Map<String, String> entries) {
        Set<Map.Entry<String, String>> entrySet = entries.entrySet();
        ListGridRecord[] records = new ListGridRecord[entrySet.size()];

        int index = 0;
        for (Map.Entry<String, String> entry : entrySet) {
            ListGridRecord record = new ListGridRecord();
            this.keyValueColumnsFactory.buildRecord(entry, record);
            applyAdvancedOrHiddenColor(jobDetailedVariables, record);
            records[index] = record;
            index++;
        }
        keyValueGrid.setData(records);
    }

    public void applyAdvancedOrHiddenColor(Map<String, Map<String, String>> jobVarDetails, ListGridRecord record) {
        boolean isAdvancedVar = Boolean.parseBoolean(jobVarDetails.get(record.getAttribute(KEY_ATTR.getName()))
                                                                  .get("advanced"));
        boolean isHiddenVar = Boolean.parseBoolean(jobVarDetails.get(record.getAttribute(KEY_ATTR.getName()))
                                                                .get("hidden"));
        if (isAdvancedVar) {
            record.setCustomStyle("color-orange");
        } else if (isHiddenVar) {
            record.setCustomStyle("color-light-grey");
        }
        keyValueGrid.redraw();
    }

    /**
     * Sets the variable description as tool tip text on the corresponding listGrid field
     * @param detailedVariables of the job
     */
    public void setVariableDescription(Map<String, Map<String, String>> detailedVariables) {
        for (int i = 0; i < keyValueGrid.getFields().length; i++) {
            ListGridField lg = keyValueGrid.getField(i);
            lg.setShowHover(true);
            lg.setHoverCustomizer(new HoverCustomizer() {
                public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                    String variableName = record.getAttribute(KEY_ATTR.getName());
                    String description = detailedVariables.get(variableName).get("description");
                    return description != null && !description.isEmpty() ? "<div class='tooltipStyle'>" + description +
                                                                           "</div>"
                                                                         : null;
                }
            });
        }
    }
}
