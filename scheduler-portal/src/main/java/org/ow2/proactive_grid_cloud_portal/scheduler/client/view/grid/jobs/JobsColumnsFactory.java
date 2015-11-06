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

package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Job;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.ColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.GridColumns;

import com.smartgwt.client.data.Record;

/**
 * A factory that give columns and records for jobs.
 */
public class JobsColumnsFactory implements ColumnsFactory<Job>{

    public static GridColumns ID_ATTR = new GridColumns("id", "id", 80, true, true);
    public static GridColumns STATE_ATTR = new GridColumns("state", "State", 100, true, false);
    public static GridColumns USER_ATTR = new GridColumns("user","User", 140, true, false);
    public static GridColumns PROGRESS_ATTR = new GridColumns("progress", "Progress", 100, true, false);
    public static GridColumns PRIORITY_ATTR = new GridColumns("priority", "Priority",150, true, false);
    public static GridColumns DURATION_ATTR = new GridColumns("duration", "Execution duration", 100, true, false);
    public static GridColumns NAME_ATTR = new GridColumns("name", "Name", -1, true, false);
    
    @Override
    public GridColumns[] getColumns() {
        return new GridColumns[]{ID_ATTR, STATE_ATTR, USER_ATTR, PROGRESS_ATTR, PRIORITY_ATTR, DURATION_ATTR, NAME_ATTR};
    }

    
    protected void buildCommonRecordAttributes(Job item, Record record){
    	record.setAttribute(ID_ATTR.getName(), item.getId());
        
        long duration = -1;
        if (item.getFinishTime() > 0 && item.getStartTime() > 0) {
            duration = item.getFinishTime() - item.getStartTime();
        }

        record.setAttribute(STATE_ATTR.getName(), item.getStatus().toString());
        record.setAttribute(USER_ATTR.getName(), item.getUser());
        record.setAttribute(PRIORITY_ATTR.getName(), item.getPriority().toString());
        record.setAttribute(NAME_ATTR.getName(), item.getName());
        record.setAttribute(DURATION_ATTR.getName(), duration);
    }
    
    
    @Override
    public void buildRecord(Job item, Record record) {
        buildCommonRecordAttributes(item, record);
        
        float progress = (float) item.getFinishedTasks() / (float) item.getTotalTasks();
        record.setAttribute(PROGRESS_ATTR.getName(), progress);
    }
    
}
