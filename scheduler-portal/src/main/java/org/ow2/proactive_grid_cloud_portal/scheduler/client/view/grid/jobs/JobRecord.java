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

import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A list grid record that embeds a job.
 * @author The activeeon team.
 *
 */
public class JobRecord extends ListGridRecord {

    /**
     * the attribute to store the job.
     */
    private static final String JOB_ATTR = "job";

    public JobRecord(Job j) {
        setAttribute(JOB_ATTR, j);  
    }

    /**
     * Gets the job stored in a job record.
     * @param record the record that contains the job.
     * @return the stored job.
     */
    public static Job getJob(ListGridRecord record){
        return (Job) record.getAttributeAsObject(JOB_ATTR);
    }

}
