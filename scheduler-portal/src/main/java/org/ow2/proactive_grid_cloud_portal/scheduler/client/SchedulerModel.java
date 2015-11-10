/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2015 INRIA/University of
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
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.HashMap;
import java.util.List;

import org.ow2.proactive_grid_cloud_portal.common.client.Model;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.JobVisuMap;


/**
 * Holds local Data from the GUI that can directly be displayed by the views
 *
 *
 * @author mschnoor
 */
public abstract class SchedulerModel implements Model {

    /**
     * @return the current status of the scheduler
     */
    public abstract SchedulerStatus getSchedulerStatus();


    /**
     * @param jobId the id of a job
     * @return the relative path of the image on the server that may be used to construct the image url
     */
    public abstract String getJobImagePath(String jobId);

    /**
     * @param jobId the id of a job
     * @return if available, information about the size and position of tasks on the image described by
     * 		{@link #getJobImagePath(String)}; or null
     */
    public abstract JobVisuMap getJobVisuMap(String jobId);


    public abstract String getJobHtml(String jobId);

    public abstract void setJobHtml(String jobId, String curHtml);


    /**
     * @return the list of users connected to the scheduler
     */
    public abstract List<SchedulerUser> getSchedulerUsers();

    /**
     * @return the list of users having jobs in the scheduler
     */
    public abstract List<SchedulerUser> getSchedulerUsersWithJobs();

    /**
     * @return statistics for the logged user account
     */
    public abstract HashMap<String, String> getAccountStatistics();

    /**
     * @return statistics for the scheduler
     */
    public abstract HashMap<String, String> getSchedulerStatistics();

    public abstract List<JobUsage> getUsage();

}
