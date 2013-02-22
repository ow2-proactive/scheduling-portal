/*
 * ################################################################
 *
 * ProActive Parallel Suite(TM): The Java(TM) library for
 *    Parallel, Distributed, Multi-Core Computing for
 *    Enterprise Grids & Clouds
 *
 * Copyright (C) 1997-2011 INRIA/University of
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

import com.google.gwt.core.client.GWT;
import com.google.gwt.resources.client.ClientBundle;
import com.google.gwt.resources.client.ImageResource;


/**
 * Image Bundle to optimizes external resource handling
 * 
 * @author mschnoor
 *
 */
public interface SchedulerImages extends ClientBundle {

    public static final SchedulerImages instance = GWT.create(SchedulerImages.class);

    @Source("images/cancel_14.png")
    ImageResource cancel_14();

    @Source("images/finished_14.png")
    ImageResource finished_14();

    @Source("images/running_24.png")
    ImageResource running_24();

    @Source("images/scheduler_freeze_16.png")
    ImageResource scheduler_freeze_16();

    @Source("images/scheduler_kill_16.png")
    ImageResource scheduler_kill_16();

    @Source("images/scheduler_pause_16.png")
    ImageResource scheduler_pause_16();

    @Source("images/scheduler_resume_16.png")
    ImageResource scheduler_resume_16();

    @Source("images/scheduler_start_16.png")
    ImageResource scheduler_start_16();

    @Source("images/scheduler_stop_16.png")
    ImageResource scheduler_stop_16();

    @Source("images/job_kill_16.png")
    ImageResource job_kill_16();

    @Source("images/job_output_16.png")
    ImageResource job_output_16();

    @Source("images/job_pause_resume_16.png")
    ImageResource job_pause_resume_16();

    @Source("images/job_submit_16.png")
    ImageResource job_submit_16();

    @Source("images/monitoring_16.png")
    ImageResource monitoring_16();

    @Source("images/info_16.png")
    ImageResource info_16();

    @Source("images/nav_22.png")
    ImageResource nav_22();

    @Source("images/progressbar.png")
    ImageResource progressbar();

    @Source("images/output_16.png")
    ImageResource output_16();

    @Source("images/section_left_10.png")
    ImageResource section_left_10();

    @Source("images/section_right_10.png")
    ImageResource section_right_10();

    @Source("images/script_16.png")
    ImageResource script_16();

    @Source("images/visu_16.png")
    ImageResource visu_16();

    @Source("images/logo_32.png")
    ImageResource logo_32();

    @Source("images/logo_350.png")
    ImageResource logo_350();

    @Source("images/usage_16.png")
    ImageResource usage_16();

}
