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

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;


/**
 * Wraps the output of a job, as it was received at a given time
 * <p>
 * Each task output is stored in a map sorted by taskid
 * each entry is this map is a list of output lines
 * 
 * 
 * @author mschnoor
 *
 */
public class JobOutput {

	/** the output content, split by task and line
	 * key is timestamp for task finish time for sorting */
	private SortedMap<Long, List<String>> lines;

	/** id of the job */
	private int jobId;

	/**
	 * Default constructor
	 * 
	 * @param jobId id of the job representing this output
	 */
	public JobOutput(int jobId) {
		this.jobId = jobId;
		this.lines = new TreeMap<Long, List<String>>();
	}

	/**
	 * Update the output for a single task
	 * 
	 * @param finishedTime
	 * @param output output for this task
	 */
	public void update(long finishedTime, List<String> output) {
		List<String> tl = this.lines.get(finishedTime);
		if (tl != null) {
			tl.clear();
		} else {
			tl = new ArrayList<String>();
			this.lines.put(finishedTime, tl);
		}
		tl.addAll(output);
	}

	/**
	 * @return the output lines, exploded in a list, per task (finished time as key for sorting)
	 */
	public SortedMap<Long, List<String>> getLines() {
		return this.lines;
	}

	/**
	 * @return the id of the job associated with this output
	 */
	public int getJobId() {
		return this.jobId;
	}
}
