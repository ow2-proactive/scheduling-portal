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
package org.ow2.proactive_grid_cloud_portal.scheduler.shared;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.google.gwt.user.client.rpc.IsSerializable;


/**
 * Contains a description of the graphical representation of a job
 * ie position of tasks and connections on the image
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class JobVisuMap implements IsSerializable {

    private Map<String, TaskPos> tasks = null;

    public JobVisuMap() {
        this.tasks = new HashMap<String, TaskPos>();
    }

    public void addTask(int x, int y, int w, int h, String name) {
        if (name == null || name.trim().length() == 0)
            throw new IllegalArgumentException();

        TaskPos t = new TaskPos(x, y, w, h);
        this.tasks.put(name, t);
    }

    public boolean hasTask(String name) {
        return this.tasks.containsKey(name);
    }

    public Set<String> getNames() {
        return this.tasks.keySet();
    }

    public int getTaskX(String name) {
        return this.tasks.get(name).x;
    }

    public int getTaskY(String name) {
        return this.tasks.get(name).y;
    }

    public int getTaskW(String name) {
        return this.tasks.get(name).w;
    }

    public int getTaskH(String name) {
        return this.tasks.get(name).h;
    }

}
