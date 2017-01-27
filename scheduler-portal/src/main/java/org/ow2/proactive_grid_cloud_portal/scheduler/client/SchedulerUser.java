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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.io.Serializable;

import com.google.gwt.json.client.JSONObject;
import com.google.gwt.json.client.JSONString;


/**
 * User connected to the scheduler
 * <p>
 * This is a condensed version of info originally held
 * by org.ow2.proactive.scheduler.common.job.UserIdentification
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class SchedulerUser implements Serializable, Comparable<SchedulerUser> {

    private String hostName = null;

    private String username = null;

    private long connectionTime;

    private long lastSubmitTime;

    private int submitNumber;

    public SchedulerUser() {
    }

    /**
     * Default constructor
     * 
     * @param hostName hostname of the user's connection endpoint
     * @param userName name of the user
     * @param connectionTime time when the user joined the scheduler
     * @param lastSubmitTime last time the user submitted a job
     * @param submitNumber number of jobs this user submitted in this session
     */
    public SchedulerUser(String hostName, String userName, long connectionTime, long lastSubmitTime, int submitNumber) {
        this.hostName = hostName;
        this.username = userName;
        this.connectionTime = connectionTime;
        this.lastSubmitTime = lastSubmitTime;
        this.submitNumber = submitNumber;
    }

    public String getHostName() {
        return hostName;
    }

    public String getUsername() {
        return username;
    }

    public long getConnectionTime() {
        return connectionTime;
    }

    public long getLastSubmitTime() {
        return lastSubmitTime;
    }

    public int getSubmitNumber() {
        return submitNumber;
    }

    public String toString() {
        return "[" + hostName + ";" + username + ";" + connectionTime + ";" + lastSubmitTime + ";" + submitNumber + "]";
    }

    public int compareTo(SchedulerUser o) {
        if (connectionTime < o.connectionTime)
            return -1;
        else if (connectionTime > o.connectionTime)
            return 1;
        else
            return 0;
    }

    /**
     * @param jsonUser the JSON representation of an user
     * @return a POJO equivalent
     */
    public static SchedulerUser parseJson(JSONObject jsonUser) {
        String host = "";
        if (jsonUser.containsKey("hostName")) {
            JSONString str = jsonUser.get("hostName").isString();
            if (str != null)
                host = str.stringValue();
        }
        String user = jsonUser.get("username").isString().stringValue();
        long connTime = (long) jsonUser.get("connectionTime").isNumber().doubleValue();
        long subTime = (long) jsonUser.get("lastSubmitTime").isNumber().doubleValue();
        int subNum = (int) jsonUser.get("submitNumber").isNumber().doubleValue();

        return new SchedulerUser(host, user, connTime, subTime, subNum);
    }

}
