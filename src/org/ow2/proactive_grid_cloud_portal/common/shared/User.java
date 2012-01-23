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
package org.ow2.proactive_grid_cloud_portal.common.shared;

import java.io.Serializable;


/**
 * View on the GWT server of an user currently logged 
 * to the remote REST server
 *  
 * 
 */
@SuppressWarnings("serial")
public class User implements Serializable {

	private String username;
	private String sessionId;

	/**
	 * Empty no-arg constructor
	 */
	public User() {
	}

	/**
	 * Creates a new instance for the User.
	 * @param username the username
	 */
	public User(String username) {
		this.username = username;
	}

	/**
	 * Setter for the user name.
	 * @param username the user name that is going to be set
	 */
	public void setUsername(String username) {
		this.username = username;
	}

	/**
	 * Getter for the user name.
	 * @return the user name
	 */
	public String getUsername() {
		return username;
	}

	/**
	 * Setter of the user's session id.
	 * @param sessionId
	 */
	public void setSessionId(String sessionId) {
		this.sessionId = sessionId;
	}

	/**
	 * Getter for the user's session id.
	 * @return the user's session id
	 */
	public String getSessionID() {
		return sessionId;
	}

	public String toString() {
		return "[username = " + this.username + ", " + "sessionId = " + this.sessionId + "]";
	}

}
