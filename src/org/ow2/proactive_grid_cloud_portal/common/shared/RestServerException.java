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
 * An exception sent from the GWT server side caused by a REST server failure
 * 
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class RestServerException extends Exception implements Serializable {

	private int status;
	private String response;

	public RestServerException() {
	}

	public RestServerException(String message) {
		super(message);
	}

	public RestServerException(int status, String message) {
		super(message);
		this.status = status;
	}

	public RestServerException(int status, String response, String message) {
		super(message);
		this.status = status;
		this.response = response;
	}

	public RestServerException(Throwable cause) {
		super(cause);
	}

	public RestServerException(String message, Throwable cause) {
		super(message, cause);
	}

	public int getStatus() {
		return status;
	}

	public String getResponse() {
		return response;
	}
}
