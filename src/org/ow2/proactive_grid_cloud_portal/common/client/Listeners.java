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
package org.ow2.proactive_grid_cloud_portal.common.client;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.Model.StatHistory;


public interface Listeners {

    public interface LogListener {

        /**
         * Issue a low priority message to be displayed by the listeners
         * <p>
         * It may be HTML formatted and can be directly inserted in the page
         * 
         * @param message a text or HTML log entry
         */
        public void logMessage(String message);

        /**
         * Issue an important message to be displayed by the listeners
         * <p>
         * It may be HTML formatted and can be directly inserted in the page
         * 
         * @param message a text or HTML log entry
         */
        public void logImportantMessage(String message);

        /**
         * Issue a critical message to be displayed by the listeners
         * <p>
         * It may be HTML formatted and can be directly inserted in the page
         * 
         * @param message a text or HTML log entry
         */
        public void logCriticalMessage(String message);

    }

    public interface StatsListener {

        /**
         * Statistics for the given value have been updated
         * 
         * @param key: source name; value: list of values
         */
        public void statsUpdated(Map<String, StatHistory> values);
    }

}
