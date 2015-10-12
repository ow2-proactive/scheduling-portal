/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;

import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


public class SchedulerServiceImplTest {

    @Test
    public void scheduler_config_is_loaded() throws Exception {
        assertEquals("http://localhost:8080/rest",
          SchedulerConfig.get().getRestUrl());
        System.setProperty(SchedulerConfig.VERSION, "a_version");

        SchedulerServiceImpl service = new SchedulerServiceImpl();

        // mock servlet context
        ServletContext servletContext = mock(ServletContext.class);
        when(servletContext.getRealPath("scheduler.conf")).thenReturn(resourceAsFilePath("/scheduler.conf"));

        service.init(mockServletContext(servletContext));

        assertEquals("a_test_url", SchedulerConfig.get().getRestUrl());
        assertEquals("a_version", SchedulerConfig.get().getVersion());
    }

    private String resourceAsFilePath(String resourcePath) {
        URL resource = getClass().getResource(resourcePath);
        try {
            return new File(resource.toURI()).getPath();
        } catch (URISyntaxException e) {
            return resource.getPath();
        }
    }

    private ServletConfig mockServletContext(ServletContext servletContext) {
        ServletConfig servletConfig = mock(ServletConfig.class);
        when(servletConfig.getServletContext()).thenReturn(servletContext);
        return servletConfig;
    }

}
