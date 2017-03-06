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
package org.ow2.proactive_grid_cloud_portal.scheduler.server;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;

import org.junit.Test;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.PortalConfig;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;


public class SchedulerServiceImplTest {

    private SchedulerServiceImpl service;

    public void setUp() throws ServletException {
        service = new SchedulerServiceImpl();

        // mock servlet context
        ServletContext servletContext = mock(ServletContext.class);
        when(servletContext.getRealPath("scheduler.conf")).thenReturn(resourceAsFilePath("/scheduler.conf"));
        when(servletContext.getRealPath("scheduler-portal.conf")).thenReturn(resourceAsFilePath("/scheduler-portal.conf"));

        service.init(mockServletContext(servletContext));
    }

    @Test
    public void testLoadingFromConfigurationFile() throws Exception {
        setUp();
        assertEquals("test", SchedulerConfig.get().getRestUrl());
        assertEquals("[{ \"title\": \"start at\", \"information\": { \"type\": \"generic-information\", \"key\": \"START_AT\"}, \"hidden-default\": false }]",
                     PortalConfig.get().getProperties().get("execution-list-extra-columns"));
    }

    @Test
    public void testPrecedenceBetweenSystemPropertiesAndConfigurationFile() throws ServletException {
        System.setProperty(SchedulerConfig.REST_URL, "unknown");
        System.setProperty(SchedulerConfig.VERSION, "x.x.x");

        setUp();

        assertEquals("unknown", SchedulerConfig.get().getRestUrl());
        assertEquals("x.x.x", SchedulerConfig.get().getVersion());
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
