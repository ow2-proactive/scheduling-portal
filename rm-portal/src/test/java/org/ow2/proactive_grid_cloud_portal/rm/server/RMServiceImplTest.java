package org.ow2.proactive_grid_cloud_portal.rm.server;

import java.io.File;
import java.net.URISyntaxException;
import java.net.URL;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;

import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;


public class RMServiceImplTest {

    @Test
    public void rm_config_is_loaded() throws Exception {
        assertEquals("http://localhost:8080/proactive_grid_cloud_portal/rest",
          RMConfig.get().getRestUrl());
        System.setProperty(RMConfig.VERSION, "a_version");

        RMServiceImpl service = new RMServiceImpl();

        // mock servlet context
        ServletContext servletContext = mock(ServletContext.class);
        when(servletContext.getRealPath("rm.conf")).thenReturn(resourceAsFilePath("/rm.conf"));

        service.init(mockServletContext(servletContext));

        assertEquals("a_test_url", RMConfig.get().getRestUrl());
        assertEquals("a_version", RMConfig.get().getVersion());
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