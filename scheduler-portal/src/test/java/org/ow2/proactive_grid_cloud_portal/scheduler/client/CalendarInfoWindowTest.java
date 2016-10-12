package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.CalendarInfoWindow.CalendarInfoContentBuilder;

import com.google.gwtmockito.GwtMockitoTestRunner;


@RunWith(GwtMockitoTestRunner.class)
public class CalendarInfoWindowTest {

    private CalendarInfoContentBuilder builder;

    private static final String ics = "private-121345679.ics/";

    private static final String notIcs = "<!DOCTYPE html><html lang=\"en\"><head>    <meta charset=\"utf-8\">";

    private String withPrivateUrlPattern = ".*ProActive Scheduling & Orchestration.*" + ics +
        ".*Regenerate.*Delete.*See calendar Documentation and Installation.*";

    private String withoutPrivateUrlPattern = ".*ProActive Scheduling & Orchestration.*Create.*See calendar Documentation and Installation.*";

    private String serviceNotAvailablePattern = ".*ProActive Scheduling & Orchestration.*Oops, Calendar Service is not available.*administrator.*";

    @Before
    public void setUp() throws Exception {
        CalendarInfoWindow window = new CalendarInfoWindow("7.18.0");
        builder = window.new CalendarInfoContentBuilder("7.18.0");
    }

    @Test
    public void testTextBuildWithPrivateUrl() {

        String content = builder.buildContentString(ics);

        assertTrue(content.matches(withPrivateUrlPattern));
    }

    @Test
    public void testTextBuildWithoutPrivateUrl() {
        String icsName = "";

        String content = builder.buildContentString(icsName);

        assertTrue(content.matches(withoutPrivateUrlPattern));
    }

    @Test
    public void testTextBuildWhenCalendarServiceNotAvailable() {

        String content = builder.buildContentString(notIcs);

        assertTrue(content.matches(serviceNotAvailablePattern));
    }

}
