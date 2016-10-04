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
 *  * $$ACTIVEEON_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.ImagesUnbundled;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


public class CalendarInfoWindow {

    private final Window window = new Window();

    private final HLayout buttons = new HLayout();

    private final HLayout pane = new HLayout();

    private final IButton ok = new IButton("Close");

    private final IButton regenerateBt = new IButton("Regenerate");

    private final IButton deleteBt = new IButton("Delete");

    private final IButton createBt = new IButton("Create URL");

    private final Img img = new Img(ImagesUnbundled.ABOUT_115, 115, 130);

    private final VLayout root = new VLayout();

    private final HTMLPane text = new HTMLPane();

    /**
     * Default constructor
     */
    public CalendarInfoWindow() {
        this.build();
    }

    private void build() {
        pane.setWidth100();
        pane.setHeight(350);
        pane.setBackgroundColor("#ffffff");

        buttons.setAlign(Alignment.RIGHT);
        buttons.setWidth100();
        buttons.setHeight(20);
        buttons.setMargin(10);

        text.setWidth100();
        text.setStyleName("paddingLeftAndRight");

        root.setBackgroundColor("#dddddd");
        root.setWidth100();
        root.setHeight100();

        window.setTitle("Calendar Integration");
        window.setShowMinimizeButton(false);
        window.setShowShadow(true);
        window.setIsModal(true);
        window.setShowModalMask(true);
        window.setWidth(729);
        window.setHeight(425);
        window.centerInPage();
        window.setCanDragReposition(false);

        ok.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        ok.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                CalendarInfoWindow.this.hide();
            }
        });

        buttons.addMember(ok);

        regenerateBt.setIcon(Images.instance.ok_16().getSafeUri().asString());
        regenerateBt.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                loadWindowsContent(RequestBuilder.PUT);
            }
        });

        deleteBt.setIcon(Images.instance.clear_16().getSafeUri().asString());
        deleteBt.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                loadWindowsContent(RequestBuilder.DELETE);
            }
        });

        createBt.setIcon(Images.instance.ok_16().getSafeUri().asString());
        createBt.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                loadWindowsContent(RequestBuilder.POST);
            }
        });

        loadWindowsContent(RequestBuilder.GET);

    }

    /**
     * Bring up the modal window
     */
    public void show() {
        this.window.show();
    }

    /**
     * Hide the modal window
     */
    public void hide() {
        this.window.hide();
    }

    /**
     * Destroy the window; you may null your references to this
     */
    public void destroy() {
        this.hide();
        this.window.destroy();
    }

    private final static String CALENDAR_CONTEXT = "<h1>ProActive Scheduling & Orchestration: integration with Calendars </h1>" +
        "<font size=\"3\">Secured Calendar URL with authentication (Apple Calendaar, Thunderbird): <br><br><i>" +
        "http://" + com.google.gwt.user.client.Window.Location.getHostName() + ":5232/" +
        LoginModel.getInstance().getLogin() + "/calendar.ics/</i></font><br><br>";

    private String getDocumentVersion() {
        return Config.get().getVersion().contains("SNAPSHOT") ? "latest" : Config.get().getVersion();
    }

    private void loadWindowsContent(RequestBuilder.Method method) {

        String host = com.google.gwt.user.client.Window.Location.getHostName();
        String user = LoginModel.getInstance().getLogin();
        String requestUrl = "http://" + host + ":" + SchedulerConfig.get().getCalendarServerPort() +
            "/calendar-service/private-urls/" + user + "/";

        RequestBuilder rb = new RequestBuilder(method, requestUrl);
        rb.setCallback(new RequestCallback() {

            @Override
            public void onResponseReceived(Request request, Response response) {
                pane.clear();
                root.clear();
                window.clear();

                if (200 == response.getStatusCode()) {
                    text.setContents(buildText(response.getText()));
                } else {
                    text.setContents("Error : status code " + response.getStatusCode());
                }

                pane.addMember(img);
                pane.addMember(text);

                root.addMember(pane);
                root.addMember(buttons);

                window.addItem(root);
                window.show();
            }

            @Override
            public void onError(Request request, Throwable exception) {
                pane.clear();
                root.clear();
                window.clear();

                text.setContents("Exception : " + exception.getMessage());
                pane.addMember(img);
                pane.addMember(text);

                root.addMember(pane);
                root.addMember(buttons);

                window.addItem(root);
                window.show();
            }

        });

        try {
            rb.send();

        } catch (RequestException e) {
            com.google.gwt.user.client.Window.alert("error = " + e.getMessage());
        }

    }

    private String buildText(String responseText) {

        final StringBuilder sb = new StringBuilder(CALENDAR_CONTEXT);

        if (responseText != null && !responseText.equals("")) {
            final String host = com.google.gwt.user.client.Window.Location.getHostName();
            final String user = LoginModel.getInstance().getLogin();
            String privateUrl = "http://" + host + ":5232/" + user + "/" + responseText;

            sb.append(
                    "<font size=\"3\"> Private Calendar URL without authentication (Outlook, Google Calendar):<br><br><i>" +
                        privateUrl +
                        "</i><br><br>Do not share this URL. <b>Regenerate</b> or <b>Delete</b> it if URL is compromised.</font>");

            buttons.addMember(regenerateBt);
            buttons.addMember(deleteBt);
            buttons.removeMember(createBt);

        } else {
            sb.append(
                    "<font size=\"3\"> Private Calendar URL without authentication (Outlook, Google Calendar): <br><b>Create</b> if needed.</font>");

            buttons.addMember(createBt);
            buttons.removeMember(regenerateBt);
            buttons.removeMember(deleteBt);
        }

        sb.append("<br><br><br><font size=\"3\"><a target='_blank' href='http://doc.activeeon.com/" +
            getDocumentVersion() +
            "/user/ProActiveUserGuide.html#_calendar_service'>See calendar Documentation and Installation</a></font> ");

        return sb.toString();
    }

}
