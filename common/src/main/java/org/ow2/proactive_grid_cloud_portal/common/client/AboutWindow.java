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

import org.ow2.proactive_grid_cloud_portal.common.shared.Config;

import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.HTMLPane;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;


/**
 * About Window: copyrights and logos
 * 
 * 
 * @author mschnoor
 *
 */
public class AboutWindow {

    private Window window = null;

    /**
     * Default constructor
     */
    public AboutWindow() {
        this.build();
    }

    private void build() {
        HLayout pane = new HLayout();
        pane.setWidth100();
        pane.setHeight(210);
        pane.setBackgroundColor("#ffffff");

        String ver = Config.get().getVersion();
        String str = "<h3>ProActive " +
            Config.get().getApplicationName() +
            " Portal</h3>" +
            "Version: " +
            ver +
            "<br><br>" +
            "Copyright (C) 1997-2013 INRIA/University of Nice-Sophia Antipolis/ActiveEon<br><br>" +
            "Visit <a target='_blank' href='http://proactive.inria.fr/'>http://proactive.inria.fr/</a> " +
            "and <a target='_blank' href='http://www.activeeon.com/'>http://www.activeeon.com/</a><br>" +
            "Contact: +33 (0)9 88 777 660, <a target='_blank' href='mailto:contact@activeeon.com'>contact@activeeon.com</a><br><br><br>" +
            "<table style='color:#404040'>" + "<tr><td>REST server</td><td>" + Config.get().getRestPublicUrl() +
            "</td></tr>" + "<tr><td>REST version</td><td>" + Config.get().getRestVersion() + "</td></tr>" +
            "<tr><td>" + Config.get().getApplicationName() + " version</td><td>" +
            Config.get().getApplicationVersion() + "</td></tr></table>";

        HTMLPane text = new HTMLPane();
        text.setContents(str);
        text.setWidth100();

        Img img = new Img(ImagesUnbundled.ABOUT_115, 115, 130);

        pane.addMember(img);
        pane.addMember(text);

        HLayout buttons = new HLayout();
        buttons.setAlign(Alignment.RIGHT);
        buttons.setWidth100();
        buttons.setHeight(20);
        buttons.setMargin(10);
        IButton ok = new IButton("Close");
        ok.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        ok.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                AboutWindow.this.hide();
            }
        });
        buttons.addMember(ok);

        VLayout root = new VLayout();
        root.setBackgroundColor("#dddddd");
        root.setWidth100();
        root.setHeight100();

        root.addMember(pane);
        root.addMember(buttons);

        this.window = new Window();
        this.window.setTitle("About");
        this.window.setShowMinimizeButton(false);
        this.window.setShowShadow(true);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.addItem(root);
        this.window.setWidth(520);
        this.window.setHeight(290);
        this.window.centerInPage();
        this.window.setCanDragReposition(false);
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
}
