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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.ImagesUnbundled;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;
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
 *  Window explains how to download and launch a node.
 */
public class AddNodeWindow {

    private Window window = null;

    /**
     * Default constructor
     */
    public AddNodeWindow() {
        this.build();
    }

    private void build() {
        HLayout pane = new HLayout();
        pane.setWidth100();
        pane.setHeight(150);
        pane.setBackgroundColor("#ffffff");

        String ver = Config.get().getVersion();
        String str =
            "<h3>Connect a node to the ProActive Resource Manager using the following way</h3><ul>" +
            "<li><a target='_blank' href='"+Config.get().getRestPublicUrl()+"/node.jar'>Download</a> jar containing ProActive node</li>" +
            "<li>Run from command line:<br/><br/><input type='text' value='java -jar node.jar -r "+ RMConfig.get().getRMUrl()+"' style='width:270px;border-style:none' disabled></li>";

        HTMLPane text = new HTMLPane();
        text.setContents(str);
        text.setCanSelectText(true);
        text.setWidth100();

        Img img = new Img(ImagesUnbundled.ABOUT_115, 115, 130);

        pane.addMember(img);
        pane.addMember(text);

        HLayout buttons = new HLayout();
        buttons.setAlign(Alignment.RIGHT);
        buttons.setWidth100();
        buttons.setHeight(20);
        buttons.setMargin(5);
        IButton ok = new IButton("Close");
        ok.setIcon(Images.instance.cancel_16().getSafeUri().asString());
        ok.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                AddNodeWindow.this.hide();
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
        this.window.setTitle("Adding a node to the Resource Manager");
        this.window.setShowMinimizeButton(false);
        this.window.setShowShadow(true);
        this.window.setIsModal(true);
        this.window.setShowModalMask(true);
        this.window.setCanSelectText(true);
        this.window.addItem(root);
        this.window.setWidth(520);
        this.window.setHeight(220);
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
