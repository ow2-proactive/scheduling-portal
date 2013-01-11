/*
 *  *
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
 *  * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import com.google.gwt.event.dom.client.*;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.Widget;

public class FileDragAndDropPanel extends SimplePanel implements HasAllDragAndDropHandlers {

    // When mixed with Toolstrip, the focus panel behaves strangely (when dragging over, drag enter and drag leave events,
    // are generated.
    // To avoid this, it turns out that two drag leave events really mean that the drag is out.
    private int dragLeaveCounter = 0;

    public FileDragAndDropPanel(Widget child) {
        super();

        add(child);

        addDragHandler(new DragHandler() {
            @Override
            public void onDrag(DragEvent event) {
                disableDefaultEventBehavior(event);

            }
        });
        addDragOverHandler(new DragOverHandler() {
            @Override
            public void onDragOver(DragOverEvent event) {
                disableDefaultEventBehavior(event);

            }
        });
        addDragEndHandler(new DragEndHandler() {
            @Override
            public void onDragEnd(DragEndEvent event) {
                disableDefaultEventBehavior(event);

            }
        });
        addDragStartHandler(new DragStartHandler() {
            @Override
            public void onDragStart(DragStartEvent event) {
                disableDefaultEventBehavior(event);
            }
        });
        addDropHandler(new DropHandler() {
            @Override
            public void onDrop(DropEvent event) {
                doOnDrop(event);
                dragLeaveCounter = 0;
                disableDefaultEventBehavior(event);
            }
        });
        addDragEnterHandler(new DragEnterHandler() {
            @Override
            public void onDragEnter(DragEnterEvent event) {
                doOnDragEnter(event);
                dragLeaveCounter = 0;
                disableDefaultEventBehavior(event);
            }
        });
        addDragLeaveHandler(new DragLeaveHandler() {
            @Override
            public void onDragLeave(DragLeaveEvent event) {
                dragLeaveCounter++;
                if (dragLeaveCounter > 1) {
                    doOnDragLeave(event);
                    dragLeaveCounter = 0;
                }
                disableDefaultEventBehavior(event);
            }
        });
    }

    protected void doOnDragEnter(DragEnterEvent event) {

    }

    protected void doOnDragLeave(DragLeaveEvent event) {

    }

    protected void doOnDrop(DropEvent event) {

    }

    private void disableDefaultEventBehavior(DomEvent event) {
        event.stopPropagation();
        event.preventDefault();
    }

    public HandlerRegistration addDragEndHandler(DragEndHandler handler) {
        return addBitlessDomHandler(handler, DragEndEvent.getType());
    }

    public HandlerRegistration addDragEnterHandler(DragEnterHandler handler) {
        return addBitlessDomHandler(handler, DragEnterEvent.getType());
    }

    public HandlerRegistration addDragHandler(DragHandler handler) {
        return addBitlessDomHandler(handler, DragEvent.getType());
    }

    public HandlerRegistration addDragLeaveHandler(DragLeaveHandler handler) {
        return addBitlessDomHandler(handler, DragLeaveEvent.getType());
    }

    public HandlerRegistration addDragOverHandler(DragOverHandler handler) {
        return addBitlessDomHandler(handler, DragOverEvent.getType());
    }

    public HandlerRegistration addDragStartHandler(DragStartHandler handler) {
        return addBitlessDomHandler(handler, DragStartEvent.getType());
    }

    public HandlerRegistration addDropHandler(DropHandler handler) {
        return addBitlessDomHandler(handler, DropEvent.getType());
    }

}
