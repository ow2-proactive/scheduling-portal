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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;


/**
 * Reloads components one after another
 */
public class ReloadableChain implements Reloadable {

    private Reloadable[] reloadables;

    private boolean reloading = false;

    private Runnable onFinish;

    public ReloadableChain(Reloadable... reloadables) {
        this.reloadables = reloadables;

        for (int i = 0; i < reloadables.length - 1; i++) {
            final int index = i;
            reloadables[i].onFinish(new Runnable() {
                public void run() {
                    if (reloading) {
                        ReloadableChain.this.reloadables[index + 1].reload();
                    }
                }
            });
        }
        reloadables[reloadables.length - 1].onFinish(new Runnable() {
            public void run() {
                reloading = false;
                if (onFinish != null) {
                    onFinish.run();
                }
            }
        });
    }

    public synchronized void stopReloading() {
        reloading = false;
    }

    public void reload() {
        if (reloading) {
            return;
        }
        reloading = true;
        reloadables[0].reload();
    }

    @Override
    public void onFinish(Runnable onFinish) {
        this.onFinish = onFinish;
    }

}
