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
package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views;

import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.Reloadable;


/**
 * Reloads components one after another
 */
public class ReloadableChain implements Reloadable {

	private Reloadable[] reloadables;
	private boolean reloading = false;

	private Runnable onFinish;

	public ReloadableChain(Reloadable[] reloadables) {
		this.reloadables = reloadables;

		for (int i = 0; i < reloadables.length - 1; i++) {
			final int index = i;
			reloadables[i].onFinish(new Runnable() {
				public void run() {
					boolean continueReloading = false;
					synchronized (ReloadableChain.this) {
						if (reloading) {
							continueReloading = true;
						}
					}

					if (continueReloading) {
						ReloadableChain.this.reloadables[index + 1].reload();
					}
				}
			});
		}
		reloadables[reloadables.length - 1].onFinish(new Runnable() {
			public void run() {
				synchronized (ReloadableChain.this) {
					reloading = false;
					if (onFinish != null) {
						onFinish.run();
					}
				}
			}
		});
	}

	public synchronized void stopReloading() {
		reloading = false;
	}

	public void reload() {
		synchronized (this) {
			if (reloading) {
				return;
			}
			reloading = true;
		}
		reloadables[0].reload();
	}

	@Override
	public void onFinish(Runnable onFinish) {
		this.onFinish = onFinish;
	}

}
