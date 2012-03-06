package org.ow2.proactive_grid_cloud_portal.rm.client.monitoring;

/**
 * Represents a reloadable entity.
 */
public interface Reloadable {
	/**
	 * Launches the reloading of the component
	 */
	public void reload();
	
	/**
	 * Executes runnable when the reload is finished
	 * 
	 * @param callback to execute
	 */
	public void onFinish(Runnable callback);
}
