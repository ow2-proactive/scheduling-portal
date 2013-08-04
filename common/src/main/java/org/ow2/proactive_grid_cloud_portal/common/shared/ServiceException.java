package org.ow2.proactive_grid_cloud_portal.common.shared;

import org.ow2.proactive_grid_cloud_portal.common.server.Service;

import com.google.gwt.user.client.rpc.IsSerializable;


/**
 * Thrown by {@link Service} upon internal logic failure detection
 * 
 * @author mschnoor
 *
 */
@SuppressWarnings("serial")
public class ServiceException extends Exception  implements IsSerializable {

    public ServiceException() {
        super();
    }

    public ServiceException(String message) {
        super(message);
    }

    public ServiceException(String message, Throwable cause) {
        super(message, cause);
    }

}
