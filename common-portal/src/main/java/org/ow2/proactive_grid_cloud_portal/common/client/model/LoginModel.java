package org.ow2.proactive_grid_cloud_portal.common.client.model;

public class LoginModel {

    private boolean logged = false;
    private String login = null;
    private String sessionId = null;
    
    
    private static LoginModel instance = null;
    
    public static LoginModel getInstance(){
        if(instance == null){
            instance = new LoginModel();
        }
        return instance;
    }
    
    
    /**
     * @return true if a user is currently logged in
     */
    public boolean isLoggedIn() {
        return this.logged;
    }

    public void setLoggedIn(boolean loggedIn) {
        this.logged = loggedIn;
        this.sessionId = null;
        this.login = null;
    }

    /**
     * @return the username of the currently logged in user, if available, or null
     */
    public String getLogin() {
        return this.login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    /**
     * @return session id of the currently logged in user
     */
    public String getSessionId() {
        return this.sessionId;
    }

    public void setSessionId(String id) {
        this.sessionId = id;
    }
}
