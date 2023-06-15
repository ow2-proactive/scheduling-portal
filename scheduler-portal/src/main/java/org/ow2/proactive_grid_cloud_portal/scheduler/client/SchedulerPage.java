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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.ow2.proactive_grid_cloud_portal.common.client.AboutWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.ImagesUnbundled;
import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.LogWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.ToolButtonsRender;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobInfoView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobResultView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TaskInfoView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.VarInfoView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsDetailColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TaskDetailColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.grid.HoverCustomizer;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import com.smartgwt.client.widgets.toolbar.ToolStripMenuButton;
import com.smartgwt.client.widgets.toolbar.ToolStripSeparator;


/**
 * Page displayed when the client is logged in
 * <p>
 * Contains and displays views for jobs, tasks, etc
 *
 *
 * @author mschnoor
 */
public class SchedulerPage implements SchedulerStatusListener, LogListener, ExecutionDisplayModeListener {

    static SchedulerPage inst;

    protected TabSet leftTabSet;

    protected Tab tasksTab;

    protected Layout tasksPane;

    protected Tab visuTab;

    private Tab varInfoTab;

    private Tab jobResultTab;

    private Tab serverLogsTab;

    private Tab outputTab;

    private Tab taskinfoTab;

    private Tab taskResultTab;

    protected Canvas visuPane;

    private static final Logger LOGGER = Logger.getLogger(SchedulerPage.class.getName());

    /** Actions on the scheduler */
    private static final String START = "Start";

    private static final String STOP = "Stop";

    private static final String FREEZE = "Freeze";

    private static final String RESUME = "Resume";

    private static final String PAUSE = "Pause";

    private static final String KILL = "Kill";

    private static final String SHUTDOWN = "Shutdown";

    private static final String MANAGE_LABELS = "Manage labels";

    /* Actions descriptions */
    private static final String DESCRIPTION = "description";

    private static final String SCHED_START_BUTTON_DESCRIPTION = "Start Scheduler Server from Stopped status";

    private static final String SCHED_STOP_BUTTON_DESCRIPTION = "Stop Scheduler Server (Submitted Jobs terminate)";

    private static final String SCHED_FREEZE_BUTTON_DESCRIPTION = "Freeze Scheduler Server (Running Tasks terminate)";

    private static final String SCHED_RESUME_BUTTON_DESCRIPTION = "Resume Scheduler Server from Paused or Frozen status";

    private static final String SCHED_PAUSE_BUTTON_DESCRIPTION = "Pause Scheduler Server (Running Jobs terminate)";

    private static final String SCHED_KILL_BUTTON_DESCRIPTION = "Kill Scheduler Server";

    private static final String SCHED_SHUTDOWN_BUTTON_DESCRIPTION = "Shutdown Scheduler Server (Running Tasks terminate)";

    private static final String MANAGE_LABELS_BUTTON_DESCRIPTION = "Manage job labels";

    /** root layout: parent to all widgets of this view */
    private Layout rootLayout = null;

    /** view displaying info about the selected job */
    private JobInfoView jobInfo = null;

    /** view displaying job variables of the selected job */
    private VarInfoView varInfoView = null;

    /** view displaying info about the selected task */
    private TaskInfoView taskInfo = null;

    /** displays connected users */
    private UsersView usersView = null;

    /** displays scheduler & accounting stats */
    private StatisticsView statsView = null;

    /** job visualization */
    private VisualizationViewSwitcher visuView = null;

    /** logs for async scheduler callbacks */
    private LogWindow logWindow = null;

    /** about this app */
    private AboutWindow aboutWindow = null;

    /** client settings */
    private SettingsWindow settingsWindow = null;

    private ManageLabelsWindow manageLabelsWindow = null;

    private Menu adminMenu = null;

    /** displayed when critical log events occur */
    private ToolStripButton errorButton = null;

    /** displays the scheduler status */
    private Label schedulerStatusLabel = null;

    private long lastCriticalMessage = 0;

    private SchedulerController controller = null;

    // Logo strip properties
    private int logoStripHeight = 40;

    private String logoStripBackgroundColor = "#fafafa";

    private String logoStripBorder = "0px";

    private ToolButtonsRender toolButtonsRender = new ToolButtonsRender();

    private JobResultView jobResultView = null;

    private ToolStrip paShortcutsStrip = null;

    private ToolStrip logoStrip = null;

    /**
     * Default constructor
     *
     * @param controller Controller that created this page
     */
    public SchedulerPage(SchedulerController controller) {
        this.controller = controller;
        buildAndShow();
        this.controller.getEventDispatcher().addSchedulerStatusListener(this);
        LogModel.getInstance().addLogListener(this);
        this.controller.getExecutionController().getModel().addExecutionsDisplayModeListener(this);
        // very ugly, only way to control the scroll viewport in VisualizationViewImage.java
        inst = this;
    }

    /**
     * Creates the layout and adds it to the page
     *
     * <pre>
     * +- content:VLayout --------+
     * |+- tools:Widget ---------+|
     * || # buildTools()         ||
     * |+------------------------+|
     * |+- stack:SectionStack ---+|
     * ||+- jobSection ---------+||
     * |||+- topPane:Layout ---+|||
     * |||| # buildTopPane()   ||||
     * |||+--------------------+|||
     * ||+----------------------+||
     * ||+- detailsSection -----+||
     * |||+- botPane:Layout ---+|||
     * |||| # buildBotPane()   ||||
     * |||+--------------------+|||
     * ||+----------------------+||
     * |+------------------------+|
     * +--------------------------+
     * </pre>
     *
     */
    private void buildAndShow() {
        VLayout contentLayout = new VLayout();
        this.rootLayout = contentLayout;
        contentLayout.setWidth100();
        contentLayout.setHeight100();
        contentLayout.setBackgroundColor(logoStripBackgroundColor);

        this.aboutWindow = new AboutWindow();
        this.settingsWindow = new SettingsWindow(controller);
        this.manageLabelsWindow = new ManageLabelsWindow(controller);

        Canvas tools = buildTools();

        HorizontalPanel panel = new HorizontalPanel();
        panel.setWidth("100%");
        panel.setHeight("3px");
        panel.getElement().getStyle().setBackgroundColor("#f47930");
        panel.getElement().getStyle().setPadding(-1, Unit.PX);

        SectionStackSection executionsSections = this.controller.buildExecutionsView();

        Layout botPane = buildBotPane();
        SectionStackSection detailsSection = new SectionStackSection();
        detailsSection.setTitle("Details");
        detailsSection.setExpanded(true);
        detailsSection.setItems(botPane);

        SectionStack stack = new SectionStack();
        stack.setWidth100();
        stack.setHeight100();
        stack.setMargin(2);
        stack.setVisibilityMode(VisibilityMode.MULTIPLE);
        stack.setAnimateSections(true);
        stack.setOverflow(Overflow.HIDDEN);

        stack.setSections(executionsSections, detailsSection);

        contentLayout.addMember(buildLogoStrip());
        contentLayout.addMember(tools);
        contentLayout.addMember(panel);
        contentLayout.addMember(stack);
        this.logWindow = new LogWindow(controller);

        this.rootLayout.draw();
    }

    /** admin scheduler functionalities */
    private MenuItem schedStartButton;

    private MenuItem schedStopButton;

    private MenuItem schedFreezeButton;

    private MenuItem schedResumeButton;

    private MenuItem schedPauseButton;

    private MenuItem schedKillButton;

    private MenuItem schedShutdownButton;

    private MenuItem schedManageLabelsButton;

    private ToolStrip buildLogoStrip() {

        // Activeeon Logo
        ToolStrip logoAE = new ToolStrip();
        logoAE.setHeight(logoStripHeight);
        logoAE.setWidth("33%");
        logoAE.setBackgroundImage("");
        logoAE.setBackgroundColor(logoStripBackgroundColor);
        logoAE.setMargin(0);
        logoAE.setBorder(logoStripBorder);
        logoAE.setAlign(Alignment.LEFT);
        logoAE.setStyleName("brand-logo");
        Img logoImg = new Img(SchedulerImagesUnbundled.AE_LOGO, 146, logoStripHeight);
        logoImg.addClickHandler(clickEvent -> Window.open("http://activeeon.com/", "", ""));
        logoAE.addMember(logoImg);

        paShortcutsStrip = buildShortcutStrip(true, true, true, true);

        // Navbar Header
        logoStrip = new ToolStrip();
        logoStrip.setHeight(logoStripHeight);
        logoStrip.setWidth100();
        logoStrip.setBackgroundImage("");
        logoStrip.setBackgroundColor(logoStripBackgroundColor);
        logoStrip.setBorder(logoStripBorder);
        logoStrip.setMargin(0);

        logoStrip.addMember(logoAE);
        logoStrip.addMember(paShortcutsStrip);

        return logoStrip;
    }

    private ToolStrip buildShortcutStrip(boolean automationDashboard, boolean studio, boolean scheduler, boolean rm) {
        String login = LoginModel.getInstance().getLogin();
        if (login != null) {
            login = " <b>" + login + "</b>";
        } else {
            login = "";
        }

        ToolStripButton automationDashboardLinkButton = toolButtonsRender.getAutomationDashboardLinkButton();
        ToolStripButton studioLinkButton = toolButtonsRender.getStudioLinkButton();
        ToolStripButton schedulerLinkButton = toolButtonsRender.getSchedulerHighlightedLinkButton();
        ToolStripButton resourceManagerLinkButton = toolButtonsRender.getResourceManagerLinkButton();
        ToolStripButton logoutButton = toolButtonsRender.getLogoutButton(login, SchedulerPage.this.controller);

        // Shortcut buttons strip
        ToolStrip paShortcutsStrip = new ToolStrip();
        Img customBrandLogo = new Img("../../assets/image/" + SchedulerImagesUnbundled.EXTRA_LOGO_CENTER,
                                      135,
                                      logoStripHeight);
        customBrandLogo.setAutoWidth();
        customBrandLogo.setStyleName("custom-brand-logo");
        paShortcutsStrip.addMember(customBrandLogo);
        if (automationDashboard) {
            paShortcutsStrip.addButton(automationDashboardLinkButton);
            paShortcutsStrip.addSpacer(4);
        }
        if (studio) {
            paShortcutsStrip.addButton(studioLinkButton);
            paShortcutsStrip.addSpacer(4);
        }
        if (scheduler) {
            paShortcutsStrip.addButton(schedulerLinkButton);
            paShortcutsStrip.addSpacer(4);
        }
        if (rm) {
            paShortcutsStrip.addButton(resourceManagerLinkButton);
            paShortcutsStrip.addSpacer(4);
        }
        ToolStripSeparator separator = new ToolStripSeparator();
        separator.setHeight(40);
        paShortcutsStrip.addMember(separator);
        paShortcutsStrip.addSpacer(4);
        paShortcutsStrip.addButton(logoutButton);
        paShortcutsStrip.addSpacer(4);

        paShortcutsStrip.setMargin(5);
        paShortcutsStrip.setBackgroundImage("");
        paShortcutsStrip.setBackgroundColor(logoStripBackgroundColor);
        paShortcutsStrip.setBorder(logoStripBorder);
        paShortcutsStrip.setAlign(Alignment.RIGHT);
        paShortcutsStrip.setStyleName("pa-shortcuts");
        paShortcutsStrip.setPosition("static");
        paShortcutsStrip.setAlign(Alignment.RIGHT);
        return paShortcutsStrip;
    }

    /**
     * Builds and returns the toolbar
     *
     * <pre>
     * +- ToolStrip --------------------------------------------------------------+
     * |+- Portal v -++- Admin v -++- Help v -+|+ Submit ++ Logout +   +- Img ---+|
     * || Submit     || Start     || Logs     ||+--------++--------+   | PA logo ||
     * || Settings   || Stop      || About    |                        +---------+|
     * +| Credentials|| Freeze    || Tutorials||
     *  | Logout     || Pause     |+----------+
     *  +------------+| Resume    |
     *                | Kill      |
     *                +-----------+
     * </pre>
     */
    @SuppressWarnings("squid:S3776")
    private ToolStrip buildTools() {
        ToolStrip tools = new ToolStrip();
        tools.setHeight(50);
        tools.setWidth100();
        tools.setBackgroundImage("");
        tools.setBackgroundColor(logoStripBackgroundColor);
        tools.setBorder("0px");

        MenuItem submitMenuItem = new MenuItem("Submit job",
                                               SchedulerImages.instance.job_submit_16().getSafeUri().asString());
        submitMenuItem.addClickHandler(event -> new SubmitWindow().show());
        MenuItem flatSubmitMenuItem = new MenuItem("Submit command file",
                                                   SchedulerImages.instance.script_16().getSafeUri().asString());
        flatSubmitMenuItem.addClickHandler(event -> new FlatSubmitWindow(SchedulerPage.this.controller).show());

        MenuItem settingsMenuItem = new MenuItem("Settings", Images.instance.settings_16().getSafeUri().asString());
        settingsMenuItem.addClickHandler(event -> SchedulerPage.this.settingsWindow.show());

        MenuItem credMenuItem = new MenuItem("Create credentials", Images.instance.key_16().getSafeUri().asString());
        credMenuItem.addClickHandler(event -> new CredentialsWindow().show());

        MenuItem thirdPartyCredentialsMenuItem = new MenuItem("Manage third-party credentials",
                                                              Images.instance.key_16().getSafeUri().asString());
        thirdPartyCredentialsMenuItem.addClickHandler(event -> new ThirdPartyCredentialsWindow(SchedulerPage.this.controller).show());

        MenuItem serversMenuItem = new MenuItem("Data servers", Images.instance.server_16().getSafeUri().asString());
        serversMenuItem.addClickHandler(event -> {
            String url = GWT.getModuleBaseURL() + "servers?codebase=" + GWT.getHostPageBaseURL();
            Window.open(url, "_blank", "");
        });

        MenuItem logoutMenuItem = new MenuItem("Logout", Images.instance.exit_18().getSafeUri().asString());
        logoutMenuItem.addClickHandler(event -> SC.confirm("Logout", "Are you sure you want to exit?", value -> {
            if (value) {
                SchedulerPage.this.controller.logout();
            }
        }));

        ToolStripMenuButton portalMenuButton = new ToolStripMenuButton("Portal");
        Menu portalMenu = new Menu();
        portalMenu.setItems(submitMenuItem,
                            flatSubmitMenuItem,
                            new MenuItemSeparator(),
                            credMenuItem,
                            thirdPartyCredentialsMenuItem,
                            serversMenuItem,
                            settingsMenuItem,
                            new MenuItemSeparator(),
                            logoutMenuItem);
        portalMenuButton.setMenu(portalMenu);

        MenuItem logMenuItem = new MenuItem("Display logs", Images.instance.log_16().getSafeUri().asString());
        logMenuItem.addClickHandler(event -> {
            SchedulerPage.this.logWindow.show();
            errorButton.hide();
        });

        MenuItem documentationMenuItem = new MenuItem("Documentation",
                                                      Images.instance.icon_manual().getSafeUri().asString());
        documentationMenuItem.addClickHandler(event -> Window.open("/doc/", "Documentation", ""));

        MenuItem aboutMenuItem = new MenuItem("About", Images.instance.about_16().getSafeUri().asString());
        aboutMenuItem.addClickHandler(event -> SchedulerPage.this.aboutWindow.show());

        // Icon created by https://www.flaticon.com/authors/srip
        MenuItem tutorialsMenuItem = new MenuItem("Tutorials",
                                                  Images.instance.icon_graduation_cap_16().getSafeUri().asString());
        tutorialsMenuItem.addClickHandler(event -> Window.open("https://try.activeeon.com/tutorials.html",
                                                               "Tutorials",
                                                               ""));

        ToolStripMenuButton helpMenuButton = new ToolStripMenuButton("Help");
        Menu helpMenu = new Menu();
        helpMenu.setItems(logMenuItem, documentationMenuItem, aboutMenuItem, tutorialsMenuItem);
        helpMenuButton.setMenu(helpMenu);

        ToolStripButton submitButton = new ToolStripButton("Submit job");
        submitButton.setIcon(SchedulerImages.instance.job_submit_16().getSafeUri().asString());
        submitButton.setIconSize(20);
        submitButton.setTooltip("Submit a new job");
        submitButton.addClickHandler(event -> new SubmitWindow().show());

        ToolStripButton planButton = new ToolStripButton("Plan job");
        planButton.setIcon(SchedulerImages.instance.job_plan_16().getSafeUri().asString());
        planButton.setIconSize(20);
        planButton.setTooltip("Plan a job");
        planButton.addClickHandler(event -> new PlanWindow(SchedulerPage.this.controller).show());

        schedStartButton = new MenuItem(START);
        schedStartButton.setIcon(SchedulerImages.instance.scheduler_start_16().getSafeUri().asString());
        schedStartButton.setAttribute(DESCRIPTION, SCHED_START_BUTTON_DESCRIPTION);
        schedStartButton.addClickHandler(event -> SchedulerPage.this.controller.startScheduler());

        schedStopButton = new MenuItem(STOP);
        schedStopButton.setIcon(SchedulerImages.instance.scheduler_stop_16().getSafeUri().asString());
        schedStopButton.setAttribute(DESCRIPTION, SCHED_STOP_BUTTON_DESCRIPTION);
        schedStopButton.addClickHandler(event -> SchedulerPage.this.controller.stopScheduler());

        schedFreezeButton = new MenuItem(FREEZE);
        schedFreezeButton.setIcon(SchedulerImages.instance.scheduler_freeze_16().getSafeUri().asString());
        schedFreezeButton.setAttribute(DESCRIPTION, SCHED_FREEZE_BUTTON_DESCRIPTION);
        schedFreezeButton.addClickHandler(event -> SchedulerPage.this.controller.freezeScheduler());

        schedResumeButton = new MenuItem(RESUME);
        schedResumeButton.setIcon(SchedulerImages.instance.scheduler_resume_16().getSafeUri().asString());
        schedResumeButton.setAttribute(DESCRIPTION, SCHED_RESUME_BUTTON_DESCRIPTION);
        schedResumeButton.addClickHandler(event -> SchedulerPage.this.controller.resumeScheduler());

        schedPauseButton = new MenuItem(PAUSE);
        schedPauseButton.setIcon(SchedulerImages.instance.scheduler_pause_16().getSafeUri().asString());
        schedPauseButton.setAttribute(DESCRIPTION, SCHED_PAUSE_BUTTON_DESCRIPTION);
        schedPauseButton.addClickHandler(event -> SchedulerPage.this.controller.pauseScheduler());

        schedKillButton = new MenuItem(KILL);
        schedKillButton.setIcon(SchedulerImages.instance.scheduler_kill_16().getSafeUri().asString());
        schedKillButton.setAttribute(DESCRIPTION, SCHED_KILL_BUTTON_DESCRIPTION);
        schedKillButton.addClickHandler(event -> SC.confirm("Do you really want to <strong>kill</strong> the Scheduler?",
                                                            value -> {
                                                                if (value)
                                                                    SchedulerPage.this.controller.killScheduler();
                                                            }));

        schedShutdownButton = new MenuItem(SHUTDOWN);
        schedShutdownButton.setIcon(SchedulerImages.instance.scheduler_shutdown_16().getSafeUri().asString());
        schedShutdownButton.setAttribute(DESCRIPTION, SCHED_SHUTDOWN_BUTTON_DESCRIPTION);
        schedShutdownButton.addClickHandler(event -> SC.confirm("Do you really want to <strong>shutdown</strong> the Scheduler?",
                                                                value -> {
                                                                    if (value)
                                                                        SchedulerPage.this.controller.shutdownScheduler();
                                                                }));
        schedManageLabelsButton = new MenuItem(MANAGE_LABELS);
        schedManageLabelsButton.setIcon(SchedulerImages.instance.label().getSafeUri().asString());
        schedManageLabelsButton.setAttribute(DESCRIPTION, MANAGE_LABELS_BUTTON_DESCRIPTION);
        schedManageLabelsButton.addClickHandler(event -> this.manageLabelsWindow.show());

        ToolStripMenuButton adminMenuButton = new ToolStripMenuButton("Admin");
        this.adminMenu = new Menu();
        this.adminMenu.setItems(schedStartButton,
                                schedStopButton,
                                schedFreezeButton,
                                schedResumeButton,
                                schedPauseButton,
                                schedManageLabelsButton,
                                schedKillButton,
                                schedShutdownButton);
        // Adding tooltips on Admin actions
        ListGridField titleFieldDefaults = adminMenu.getTitleFieldDefaults();
        titleFieldDefaults.setShowHover(true);
        titleFieldDefaults.setHoverCustomizer(new HoverCustomizer() {
            @Override
            public String hoverHTML(Object value, ListGridRecord record, int rowNum, int colNum) {
                String decription = record.getAttribute(DESCRIPTION);
                if (record.getAttribute(DESCRIPTION) != null)
                    return decription;
                return null;
            }
        });
        adminMenuButton.setMenu(adminMenu);

        errorButton = new ToolStripButton("<strong>Network error</strong>",
                                          Images.instance.net_error_16().getSafeUri().asString());
        errorButton.setBackgroundColor("#ffbbbb");
        errorButton.addClickHandler(event -> {
            SchedulerPage.this.logWindow.show();
            errorButton.hide();
        });
        errorButton.hide();

        schedulerStatusLabel = new Label(SchedulerStatus.STARTED.name());
        schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_start_16().getSafeUri().asString());
        schedulerStatusLabel.setIconSize(20);
        schedulerStatusLabel.setSize("180%", "105%");
        HLayout schedulerStatusLabelLayout = new HLayout();
        schedulerStatusLabelLayout.addMember(schedulerStatusLabel);

        tools.addMenuButton(portalMenuButton);
        tools.addMenuButton(adminMenuButton);
        tools.addMenuButton(helpMenuButton);
        tools.addSeparator();
        tools.addButton(submitButton);
        tools.addSeparator();
        tools.addButton(planButton);
        tools.addSeparator();
        tools.addButton(errorButton);
        tools.addFill();
        tools.addMember(schedulerStatusLabelLayout);

        // disable all controls at first, next event will sort it out
        this.statusChanged(SchedulerStatus.LOADING);

        return tools;
    }

    /*
     * (non-Javadoc)
     *
     * @see
     * org.ow2.proactive_grid_cloud_portal.client.Listeners.SchedulerStatusListener#statusChanged(
     * org.ow2.proactive_grid_cloud_portal.shared.SchedulerStatus)
     */
    public void statusChanged(SchedulerStatus status) {
        // this only changes the enable status of scheduler admin buttons

        LoginModel loginModel = LoginModel.getInstance();
        switch (status) {
            case LOADING:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(false);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(false);
                schedShutdownButton.setEnabled(false);
                schedManageLabelsButton.setEnabled(false);
                schedulerStatusLabel.setIcon(SchedulerImages.instance.loading().getSafeUri().asString());
                break;
            case SHUTTING_DOWN:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(false);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(false);
                schedShutdownButton.setEnabled(false);
                schedManageLabelsButton.setEnabled(false);
                schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_shutdown_16().getSafeUri().asString());
                break;
            case KILLED:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(false);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(false);
                schedShutdownButton.setEnabled(false);
                schedManageLabelsButton.setEnabled(false);
                schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_kill_16().getSafeUri().asString());
                break;
            case FROZEN:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(loginModel.userHasPermissionToStopScheduler());
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(loginModel.userHasPermissionToResumeScheduler());
                schedKillButton.setEnabled(loginModel.userHasPermissionToKillScheduler());
                schedShutdownButton.setEnabled(loginModel.userHasPermissionToShutDownScheduler());
                schedManageLabelsButton.setEnabled(loginModel.userHasPermissionToSetLabels());
                schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_freeze_16().getSafeUri().asString());
                break;
            case PAUSED:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(loginModel.userHasPermissionToStopScheduler());
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(loginModel.userHasPermissionToResumeScheduler());
                schedKillButton.setEnabled(loginModel.userHasPermissionToKillScheduler());
                schedShutdownButton.setEnabled(loginModel.userHasPermissionToShutDownScheduler());
                schedManageLabelsButton.setEnabled(loginModel.userHasPermissionToSetLabels());
                schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_pause_16().getSafeUri().asString());
                break;
            case STARTED:
            case UNLINKED:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(loginModel.userHasPermissionToStopScheduler());
                schedFreezeButton.setEnabled(loginModel.userHasPermissionToFreezeScheduler());
                schedPauseButton.setEnabled(loginModel.userHasPermissionToPauseScheduler());
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(loginModel.userHasPermissionToKillScheduler());
                schedShutdownButton.setEnabled(loginModel.userHasPermissionToShutDownScheduler());
                schedManageLabelsButton.setEnabled(loginModel.userHasPermissionToSetLabels());
                schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_start_16().getSafeUri().asString());
                break;
            case STOPPED:
                schedStartButton.setEnabled(loginModel.userHasPermissionToStartScheduler());
                schedStopButton.setEnabled(false);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(loginModel.userHasPermissionToKillScheduler());
                schedShutdownButton.setEnabled(loginModel.userHasPermissionToShutDownScheduler());
                schedManageLabelsButton.setEnabled(loginModel.userHasPermissionToSetLabels());
                schedulerStatusLabel.setIcon(SchedulerImages.instance.scheduler_stop_16().getSafeUri().asString());
                break;
            default:
                LOGGER.warning("Unexpected scheduler status");
                break;
        }

        // Update the scheduler status label

        String neededNodes = "-";

        HashMap<String, String> statistics = controller.getModel().getSchedulerStatistics();
        if (statistics != null) {
            neededNodes = statistics.get("NeededNodes");
        }

        if (notOnlyDigits(neededNodes)) {
            neededNodes = "-";
        }

        schedulerStatusLabel.setContents("Status:" + status.name() + "<br>Needed Nodes:" + neededNodes);
        this.adminMenu.redraw();
    }

    private boolean notOnlyDigits(String str) {
        return !str.matches("\\d+");
    }

    /**
     * Builds and returns the bottom pane: currently selected job somewhat in a master/detail fashion
     *
     * <pre>
     * +- layout:HLayout ----+
     * |+- leftTabs:TabSet -+|
     * ||+- output:Tab ----+||
     * ||| sel. job output |||
     * ||+-----------------+||
     * ||+- tasks:Tab -----+||
     * ||| sel. job tasks  |||
     * ||+-----------------+||
     * ||+- users:Tab -----+||   | left
     * ||| connected users |||   |
     * ||+-----------------+||   | the actual widget is horizontal
     * |+-------------------+|   |
     * |+- rightTabs:TabSet +|   V right
     * ||+- infoTab:Tab ---+||
     * ||| sel. job info   |||
     * ||+-----------------+||
     * ||+- filters:Tab ---+||
     * ||| jobgrid filters |||
     * ||+-----------------+||
     * |+-------------------+|
     * +---------------------+
     * </pre>
     *
     */
    private Layout buildBotPane() {
        leftTabSet = new TabSet();
        leftTabSet.setWidth("50%");
        leftTabSet.setHeight100();
        leftTabSet.setTabBarPosition(Side.TOP);
        leftTabSet.setShowResizeBar(true);

        tasksPane = this.controller.buildTaskView();
        this.buildTasksTab();

        this.visuView = new VisualizationViewSwitcher(this.controller);
        this.visuPane = this.visuView.build();
        this.buildVisuTab();

        final Tab usersTab = new Tab("Users Sessions", Images.instance.user_16().getSafeUri().asString());
        this.usersView = new UsersView(this.controller);
        usersTab.setPane(this.usersView.build());

        final Tab statsTab = new Tab("Statistics", Images.instance.stats_16().getSafeUri().asString());
        this.statsView = new StatisticsView(this.controller);
        statsTab.setPane(this.statsView.build());

        final Tab usageTab = new Tab("Usage", SchedulerImages.instance.usage_16().getSafeUri().asString());
        usageTab.setPane(new UsageView(this.controller).build());

        leftTabSet.addTab(tasksTab);
        leftTabSet.addTab(visuTab);
        leftTabSet.addTab(usersTab);
        leftTabSet.addTab(statsTab);
        leftTabSet.addTab(usageTab);

        leftTabSet.addTabSelectedHandler(event -> {
            if (leftTabSet.getSelectedTab().equals(tasksTab)) {
                controller.setLazyStatsFetch(true);
                controller.setLazyUserFetch(true);
            } else if (leftTabSet.getSelectedTab().equals(usersTab)) {
                controller.setLazyStatsFetch(true);
                controller.setLazyUserFetch(false);
            } else if (leftTabSet.getSelectedTab().equals(statsTab)) {
                controller.setLazyStatsFetch(false);
                controller.setLazyUserFetch(true);
            }

            if (leftTabSet.getSelectedTab().equals(visuTab)) {
                controller.setVisuFetchEnabled(true);
                JobsModel jobsModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel().getJobsModel();
                if (jobsModel.getSelectedJob() != null) {
                    controller.visuFetch(jobsModel.getSelectedJob().getId().toString());
                }
            } else {
                controller.setVisuFetchEnabled(false);
            }
        });

        TabSet rightTabSet = new TabSet();
        rightTabSet.setWidth("50%");
        rightTabSet.setHeight100();
        rightTabSet.setTabBarPosition(Side.TOP);

        Tab jobinfoTab = new Tab("Job Info", ImagesUnbundled.INFO_16);
        this.jobInfo = new JobInfoView(this.controller, new JobsDetailColumnsFactory());
        jobinfoTab.setPane(this.jobInfo.build());

        varInfoTab = new Tab("Job Variables", ImagesUnbundled.INFO_16);
        this.varInfoView = new VarInfoView(this.controller);
        varInfoTab.setPane(this.varInfoView.build());

        taskinfoTab = new Tab("Task Info", ImagesUnbundled.INFO_16);
        this.taskInfo = new TaskInfoView(this.controller, new TaskDetailColumnsFactory());
        taskinfoTab.setPane(this.taskInfo.build());

        outputTab = new Tab("Output", ImagesUnbundled.OUTPUT_16);
        outputTab.setPane(this.controller.buildOutputView());

        serverLogsTab = new Tab("Server Logs", ImagesUnbundled.OUTPUT_16);
        serverLogsTab.setPane(this.controller.buildServerLogsView());

        taskResultTab = new Tab("Task Preview", ImagesUnbundled.SEARCH_16);
        taskResultTab.setPane(this.controller.buildPreviewView());

        jobResultTab = new Tab("Job Results", ImagesUnbundled.SEARCH_16);
        this.jobResultView = new JobResultView(this.controller);
        jobResultTab.setPane(this.jobResultView.build());

        rightTabSet.addTab(jobinfoTab);
        rightTabSet.addTab(varInfoTab);
        rightTabSet.addTab(jobResultTab);
        rightTabSet.addTab(taskinfoTab);
        rightTabSet.addTab(taskResultTab);
        rightTabSet.addTab(outputTab);
        rightTabSet.addTab(serverLogsTab);

        HLayout layout = new HLayout();

        layout.addMember(leftTabSet);
        layout.addMember(rightTabSet);

        return layout;
    }

    /**
     * Removes the layout and widgets from the page
     * Call this when the view should be definitely removed and GC's, else just hide() it
     */
    public void destroy() {
        this.rootLayout.destroy();
        this.logWindow.destroy();
        this.aboutWindow.destroy();
        this.settingsWindow.destroy();
        this.manageLabelsWindow.destroy();

        this.rootLayout = null;
        this.jobInfo = null;
        this.varInfoView = null;
        this.controller = null;
        this.logWindow = null;
        this.aboutWindow = null;
    }

    @Override
    public void logMessage(String message) {
        long dt = System.currentTimeMillis() - this.lastCriticalMessage;
        if (dt > SchedulerConfig.get().getClientRefreshTime() * 4) {
            this.errorButton.hide();
        }
    }

    @Override
    public void logImportantMessage(String message) {
        logMessage(message);
    }

    @Override
    public void logCriticalMessage(String message) {
        this.lastCriticalMessage = System.currentTimeMillis();
        this.errorButton.show();
    }

    @Override
    public void modeSwitched(ExecutionListMode mode) {
        switch (mode) {
            case JOB_CENTRIC:
                this.buildTasksTab();
                this.buildVisuTab();
                leftTabSet.addTab(tasksTab, 0);
                leftTabSet.addTab(visuTab, 1);
                List<Integer> selectedJobIds = controller.getExecutionController().getModel().getSelectedJobIds();
                controller.getExecutionController()
                          .getJobsController()
                          .checkJobsPermissionMethods(selectedJobIds.stream()
                                                                    .map(String::valueOf)
                                                                    .collect(Collectors.toList()),
                                                      null);
                break;
            case TASK_CENTRIC:
                leftTabSet.updateTab(tasksTab, null);
                leftTabSet.removeTab(tasksTab);
                leftTabSet.updateTab(visuTab, null);
                leftTabSet.removeTab(visuTab);
                break;
            default:
                LOGGER.warning("Unexpected mode");
                break;
        }
        leftTabSet.markForRedraw();
    }

    protected void buildTasksTab() {
        tasksTab = new Tab("Tasks", ImagesUnbundled.MONITORING_16);
        tasksTab.setPane(tasksPane);
        Job selectedJob = controller.getExecutionController().getModel().getSelectedJob();
        LoginModel loginModel = LoginModel.getInstance();
        if (selectedJob != null &&
            loginModel.userDoesNotHavePermissionToGetJobsState(new ArrayList<>(Collections.singleton(selectedJob.getId()
                                                                                                                .toString())))) {
            tasksTab.setDisabled(true);
            disableTaskInfoTab(true);
            disableTaskResultTab(true);
        } else {
            tasksTab.setDisabled(false);
            disableTaskInfoTab(false);
            disableTaskResultTab(false);
        }
    }

    public void rebuildShortcutStrip(boolean automationDashboard, boolean studio, boolean scheduler, boolean rm) {
        logoStrip.removeMember(paShortcutsStrip);
        paShortcutsStrip = buildShortcutStrip(automationDashboard, studio, scheduler, rm);
        logoStrip.addMember(paShortcutsStrip);
        logoStrip.redraw();
    }

    public void disableTaskInfoTab(boolean disabled) {
        if (taskinfoTab != null) {
            taskinfoTab.setDisabled(disabled);
        }
    }

    public void disableTaskResultTab(boolean disabled) {
        if (taskResultTab != null) {
            taskResultTab.setDisabled(disabled);
        }
    }

    public void disableTasksTab(boolean disabled) {
        leftTabSet.updateTab(tasksTab, null);
        leftTabSet.removeTab(tasksTab);
        tasksTab = new Tab("Tasks", ImagesUnbundled.MONITORING_16);
        tasksTab.setPane(tasksPane);
        tasksPane.setDisabled(disabled);
        tasksTab.setDisabled(disabled);
        leftTabSet.addTab(tasksTab, 0);
        leftTabSet.redraw();
    }

    public void disableVarInfoTab(boolean disabled) {
        varInfoTab.setDisabled(disabled);
    }

    public void disableJobResultsTab(boolean disabled) {
        jobResultTab.setDisabled(disabled);
    }

    public void disableVisualizationTab(boolean disabled) {
        leftTabSet.updateTab(visuTab, null);
        leftTabSet.removeTab(visuTab);
        visuTab = new Tab("Visualization", ImagesUnbundled.PA_16);
        visuTab.setPane(this.visuPane);
        visuTab.setDisabled(disabled);
        leftTabSet.addTab(visuTab, 1);
        leftTabSet.redraw();
    }

    public void disableServerLogsTab(boolean disabled) {
        serverLogsTab.setDisabled(disabled);
    }

    public void disableOutputTab(boolean disabled) {
        outputTab.setDisabled(disabled);
    }

    protected void buildVisuTab() {
        visuTab = new Tab("Visualization", ImagesUnbundled.PA_16);
        visuTab.setPane(this.visuPane);
        Job selectedJob = controller.getExecutionController().getModel().getSelectedJob();
        LoginModel loginModel = LoginModel.getInstance();
        if (selectedJob != null &&
            loginModel.userDoesNotHavePermissionToGetJobsContent(new ArrayList<>(Collections.singleton(selectedJob.getId()
                                                                                                                  .toString())))) {
            visuTab.setDisabled(true);
            visuView.getHtmlView().disableOpenInStudioButton(true);
        } else {
            visuTab.setDisabled(false);
            visuView.getHtmlView().disableOpenInStudioButton(false);
        }
    }

}
