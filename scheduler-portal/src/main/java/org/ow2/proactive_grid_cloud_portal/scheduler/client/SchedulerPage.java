/*
 * ################################################################
 *
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
 * ################################################################
 * $$PROACTIVE_INITIAL_DEV$$
 */
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import org.ow2.proactive_grid_cloud_portal.common.client.AboutWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.ImagesUnbundled;
import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.LogWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.ToolButtonsRender;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.common.shared.Config;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.ExecutionDisplayModeListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.ExecutionListMode;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.model.JobsModel;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.JobInfoView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TaskInfoView;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.jobs.JobsDetailColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid.tasks.TaskDetailColumnsFactory;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.MenuItemSeparator;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.tab.events.TabSelectedEvent;
import com.smartgwt.client.widgets.tab.events.TabSelectedHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import com.smartgwt.client.widgets.toolbar.ToolStripMenuButton;


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
    protected Canvas visuPane;

    /** root layout: parent to all widgets of this view */
    private Layout rootLayout = null;

    /** view displaying info about the selected job */
    private JobInfoView jobInfo = null;
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

    private Menu adminMenu = null;

    /** displayed when critical log events occur */
    private ToolStripButton errorButton = null;
    private long lastCriticalMessage = 0;

    private SchedulerController controller = null;

    // Logo strip properties
    private int logoStripHeight = 40;
    private String logoStripBackgroundColor = "#fafafa";
    private String logoStripBorder = "0px";

    private ToolButtonsRender toolButtonsRender = new ToolButtonsRender();

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
        contentLayout.setBackgroundColor("#fafafa");

        this.aboutWindow = new AboutWindow();
        this.settingsWindow = new SettingsWindow(controller);

        Canvas tools = buildTools();

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
        contentLayout.addMember(stack);
        this.logWindow = new LogWindow(controller);

        this.rootLayout.draw();
    }

    /** admin scheduler functionalities */
    private MenuItem schedStartButton, schedStopButton, schedFreezeButton, schedResumeButton,
            schedPauseButton, schedKillButton;

    private ToolStrip buildLogoStrip() {
        ToolStrip logoStrip = new ToolStrip();

        final Label schedulerLabel = new Label("ProActive Scheduling & Orchestration");
        schedulerLabel.setStyleName("schedulerHeadline");
        schedulerLabel.setHeight100();
        schedulerLabel.setAutoWidth();

        logoStrip.setStyleName("paddingLeftAndRight");
        logoStrip.setHeight(logoStripHeight);
        logoStrip.setWidth100();
        logoStrip.setBackgroundImage("");
        logoStrip.setBackgroundColor(logoStripBackgroundColor);
        logoStrip.setBorder(logoStripBorder);
        logoStrip.setMargin(0);

        logoStrip.addMember(new Img(SchedulerImagesUnbundled.PA_ICON, logoStripHeight, logoStripHeight));
        logoStrip.addMember(schedulerLabel);
        logoStrip.addFill();
        logoStrip.addMember(new Img(SchedulerImagesUnbundled.COMPANY_ICON, logoStripHeight, logoStripHeight));
        logoStrip.addFill();

        //by click on logo - open home activeeon page in new tab
        Img logoImg = new Img(SchedulerImagesUnbundled.AE_LOGO, 146, logoStripHeight);
        logoImg.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent clickEvent) {
                Window.open("http://activeeon.com/", "", "");
            }
        });
        logoStrip.addMember(logoImg);

        return logoStrip;
    }

    /**
     * Builds and returns the toolbar
     *
     * <pre>
     * +- ToolStrip --------------------------------------------------------------+
     * |+- Portal v -++- Admin v -++- Help v -+|+ Submit ++ Logout +   +- Img ---+|
     * || Submit     || Start     || Logs     ||+--------++--------+   | PA logo ||
     * || Settings   || Stop      || About    |                        +---------+|
     * +| Credentials|| Freeze    |+----------+-----------------------------------+
     *  | Logout     || Pause     |
     *  +------------+| Resume    |
     *                | Kill      |
     *                +-----------+
     * </pre>
     */
    private ToolStrip buildTools() {
        ToolStrip tools = new ToolStrip();
        tools.setHeight(50);
        tools.setWidth100();
        tools.setBackgroundImage("");
        tools.setBackgroundColor("#fafafa");
        tools.setBorder("0px");

        MenuItem submitMenuItem = new MenuItem("Submit job",
            SchedulerImages.instance.job_submit_16().getSafeUri().asString());
        submitMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SubmitWindow w = new SubmitWindow(SchedulerPage.this.controller);
                w.show();
            }
        });
        MenuItem flatSubmitMenuItem = new MenuItem("Submit command file",
            SchedulerImages.instance.script_16().getSafeUri().asString());
        flatSubmitMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                FlatSubmitWindow w = new FlatSubmitWindow(SchedulerPage.this.controller);
                w.show();
            }
        });

        MenuItem settingsMenuItem = new MenuItem("Settings",
            Images.instance.settings_16().getSafeUri().asString());
        settingsMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.settingsWindow.show();
            }
        });

        MenuItem credMenuItem = new MenuItem("Create credentials",
            Images.instance.key_16().getSafeUri().asString());
        credMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                CredentialsWindow credentialsWindow = new CredentialsWindow();
                credentialsWindow.show();
            }
        });

        MenuItem thirdPartyCredentialsMenuItem = new MenuItem("Manage third-party credentials",
            Images.instance.key_16().getSafeUri().asString());
        thirdPartyCredentialsMenuItem
                .addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
                    public void onClick(MenuItemClickEvent event) {
                        ThirdPartyCredentialsWindow credentialsWindow = new ThirdPartyCredentialsWindow(
                            SchedulerPage.this.controller);
                        credentialsWindow.show();
                    }
                });

        MenuItem serversMenuItem = new MenuItem("Data servers",
            Images.instance.server_16().getSafeUri().asString());
        serversMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                String url = GWT.getModuleBaseURL() + "servers?codebase=" + GWT.getHostPageBaseURL();
                Window.open(url, "_blank", "");
            }
        });

        MenuItem logoutMenuItem = new MenuItem("Logout", Images.instance.exit_18().getSafeUri().asString());
        logoutMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SC.confirm("Logout", "Are you sure you want to exit?", new BooleanCallback() {
                    public void execute(Boolean value) {
                        if (value) {
                            SchedulerPage.this.controller.logout();
                        }
                    }
                });
            }
        });

        ToolStripMenuButton portalMenuButton = new ToolStripMenuButton("Portal");
        Menu portalMenu = new Menu();
        portalMenu.setItems(submitMenuItem, flatSubmitMenuItem, new MenuItemSeparator(), credMenuItem,
                thirdPartyCredentialsMenuItem, serversMenuItem, settingsMenuItem, new MenuItemSeparator(),
                logoutMenuItem);
        portalMenuButton.setMenu(portalMenu);

        MenuItem logMenuItem = new MenuItem("Display logs", Images.instance.log_16().getSafeUri().asString());
        logMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.logWindow.show();
                errorButton.hide();
            }
        });

        MenuItem documentationMenuItem = new MenuItem("Documentation",
            Images.instance.icon_manual().getSafeUri().asString());
        documentationMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                String docVersion = Config.get().getVersion().contains("SNAPSHOT") ? "latest"
                        : Config.get().getVersion();
                Window.open("http://doc.activeeon.com/" + docVersion, "", "");
            }
        });

        MenuItem aboutMenuItem = new MenuItem("About", Images.instance.about_16().getSafeUri().asString());
        aboutMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.aboutWindow.show();
            }
        });
        ToolStripMenuButton helpMenuButton = new ToolStripMenuButton("Help");
        Menu helpMenu = new Menu();
        helpMenu.setItems(logMenuItem, documentationMenuItem, aboutMenuItem);
        helpMenuButton.setMenu(helpMenu);

        ToolStripButton submitButton = new ToolStripButton("Submit job");
        submitButton.setIcon(SchedulerImages.instance.job_submit_16().getSafeUri().asString());
        submitButton.setTooltip("Submit a new job");
        submitButton.addClickHandler(new ClickHandler() {
            public void onClick(ClickEvent event) {
                SubmitWindow w = new SubmitWindow(SchedulerPage.this.controller);
                w.show();
            }
        });

        schedStartButton = new MenuItem("Start");
        schedStartButton.setIcon(SchedulerImages.instance.scheduler_start_16().getSafeUri().asString());
        schedStartButton.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.controller.startScheduler();
            }
        });

        schedStopButton = new MenuItem("Stop");
        schedStopButton.setIcon(SchedulerImages.instance.scheduler_stop_16().getSafeUri().asString());
        schedStopButton.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.controller.stopScheduler();
            }
        });

        schedFreezeButton = new MenuItem("Freeze");
        schedFreezeButton.setIcon(SchedulerImages.instance.scheduler_freeze_16().getSafeUri().asString());
        schedFreezeButton.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.controller.freezeScheduler();
            }
        });

        schedResumeButton = new MenuItem("Resume");
        schedResumeButton.setIcon(SchedulerImages.instance.scheduler_resume_16().getSafeUri().asString());
        schedResumeButton.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.controller.resumeScheduler();
            }
        });

        schedPauseButton = new MenuItem("Pause");
        schedPauseButton.setIcon(SchedulerImages.instance.scheduler_pause_16().getSafeUri().asString());
        schedPauseButton.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SchedulerPage.this.controller.pauseScheduler();

            }
        });

        schedKillButton = new MenuItem("Kill");
        schedKillButton.setIcon(SchedulerImages.instance.scheduler_kill_16().getSafeUri().asString());
        schedKillButton.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
            public void onClick(MenuItemClickEvent event) {
                SC.confirm("Do you really want to <strong>kill</strong> the Scheduler?",
                        new BooleanCallback() {
                    public void execute(Boolean value) {
                        if (value)
                            SchedulerPage.this.controller.killScheduler();
                    }
                });
            }
        });

        ToolStripMenuButton adminMenuButton = new ToolStripMenuButton("Admin");
        this.adminMenu = new Menu();
        this.adminMenu.setItems(schedStartButton, schedStopButton, schedFreezeButton, schedResumeButton,
                schedPauseButton, schedKillButton);
        adminMenuButton.setMenu(adminMenu);

        String login = LoginModel.getInstance().getLogin();
        if (login != null)
            login = " <b>" + login + "</b>";
        else
            login = "";

        errorButton = new ToolStripButton("<strong>Network error</strong>",
            Images.instance.net_error_16().getSafeUri().asString());
        errorButton.setBackgroundColor("#ffbbbb");
        errorButton.addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                SchedulerPage.this.logWindow.show();
                errorButton.hide();
            }
        });
        errorButton.hide();

        ToolStripButton resourceManagerLinkButton = toolButtonsRender.getResourceManagerLinkButton();
        ToolStripButton studioLinkButton = toolButtonsRender.getStudioLinkButton();
        ToolStripButton cloudAutomationLinkButton = toolButtonsRender.getCloudAutomationLinkButton();
        ToolStripButton notificationPortalLinkButton = toolButtonsRender.getNotificationPortalLinkButton();
        ToolStripButton logoutButton = toolButtonsRender.getLogoutButton(login, SchedulerPage.this.controller);

        tools.addMenuButton(portalMenuButton);
        tools.addMenuButton(adminMenuButton);
        tools.addMenuButton(helpMenuButton);
        tools.addSeparator();
        tools.addButton(submitButton);
        tools.addButton(errorButton);
        tools.addFill();
        tools.addButton(studioLinkButton);
        tools.addSpacer(12);
        tools.addButton(resourceManagerLinkButton);
        tools.addSpacer(12);
        tools.addButton(cloudAutomationLinkButton);
        tools.addSpacer(12);
        tools.addButton(notificationPortalLinkButton);
        tools.addSpacer(2);
        tools.addSeparator();
        tools.addSpacer(2);
        tools.addButton(logoutButton);
        tools.addSpacer(10);

        // disable all controls at first, next event will sort it out
        this.statusChanged(SchedulerStatus.KILLED);

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

        switch (status) {
            case SHUTTING_DOWN:
            case KILLED:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(false);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(false);
                break;
            case FROZEN:
            case PAUSED:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(true);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(true);
                schedKillButton.setEnabled(true);
                break;
            case STARTED:
            case UNLINKED:
                schedStartButton.setEnabled(false);
                schedStopButton.setEnabled(true);
                schedFreezeButton.setEnabled(true);
                schedPauseButton.setEnabled(true);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(true);
                break;
            case STOPPED:
                schedStartButton.setEnabled(true);
                schedStopButton.setEnabled(false);
                schedFreezeButton.setEnabled(false);
                schedPauseButton.setEnabled(false);
                schedResumeButton.setEnabled(false);
                schedKillButton.setEnabled(true);
                break;
        }
        this.adminMenu.redraw();
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

        leftTabSet.addTabSelectedHandler(new TabSelectedHandler() {
            public void onTabSelected(TabSelectedEvent event) {
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
                    JobsModel jobsModel = ((SchedulerModelImpl) controller.getModel()).getExecutionsModel()
                            .getJobsModel();
                    if (jobsModel.getSelectedJob() != null) {
                        controller.visuFetch(jobsModel.getSelectedJob().getId().toString());
                    }
                } else {
                    controller.setVisuFetchEnabled(false);
                }
            }
        });

        TabSet rightTabSet = new TabSet();
        rightTabSet.setWidth("50%");
        rightTabSet.setHeight100();
        rightTabSet.setTabBarPosition(Side.TOP);

        Tab jobinfoTab = new Tab("Job Info", SchedulerImages.instance.info_16().getSafeUri().asString());
        this.jobInfo = new JobInfoView(this.controller, new JobsDetailColumnsFactory());
        jobinfoTab.setPane(this.jobInfo.build());

        Tab taskinfoTab = new Tab("Task Info", SchedulerImages.instance.info_16().getSafeUri().asString());
        this.taskInfo = new TaskInfoView(this.controller, new TaskDetailColumnsFactory());
        taskinfoTab.setPane(this.taskInfo.build());

        Tab outputTab = new Tab("Output", SchedulerImages.instance.output_16().getSafeUri().asString());
        outputTab.setPane(this.controller.buildOutputView());

        Tab serverLogsTab = new Tab("Server Logs",
            SchedulerImages.instance.output_16().getSafeUri().asString());
        serverLogsTab.setPane(this.controller.buildServerLogsView());

        Tab resultTab = new Tab("Preview", Images.instance.search_16().getSafeUri().asString());
        resultTab.setPane(this.controller.buildPreviewView());

        rightTabSet.addTab(jobinfoTab);
        rightTabSet.addTab(taskinfoTab);
        rightTabSet.addTab(outputTab);
        rightTabSet.addTab(serverLogsTab);
        rightTabSet.addTab(resultTab);

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

        this.rootLayout = null;
        this.jobInfo = null;
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
        long dt = System.currentTimeMillis() - this.lastCriticalMessage;
        if (dt > SchedulerConfig.get().getClientRefreshTime() * 4) {
            this.errorButton.hide();
        }
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
                leftTabSet.addTab(this.tasksTab, 0);
                leftTabSet.addTab(this.visuTab, 1);
                break;
            case TASK_CENTRIC:
                leftTabSet.updateTab(tasksTab, null);
                leftTabSet.removeTab(tasksTab);
                leftTabSet.updateTab(visuTab, null);
                leftTabSet.removeTab(visuTab);
        }
        leftTabSet.markForRedraw();
    }

    protected void buildTasksTab() {
        tasksTab = new Tab("Tasks", SchedulerImages.instance.monitoring_16().getSafeUri().asString());

        tasksTab.setPane(tasksPane);
    }

    protected void buildVisuTab() {
        visuTab = new Tab("Visualization", ImagesUnbundled.PA_16);
        visuTab.setPane(this.visuPane);
    }

}
