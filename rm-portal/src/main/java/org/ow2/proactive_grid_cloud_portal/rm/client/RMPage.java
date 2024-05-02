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
package org.ow2.proactive_grid_cloud_portal.rm.client;

import org.ow2.proactive_grid_cloud_portal.common.client.*;
import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LogModel;
import org.ow2.proactive_grid_cloud_portal.common.client.model.LoginModel;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.MonitoringHostView;
import org.ow2.proactive_grid_cloud_portal.rm.client.monitoring.views.compact.CompactView;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.NodeSourceWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.creation.CreateNodeSourceWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.EditDynamicParametersWindow;
import org.ow2.proactive_grid_cloud_portal.rm.client.nodesource.edition.EditNodeSourceWindow;
import org.ow2.proactive_grid_cloud_portal.rm.shared.RMConfig;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Style.Unit;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.ImgButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.layout.*;
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
 * Page shown when an user is logged
 *
 * @author mschnoor
 *
 */
public class RMPage implements LogListener {

    private static final int EXPAND_COLLAPSE_SECTION_LABEL_WIDTH = 80;

    private RMController controller = null;

    /** parent of all widgets held by this page */
    private Canvas rootLayout = null;

    /** displays all logs received from the event dispatcher */
    private LogWindow logWindow = null;

    /** about this app */
    private AboutWindow aboutWindow = null;

    /** client settings */
    private SettingsWindow settingsWindow = null;

    /** logger settings */
    private LoggersWindow loggersWindow = null;

    /** create and download credentials files */
    private CredentialsWindow credentialsWindow = null;

    /** manage node sources */
    private NodeSourceWindow nsWindow = null;

    /** node launcher window */
    private AddNodeWindow addNodeWindow = null;

    /** top pane : textual list of nodes */
    private TreeView treeView = null;

    /** bot left pane : selected node info */
    private InfoView infoView = null;

    /** bot left pane : selected node monitoring */
    private MonitoringView monitoringView = null;

    /** bot right pane : graphical list of nodes */
    private CompactView compactView = null;

    /** bot left pane : node count */
    private StatisticsView statsView = null;

    private ScriptConsoleView scriptConsoleView = null;

    private ThreadDumpView threadDumpView = null;

    /** bot right : graphs */
    private RMStatsView rmStatsView = null;

    /** displayed when critical log events occur */
    private ToolStripButton errorButton = null;

    /** user's account information */
    private AccountInfoWindow accountInfoWindow = null;

    private long lastCriticalMessage = 0;

    // Logo strip properties
    private int logoStripHeight = 40;

    private static final String LOGO_STRIP_BACKGROUND_COLOR = "#fafafa";

    private String logoStripBorder = "0px";

    private ToolButtonsRender toolButtonsRender = new ToolButtonsRender();

    private ToolStripButton nsButton;

    private Tab monitoringTab;

    private Tab scriptConsoleTab;

    private Tab threadDumpTab;

    private MenuItem loggersMenuItem;

    private ToolStrip paShortcutsStrip;

    private ToolStrip logoStrip;

    private Label errorLabel = null;

    public static final String NOT_EMPTY_NS_VIEW = "not.empty.ns.view";

    RMPage(RMController controller) {
        this.controller = controller;
        LogModel.getInstance().addLogListener(this);

        buildAndShow();
    }

    private void buildAndShow() {
        VLayout rl = new VLayout();
        this.rootLayout = rl;
        rl.setWidth100();
        rl.setHeight100();
        rl.setBackgroundColor(LOGO_STRIP_BACKGROUND_COLOR);

        this.aboutWindow = new AboutWindow(GWT.getHostPageBaseURL().replace("/rm/", "/rest/"));
        this.addNodeWindow = new AddNodeWindow();
        this.settingsWindow = new SettingsWindow(controller);
        this.loggersWindow = new LoggersWindow(controller);
        this.credentialsWindow = new CredentialsWindow();
        this.accountInfoWindow = new AccountInfoWindow(controller);

        final Canvas header = buildTools();
        HorizontalPanel panel = new HorizontalPanel();
        panel.setWidth("100%");
        panel.setHeight("3px");
        panel.getElement().getStyle().setBackgroundColor("#f47930");
        panel.getElement().getStyle().setPadding(-1, Unit.PX);

        Canvas topPane = buildTopPane();
        topPane.setWidth100();
        topPane.setHeight100();
        Canvas botPane = buildBotPane();
        botPane.setWidth100();
        botPane.setHeight100();

        SectionStackSection topSection = new SectionStackSection();
        topSection.setTitle("Nodes");
        topSection.setExpanded(true);
        topSection.setItems(topPane);

        ImgButton expandButton = new ImgButton();
        expandButton.setWidth(16);
        expandButton.setHeight(16);
        expandButton.setSrc(Images.instance.expand_16().getSafeUri().asString());
        expandButton.setTooltip("Expand all items");
        expandButton.addClickHandler(event -> RMPage.this.treeView.expandAll());
        expandButton.setShowFocusedIcon(false);
        expandButton.setShowRollOver(false);
        expandButton.setShowDown(false);
        expandButton.setLayoutAlign(Alignment.CENTER);

        final CheckboxItem c1 = new CheckboxItem("mynodes", "My nodes");
        c1.setValue(false);
        c1.addChangedHandler(event -> compactView.setViewMyNodes(c1.getValueAsBoolean()));

        // for some reason IE9 standards fails to detect the right width
        if (SC.isIE()) {
            c1.setWidth(60);
        }

        DynamicForm checkBoxes = new DynamicForm();
        checkBoxes.setNumCols(8);
        checkBoxes.setItems(c1);

        final CheckboxItem hideEmpty = new CheckboxItem("emptyNs", "Hide Empty Node Sources");
        hideEmpty.setValue(Boolean.parseBoolean(Settings.get().getSetting(NOT_EMPTY_NS_VIEW)));
        hideEmpty.addChangedHandler(event -> {
            Settings.get().setSetting(NOT_EMPTY_NS_VIEW, String.valueOf(hideEmpty.getValueAsBoolean()));
            treeView.setNotEmptyNsView(hideEmpty.getValueAsBoolean());
        });

        DynamicForm hideEmptyForm = new DynamicForm();
        hideEmptyForm.setNumCols(8);
        hideEmptyForm.setTop(20);
        hideEmptyForm.setItems(hideEmpty);

        ImgButton closeButton = new ImgButton();
        closeButton.setWidth(16);
        closeButton.setHeight(16);
        closeButton.setSrc(Images.instance.close_16().getSafeUri().asString());
        closeButton.setTooltip("Collapse all items");
        closeButton.addClickHandler(event -> RMPage.this.treeView.closeAll());
        closeButton.setShowFocusedIcon(false);
        closeButton.setShowRollOver(false);
        closeButton.setShowDown(false);
        closeButton.setLayoutAlign(Alignment.CENTER);

        // left align expand collapse buttons, right align mynodes
        final HLayout controls = new HLayout();
        controls.addMember(expandButton);
        controls.addMember(closeButton);
        controls.addMember(hideEmptyForm);
        controls.addMember(new LayoutSpacer());
        controls.addMember(checkBoxes);
        controls.setLayoutAlign(VerticalAlignment.CENTER);
        topSection.setControls(controls);

        SectionStackSection botSection = new SectionStackSection();
        botSection.setTitle("Details");
        botSection.setExpanded(true);
        botSection.setItems(botPane);

        final SectionStack stack = new SectionStack();
        stack.setWidth100();
        stack.setHeight100();
        stack.setMargin(2);
        stack.setVisibilityMode(VisibilityMode.MULTIPLE);
        stack.setAnimateSections(true);
        stack.setOverflow(Overflow.HIDDEN);
        stack.setSections(topSection, botSection);

        // dynamic width of controls layout
        stack.addResizedHandler(event -> controls.setWidth(stack.getWidth() - EXPAND_COLLAPSE_SECTION_LABEL_WIDTH));
        stack.addDrawHandler(event -> controls.setWidth(stack.getWidth() - EXPAND_COLLAPSE_SECTION_LABEL_WIDTH));

        rl.addMember(buildLogoStrip());
        rl.addMember(header);
        rl.addMember(panel);
        rl.addMember(stack);

        this.logWindow = new LogWindow(controller);
        MonitoringHostView.setPreviousRange(Model.StatHistory.Range.MINUTE_1);

        this.rootLayout.draw();
    }

    private ToolStrip buildLogoStrip() {

        // Activeeon Logo
        ToolStrip logoAE = new ToolStrip();
        logoAE.setHeight(logoStripHeight);
        logoAE.setWidth("33%");
        logoAE.setBackgroundImage("");
        logoAE.setBackgroundColor(LOGO_STRIP_BACKGROUND_COLOR);
        logoAE.setMargin(0);
        logoAE.setBorder(logoStripBorder);
        logoAE.setAlign(Alignment.LEFT);
        logoAE.setStyleName("brand-logo");
        logoAE.setTop(0);
        Img logoImg = new Img(RMImagesUnbundled.AE_LOGO, 146, logoStripHeight);
        logoImg.addClickHandler(clickEvent -> Window.open("http://activeeon.com/", "", ""));
        logoAE.addMember(logoImg);

        paShortcutsStrip = buildShortcutStrip(true, true, true, true);

        // Navbar Header
        logoStrip = new ToolStrip();
        logoStrip.setHeight(logoStripHeight);
        logoStrip.setWidth100();
        logoStrip.setBackgroundImage("");
        logoStrip.setBackgroundColor(LOGO_STRIP_BACKGROUND_COLOR);
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
        ToolStripButton schedulerLinkButton = toolButtonsRender.getSchedulerLinkButton();
        ToolStripButton resourceManagerLinkButton = toolButtonsRender.getResourceManagerHighlightedLinkButton();
        ToolStripButton logoutButton = toolButtonsRender.getLogoutButton(login, RMPage.this.controller);

        // Shortcut buttons strip
        ToolStrip paShortcutsStrip = new ToolStrip();
        Img customBrandLogo = new Img("../../assets/image/" + RMImagesUnbundled.EXTRA_LOGO_CENTER,
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
        paShortcutsStrip.setBackgroundColor(LOGO_STRIP_BACKGROUND_COLOR);
        paShortcutsStrip.setBorder(logoStripBorder);
        paShortcutsStrip.setAlign(Alignment.RIGHT);
        paShortcutsStrip.setStyleName("pa-shortcuts");
        paShortcutsStrip.setPosition("static");
        paShortcutsStrip.setAlign(Alignment.RIGHT);
        return paShortcutsStrip;
    }

    private ToolStrip buildTools() {
        ToolStrip tools = new ToolStrip();
        tools.setHeight(50);
        tools.setWidth100();
        tools.setBackgroundImage("");
        tools.setBackgroundColor(LOGO_STRIP_BACKGROUND_COLOR);
        tools.setBorder("0px");

        MenuItem settingsMenuItem = new MenuItem("Settings", Images.instance.settings_16().getSafeUri().asString());
        settingsMenuItem.addClickHandler(event -> RMPage.this.settingsWindow.show());

        loggersMenuItem = new MenuItem("Loggers", Images.instance.log_16().getSafeUri().asString());
        loggersMenuItem.addClickHandler(event -> RMPage.this.loggersWindow.show());

        MenuItem credMenuItem = new MenuItem("Create credentials", Images.instance.key_16().getSafeUri().asString());
        credMenuItem.addClickHandler(event -> RMPage.this.credentialsWindow.show());

        MenuItem nodeMenuItem = new MenuItem("Launch a Node", ImagesUnbundled.PA_16);
        nodeMenuItem.addClickHandler(event -> RMPage.this.addNodeWindow.show());

        MenuItem logoutMenuItem = new MenuItem("Logout", Images.instance.exit_18().getSafeUri().asString());
        logoutMenuItem.addClickHandler(event -> SC.confirm("Logout", "Are you sure you want to exit?", value -> {
            if (value) {
                RMPage.this.controller.logout();
            }
        }));

        ToolStripMenuButton portalMenuButton = new ToolStripMenuButton("Portal");
        Menu portalMenu = new Menu();
        portalMenu.setItems(credMenuItem,
                            nodeMenuItem,
                            settingsMenuItem,
                            loggersMenuItem,
                            new MenuItemSeparator(),
                            logoutMenuItem);
        portalMenuButton.setMenu(portalMenu);

        MenuItem logMenuItem = new MenuItem("Display logs", Images.instance.log_16().getSafeUri().asString());
        logMenuItem.addClickHandler(e -> showLogWindow());

        MenuItem documentationMenuItem = new MenuItem("Documentation",
                                                      Images.instance.icon_manual().getSafeUri().asString());
        documentationMenuItem.addClickHandler(event -> Window.open(controller.getAbsoluteUrlFromRelativePath("doc/"),
                                                                   "Documentation",
                                                                   ""));

        MenuItem aboutMenuItem = new MenuItem("About", Images.instance.about_16().getSafeUri().asString());
        aboutMenuItem.addClickHandler(event -> RMPage.this.aboutWindow.show());

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

        nsButton = new ToolStripButton("Add Node Source");
        nsButton.setIcon(ImagesUnbundled.NODESOURCE_DEPLOYED);
        nsButton.setTooltip("Create and add a new Node Source");
        nsButton.addClickHandler(e -> showNodeSourceCreationWindow());

        errorButton = new ToolStripButton("<strong>Error</strong>",
                                          Images.instance.net_error_16().getSafeUri().asString());
        errorButton.setBackgroundColor("#ffbbbb");
        errorButton.addClickHandler(e -> showLogWindow());
        errorButton.hide();

        errorLabel = new Label();
        errorLabel.setWidth100();
        errorLabel.hide();

        ToolStripButton accountInfoButton = new ToolStripButton("Account Info");
        accountInfoButton.setTooltip("User's account info");
        accountInfoButton.addClickHandler(event -> RMPage.this.accountInfoWindow.show());

        tools.addMenuButton(portalMenuButton);
        tools.addMenuButton(helpMenuButton);
        tools.addSeparator();
        tools.addButton(nsButton);
        tools.addButton(errorButton);
        tools.addMember(errorLabel);
        tools.addFill();
        tools.addMember(accountInfoButton);

        return tools;
    }

    public void showErrorLabel(String message) {
        errorLabel.setContents("<span style='color:red; font-weight: bold; font-size:14px; padding-left:4px;'> " +
                               message + "</span>");
        errorLabel.show();
    }

    public void disableNSButton() {
        nsButton.setTooltip("User is not authorized to create or add a new Node Source");
        nsButton.setDisabled(true);
    }

    public void disableLoggersMenuItem() {
        loggersMenuItem.setEnabled(false);
    }

    private Canvas buildTopPane() {
        HLayout hl = new HLayout();
        hl.setWidth100();
        hl.setHeight100();

        this.treeView = new TreeView(controller);
        this.compactView = new CompactView(controller);

        Canvas compactCanvas = this.compactView.build(treeView);
        compactCanvas.setWidth("50%");
        Canvas treeCanvas = this.treeView.build(compactView);
        treeCanvas.setWidth("50%");
        treeCanvas.setShowResizeBar(true);

        hl.addMember(treeCanvas);
        hl.addMember(compactCanvas);

        return hl;
    }

    private Canvas buildBotPane() {
        final HLayout hl = new HLayout();
        hl.setWidth100();
        hl.setHeight100();

        // Left tabs (bottom left)
        final TabSet leftTabs = new TabSet();
        leftTabs.setWidth("50%");
        leftTabs.setShowResizeBar(true);

        // "Selection" tab
        this.infoView = new InfoView(controller);
        final Canvas infoCanvas = this.infoView.build();
        Tab selectionTab = new Tab("Selection");
        selectionTab.setPane(infoCanvas);
        leftTabs.addTab(selectionTab);

        // "Nodes" tab
        this.statsView = new StatisticsView(controller);
        Canvas statsCanvas = this.statsView.build();
        Tab nodesTab = new Tab("Nodes");
        nodesTab.setPane(statsCanvas);
        leftTabs.addTab(nodesTab);

        // "Script Console" tab
        this.scriptConsoleView = new ScriptConsoleView(controller);
        Canvas scriptConsoleCanvas = this.scriptConsoleView.build();
        scriptConsoleTab = new Tab("Script Console");
        scriptConsoleTab.setPane(scriptConsoleCanvas);
        leftTabs.addTab(scriptConsoleTab);

        // "Thread Dump" tab
        this.threadDumpView = new ThreadDumpView(controller);
        Canvas threadDumpCanvas = this.threadDumpView.build();
        threadDumpTab = new Tab("Thread Dump");
        threadDumpTab.setPane(threadDumpCanvas);
        leftTabs.addTab(threadDumpTab);

        // "Monitoring" tab
        this.monitoringView = new MonitoringView(controller);
        Canvas monitoringCanvas = monitoringView.build();
        monitoringTab = new Tab("Monitoring");
        monitoringTab.setPane(monitoringCanvas);
        leftTabs.addTab(monitoringTab);

        // ADD the left tabs to the layout
        hl.addMember(leftTabs);

        // Stats view
        this.rmStatsView = new RMStatsView(controller);
        final Canvas rmStatsCanvas = rmStatsView.build();
        rmStatsCanvas.setWidth("50%");

        // ADD the right view to the layout
        hl.addMember(rmStatsCanvas);

        return hl;
    }

    public void setScriptConsoleTabPageDisabled(boolean disabled) {
        scriptConsoleTab.setDisabled(disabled);
    }

    public void setMonitoringTabPageDisabled(boolean disabled) {
        monitoringTab.setDisabled(disabled);
    }

    public void setThreadDumpTabPageDisabled(boolean disabled) {
        threadDumpTab.setDisabled(disabled);
    }

    public void rebuildShortcutStrip(boolean automationDashboard, boolean studio, boolean scheduler, boolean rm) {
        logoStrip.removeMember(paShortcutsStrip);
        paShortcutsStrip = buildShortcutStrip(automationDashboard, studio, scheduler, rm);
        logoStrip.addMember(paShortcutsStrip);
        logoStrip.redraw();
    }

    void destroy() {
        this.rootLayout.destroy();
        this.logWindow.destroy();
        this.credentialsWindow.destroy();
        this.settingsWindow.destroy();
        this.loggersWindow.destroy();
        this.aboutWindow.destroy();
        this.addNodeWindow.destroy();
        this.accountInfoWindow.destroy();
        this.destroyNodeSourceWindow();
    }

    @Override
    public void logMessage(String message) {
        long dt = System.currentTimeMillis() - this.lastCriticalMessage;
        if (dt > RMConfig.get().getClientRefreshTime() * 4) {
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

    public void showNodeSourceCreationWindow() {
        destroyNodeSourceWindow();
        RMPage.this.nsWindow = new CreateNodeSourceWindow(controller);
        RMPage.this.nsWindow.show();
    }

    public void showEditNodeSourceWindow(String nodeSourceName) {
        destroyNodeSourceWindow();
        RMPage.this.nsWindow = new EditNodeSourceWindow(controller, nodeSourceName);
        RMPage.this.nsWindow.show();
    }

    public void showEditDynamicParametersWindow(String nodeSourceName) {
        destroyNodeSourceWindow();
        RMPage.this.nsWindow = new EditDynamicParametersWindow(controller, nodeSourceName);
        RMPage.this.nsWindow.show();
    }

    private void destroyNodeSourceWindow() {
        if (RMPage.this.nsWindow != null) {
            RMPage.this.nsWindow.destroy();
        }
    }

    private void showLogWindow() {
        RMPage.this.logWindow.show();
        errorButton.hide();
    }
}
