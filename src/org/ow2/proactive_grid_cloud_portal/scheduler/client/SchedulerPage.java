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
package org.ow2.proactive_grid_cloud_portal.scheduler.client;

import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.common.client.AboutWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.CredentialsWindow;
import org.ow2.proactive_grid_cloud_portal.common.client.Images;
import org.ow2.proactive_grid_cloud_portal.common.client.Listeners.LogListener;
import org.ow2.proactive_grid_cloud_portal.common.client.LogWindow;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.SchedulerStatusListener;
import org.ow2.proactive_grid_cloud_portal.scheduler.shared.SchedulerConfig;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.types.Side;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.types.VisibilityMode;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.Img;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.MouseOutEvent;
import com.smartgwt.client.widgets.events.MouseOutHandler;
import com.smartgwt.client.widgets.events.MouseOverEvent;
import com.smartgwt.client.widgets.events.MouseOverHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.CheckboxItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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
public class SchedulerPage implements SchedulerStatusListener, JobsUpdatedListener, LogListener {

	static SchedulerPage inst;

	// this is ugly but I need this stupid object for the visu view
	// to control the scroll viewport of tab's 'paneContainer' which is
	// not accessible anywhere
	Tab visuTab;

	/** root layout: parent to all widgets of this view */
	private Layout rootLayout = null;

	/** grid displaying the jobs */
	private JobsView jobGrid = null;
	/** view displaying info about the selected job */
	private JobInfoView jobInfo = null;
	/** list of tasks for the selected job in jobGrid */
	private TasksView tasksView = null;
	/** output for the selected job */
	private OutputView outputView = null;
	/** result download view */
	private ResultView resultView = null;
	/** displays connected users */
	private UsersView usersView = null;
	/** displays scheduler & accounting stats */
	private StatisticsView statsView = null;
	/** job visualization */
	private VisualizationView visuView = null;
	/** logs for async scheduler callbacks */
	private LogWindow logWindow = null;
	/** about this app */
	private AboutWindow aboutWindow = null;
	/** client settings */
	private SettingsWindow settingsWindow = null;
	/** jobs filtering */
	private Layout filterPane = null;

	private Menu adminMenu = null;

	/** job page number */
	private Label pageLabel = null;
	private ToolStripButton pagePreviousButton = null;
	private ToolStripButton pageNextButton = null;

	/** displayed when critical log events occur */
	private ToolStripButton errorButton = null;
	private long lastCriticalMessage = 0;

	private SchedulerController controller = null;

	/**
	 * Default constructor
	 *
	 * @param controller Controller that created this page
	 */
	public SchedulerPage(SchedulerController controller) {
		this.controller = controller;
		buildAndShow();
		this.controller.getEventDispatcher().addSchedulerStatusListener(this);
		this.controller.getEventDispatcher().addJobsUpdatedListener(this);
		this.controller.getEventDispatcher().addLogListener(this);
		inst = this;
	}

	/**
	 * Clicked next/previous page, set inderterminate state until {@link #jobsUpdated(JobSet)}
	 */
	private void pageChanged() {
		int page = controller.getModel().getJobPage();
		int size = controller.getModel().getJobPageSize();
		this.pageNextButton.disable();
		this.pagePreviousButton.disable();

		String str = "" + (page * size + 1) + " - " + ((page + 1) * size);
		this.pageLabel.setContents(str);
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.JobsUpdatedListener#jobsUpdated(org.ow2.proactive_grid_cloud_portal.shared.job.JobSet)
	 */
	public void jobsUpdated(Map<Integer, Job> jobs) {
		int page = controller.getModel().getJobPage();
		int size = controller.getModel().getJobPageSize();
		String str = "" + (page * size + 1) + " - " + ((page + 1) * size);
		this.pageLabel.setContents(str);

		this.pageNextButton.disable();
		this.pagePreviousButton.disable();

		if (page > 0)
			this.pagePreviousButton.enable();

		if (jobs != null && jobs.size() == controller.getModel().getJobPageSize())
			this.pageNextButton.enable();
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.JobsUpdatedListener#jobsUpdating()
	 */
	public void jobsUpdating() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.JobsUpdatedListener#jobSubmitted(org.ow2.proactive_grid_cloud_portal.client.Job)
	 */
	public void jobSubmitted(Job j) {
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

		final Layout topPane = buildTopPane();

		SectionStackSection jobsSection = new SectionStackSection();
		jobsSection.setTitle("Jobs list");
		jobsSection.setExpanded(true);
		jobsSection.setItems(topPane);

		this.pageNextButton = new ToolStripButton("Next >");
		this.pageNextButton.disable();
		this.pageNextButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				controller.nextPage();
				pageChanged();
			}
		});
		this.pagePreviousButton = new ToolStripButton("< Previous");
		this.pagePreviousButton.disable();
		this.pagePreviousButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				controller.previousPage();
				pageChanged();
			}
		});
		this.pageLabel = new Label("");
		this.pageLabel.setAlign(Alignment.CENTER);
		this.pageLabel.setWidth(60);
		this.pageLabel.setMargin(0);
		this.pageLabel.setPadding(0);

		final CheckboxItem c1 = new CheckboxItem("myjobs", "My jobs");
		c1.setValue(false);
		c1.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				controller.fetchMyJobsOnly(c1.getValueAsBoolean());
			}
		});
		final CheckboxItem c2 = new CheckboxItem("pending", "Pending");
		c2.setValue(true);
		c2.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				controller.fetchPending(c2.getValueAsBoolean());
			}
		});
		final CheckboxItem c3 = new CheckboxItem("running", "Running");
		c3.setValue(true);
		c3.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				controller.fetchRunning(c3.getValueAsBoolean());
			}
		});
		final CheckboxItem c4 = new CheckboxItem("finished", "Finished");
		c4.setValue(true);
		c4.addChangedHandler(new ChangedHandler() {
			public void onChanged(ChangedEvent event) {
				controller.fetchFinished(c4.getValueAsBoolean());
			}
		});

		// for some reason IE9 standards fails to detect the right width
		if (SC.isIE()) {
			c1.setWidth(60);
			c2.setWidth(60);
			c3.setWidth(60);
			c4.setWidth(60);
		}

		DynamicForm checkBoxes = new DynamicForm();
		checkBoxes.setNumCols(8);
		checkBoxes.setItems(c1, c2, c3, c4);

		String user = controller.getModel().getLogin();
		// login unknown: credentials login; fetching only my jobs will be impossible server side
		if (user == null || user.trim().length() == 0) {
			c1.setDisabled(true);
		}

		Canvas fill = new Canvas();
		fill.setWidth(5);
		jobsSection.setControls(checkBoxes, fill, pagePreviousButton, pageLabel, pageNextButton);
		this.pageChanged();

		Canvas tools = buildTools();

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
		stack.setSections(jobsSection, detailsSection);

		contentLayout.addMember(tools);
		contentLayout.addMember(stack);
		this.logWindow = new LogWindow(controller);

		this.rootLayout.draw();
	}

	/** admin scheduler functionalities */
	private MenuItem schedStartButton, schedStopButton, schedFreezeButton, schedResumeButton,
			schedPauseButton, schedKillButton;

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
		tools.setHeight(34);
		tools.setWidth100();
		tools.setBackgroundImage("");
		tools.setBackgroundColor("#fafafa");
		tools.setBorder("0px");

		MenuItem submitMenuItem = new MenuItem("Submit job", SchedulerImages.instance.job_submit_16()
				.getSafeUri().asString());
		submitMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SubmitWindow w = new SubmitWindow(SchedulerPage.this.controller);
				w.show();
			}
		});
		MenuItem flatSubmitMenuItem = new MenuItem("Submit command file", SchedulerImages.instance
				.script_16().getSafeUri().asString());
		flatSubmitMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				FlatSubmitWindow w = new FlatSubmitWindow(SchedulerPage.this.controller);
				w.show();
			}
		});

		MenuItem settingsMenuItem = new MenuItem("Settings", Images.instance.settings_16().getSafeUri()
				.asString());
		settingsMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SchedulerPage.this.settingsWindow.show();
			}
		});

		MenuItem credMenuItem = new MenuItem("Create credentials", Images.instance.key_16().getSafeUri()
				.asString());
		credMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				CredentialsWindow credentialsWindow = new CredentialsWindow();
				credentialsWindow.show();
			}
		});

		MenuItem serversMenuItem = new MenuItem("Data servers", Images.instance.server_16().getSafeUri()
				.asString());
		serversMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				String url = GWT.getModuleBaseURL() + "servers?codebase=" + GWT.getHostPageBaseURL();
				Window.open(url, "_blank", "");
			}
		});

		MenuItem logoutMenuItem = new MenuItem("Logout", Images.instance.exit_16().getSafeUri().asString());
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
				serversMenuItem, settingsMenuItem, new MenuItemSeparator(), logoutMenuItem);
		portalMenuButton.setMenu(portalMenu);

		MenuItem logMenuItem = new MenuItem("Display logs", Images.instance.log_16().getSafeUri().asString());
		logMenuItem.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				SchedulerPage.this.logWindow.show();
				errorButton.hide();
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
		helpMenu.setItems(logMenuItem, aboutMenuItem);
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

		String login = this.controller.getModel().getLogin();
		if (login != null)
			login = " <b>" + login + "</b>";
		else
			login = "";

		ToolStripButton logoutButton = new ToolStripButton("Logout" + login);
		logoutButton.setIcon(Images.instance.exit_16().getSafeUri().asString());
		logoutButton.setTooltip("Logout");
		logoutButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				SC.confirm("Logout", "Are you sure you want to exit?", new BooleanCallback() {
					public void execute(Boolean value) {
						if (value) {
							SchedulerPage.this.controller.logout();
						}
					}
				});
			}
		});

		errorButton = new ToolStripButton("<strong>Network error</strong>", Images.instance.net_error_16()
				.getSafeUri().asString());
		errorButton.setBackgroundColor("#ffbbbb");
		errorButton.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SchedulerPage.this.logWindow.show();
				errorButton.hide();
			}
		});
		errorButton.hide();

		tools.addMenuButton(portalMenuButton);
		tools.addMenuButton(adminMenuButton);
		tools.addMenuButton(helpMenuButton);
		tools.addSeparator();
		tools.addButton(submitButton);
		tools.addButton(logoutButton);
		tools.addButton(errorButton);
		tools.addFill();
		tools.addMember(new Img(SchedulerImages.instance.logo_32().getSafeUri().asString(), 154, 32));

		// disable all controls at first, next event will sort it out
		this.statusChanged(SchedulerStatus.KILLED);

		return tools;
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.SchedulerStatusListener#statusChanged(org.ow2.proactive_grid_cloud_portal.shared.SchedulerStatus)
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
	 * Builds and return the top pane: the jobs list and filtering options
	 * 
	 * <pre>
	 * +- HLayout -----------------------------------------------+
	 * |+- ListGrid -----------++--++- VLayout -----------------+|
	 * || JobsView#build()     ||  || JobsView#buildFilterPane()||
	 * ||                      ||>>|| hidden/shown upon click   ||
	 * ||                      ||  || on the '>>' canvas        ||
	 * |+----------------------++--++---------------------------+|
	 * +---------------------------------------------------------+
	 * </pre>
	 */
	private Layout buildTopPane() {
		final HLayout topPane = new HLayout();

		this.jobGrid = new JobsView(this.controller);
		Layout jobs = jobGrid.build();

		Label label = new Label("Use filters to restrict the number of jobs currently displayed.<br><br>"
			+ "Filters apply only to the current page.<br>"
			+ "Use The <strong>&lt;Previous</strong> and <strong>Next&gt;</strong> "
			+ "controls to view more results.");
		label.setHeight(55);

		this.filterPane = new VLayout();
		this.filterPane.setBackgroundColor("#fafafa");
		this.filterPane.addMember(label);
		Layout gridFilter = this.jobGrid.buildFilterPane();
		this.filterPane.setPadding(5);
		this.filterPane.setMembersMargin(10);
		this.filterPane.setOverflow(Overflow.AUTO);
		this.filterPane.addMember(gridFilter);
		this.filterPane.hide();

		final VLayout filterButton = new VLayout();
		filterButton.setBackgroundColor("#fafafa");
		filterButton.setAlign(VerticalAlignment.CENTER);
		filterButton.setWidth(12);
		filterButton.setHeight100();
		final Img filterButtonLabel = new Img(SchedulerImages.instance.section_left_10().getSafeUri()
				.asString(), 10, 13);
		filterButton.addMember(filterButtonLabel);
		filterButton.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				if (!filterPane.isVisible()) {
					filterPane.setWidth(490);
					filterButtonLabel.setSrc(SchedulerImages.instance.section_right_10().getSafeUri()
							.asString());
					topPane.showMember(filterPane);
				} else {
					filterButtonLabel.setSrc(SchedulerImages.instance.section_left_10().getSafeUri()
							.asString());
					topPane.hideMember(filterPane);
				}
			}
		});
		filterButton.addMouseOverHandler(new MouseOverHandler() {
			public void onMouseOver(MouseOverEvent event) {
				filterButton.setBackgroundColor("#eee");
			}
		});
		filterButton.addMouseOutHandler(new MouseOutHandler() {
			public void onMouseOut(MouseOutEvent event) {
				filterButton.setBackgroundColor("#fafafa");
			}
		});

		topPane.setMembers(jobs, filterButton, filterPane);

		return topPane;
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
		final TabSet leftTabSet = new TabSet();
		leftTabSet.setWidth("50%");
		leftTabSet.setHeight100();
		leftTabSet.setTabBarPosition(Side.TOP);
		leftTabSet.setShowResizeBar(true);

		final Tab tasksTab = new Tab("Tasks", SchedulerImages.instance.monitoring_16().getSafeUri()
				.asString());
		this.tasksView = new TasksView(this.controller);
		tasksTab.setPane(this.tasksView.build());

		visuTab = new Tab("Visualization", Images.instance.pa_16().getSafeUri().asString());
		this.visuView = new VisualizationView(this.controller);
		visuTab.setPane(this.visuView.build());

		final Tab usersTab = new Tab("Users", Images.instance.user_16().getSafeUri().asString());
		this.usersView = new UsersView(this.controller);
		usersTab.setPane(this.usersView.build());

		final Tab statsTab = new Tab("Statistics", Images.instance.stats_16().getSafeUri().asString());
		this.statsView = new StatisticsView(this.controller);
		statsTab.setPane(this.statsView.build());

		leftTabSet.addTab(tasksTab);
		leftTabSet.addTab(visuTab);
		leftTabSet.addTab(usersTab);
		leftTabSet.addTab(statsTab);

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
					if (controller.getModel().getSelectedJob() != null) {
						controller.visuFetch(controller.getModel().getSelectedJob().getId().toString());
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
		this.jobInfo = new JobInfoView(this.controller);
		jobinfoTab.setPane(this.jobInfo.build());

		Tab outputTab = new Tab("Output", SchedulerImages.instance.output_16().getSafeUri().asString());
		this.outputView = new OutputView(this.controller);
		outputTab.setPane(this.outputView.build());

		Tab resultTab = new Tab("Preview", Images.instance.search_16().getSafeUri().asString());
		this.resultView = new ResultView(this.controller);
		resultTab.setPane(this.resultView.build());

		rightTabSet.addTab(jobinfoTab);
		rightTabSet.addTab(outputTab);
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
		this.jobGrid = null;
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
}
