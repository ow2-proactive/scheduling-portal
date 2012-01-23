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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.SchedulerListeners.JobsUpdatedListener;

import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.data.DataSource;
import com.smartgwt.client.data.fields.DataSourceIntegerField;
import com.smartgwt.client.data.fields.DataSourceTextField;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.types.SortDirection;
import com.smartgwt.client.types.TopOperatorAppearance;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.FilterBuilder;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.grid.events.CellOutEvent;
import com.smartgwt.client.widgets.grid.events.CellOutHandler;
import com.smartgwt.client.widgets.grid.events.SelectionChangedHandler;
import com.smartgwt.client.widgets.grid.events.SelectionEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.ClickHandler;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;


/**
 * Contains the ListGrid that displays jobs
 *
 *
 * @author mschnoor
 *
 */
public class JobsView implements JobsUpdatedListener {

	private static final String ID_ATTR = "id";
	private static final String STATE_ATTR = "state";
	private static final String USER_ATTR = "user";
	private static final String PRIORITY_ATTR = "priority";
	private static final String NAME_ATTR = "name";
	private static final String PROGRESS_ATTR = "progress";
	private static final String DURATION_ATTR = "duration";

	/**
	 * Entries in the Job Grid
	 *
	 */
	private class JobRecord extends ListGridRecord {

		private Job job;

		public JobRecord(Job j) {
			setAttribute(ID_ATTR, j.getId());
			this.job = j;
			update(j);
		}

		/**
		 * Updates the attributes of this record to reflect the fields of the job <code>j</code>
		 *
		 * @param j a Job
		 */
		public void update(Job j) {
			this.job = j;

			float progress = (float) j.getFinishedTasks() / (float) j.getTotalTasks();
			long duration = -1;
			if (j.getFinishTime() > 0 && j.getStartTime() > 0) {
				duration = j.getFinishTime() - j.getStartTime();
			}

			setAttribute(STATE_ATTR, j.getStatus().toString());
			setAttribute(USER_ATTR, j.getUser());
			setAttribute(PRIORITY_ATTR, j.getPriority().toString());
			setAttribute(NAME_ATTR, j.getName());
			setAttribute(DURATION_ATTR, duration);
			setAttribute(PROGRESS_ATTR, progress);
		}
	}

	/**
	 * DataStore for the Job Grid
	 *
	 */
	private class JobDS extends DataSource {

		public JobDS(String id) {
			setID(id);

			DataSourceIntegerField idF = new DataSourceIntegerField(ID_ATTR);
			idF.setPrimaryKey(true);

			DataSourceTextField stateF = new DataSourceTextField(STATE_ATTR, "State");
			stateF.setRequired(true);
			String[] states = new String[JobStatus.values().length];
			for (int i = 0; i < JobStatus.values().length; i++)
				states[i] = JobStatus.values()[i].toString();
			stateF.setValueMap(states);

			DataSourceTextField userF = new DataSourceTextField(USER_ATTR, "User");
			userF.setRequired(true);

			DataSourceTextField priorF = new DataSourceTextField(PRIORITY_ATTR, "Priority");
			priorF.setRequired(true);
			states = new String[JobPriority.values().length];
			for (int i = 0; i < JobPriority.values().length; i++)
				states[i] = JobPriority.values()[i].toString();
			priorF.setValueMap(states);

			DataSourceTextField nameF = new DataSourceTextField(NAME_ATTR, "Name");
			nameF.setRequired(true);
			DataSourceTextField progressF = new DataSourceTextField(PROGRESS_ATTR, "Progress");
			progressF.setRequired(true);
			DataSourceTextField durF = new DataSourceTextField(DURATION_ATTR, "Duration");
			durF.setRequired(true);

			setFields(idF, stateF, userF, priorF, nameF, progressF, durF);

			setClientOnly(true);
		}

	}

	/** the Grid widget displayed in the view */
	private ListGrid jobsGrid = null;
	/** shown in undeterminate state */
	private Label jobsLoading = null;
	/** data-source: contains the actual data */
	private JobDS ds = null;
	/** ui panel used to edit filters */
	private FilterBuilder filterBuilder = null;
	/** current job filtering criteria, or null */
	private AdvancedCriteria filter = null;
	/** the JobSet currently stored in the DataStore */
	private Map<Integer, Job> oldJobs = null;
	/** jobs submitted locally, to display even though not in the jobs list */
	private Map<Integer, JobRecord> submittingJobs = null;
	/** id of the currently selected job */
	private int selJobId = 0;

	private SchedulerController controller = null;

	/**
	 * Default Constructor
	 *
	 * @param controller Controller used to create this view
	 */
	public JobsView(SchedulerController controller) {
		this.controller = controller;
		this.controller.getEventDispatcher().addJobsUpdatedListener(this);
		this.submittingJobs = new HashMap<Integer, JobRecord>();
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.JobsUpdatedListener#jobsUpdating()
	 */
	public void jobsUpdating() {
		this.jobsGrid.hide();
		this.jobsLoading.show();
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.Listeners.JobsUpdatedListener#jobSubmitted(org.ow2.proactive_grid_cloud_portal.client.Job)
	 */
	public void jobSubmitted(Job j) {
		JobRecord jr = new JobRecord(j);
		this.submittingJobs.put(j.getId(), jr);
		this.ds.addData(jr);
		this.jobsGrid.filterData(this.filter);
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.proactive_grid_cloud_portal.client.event.JobsUpdatedListener#jobsUpdated(org.ow2.proactive_grid_cloud_portal.shared.job.JobSet)
	 */
	public void jobsUpdated(Map<Integer, Job> jobs) {
		boolean resetData = (this.oldJobs == null || this.oldJobs.isEmpty() || jobs.isEmpty());
		boolean keepSel = false;

		// no data is present, or the entire dataset has been invalidated
		// insert all data at once
		if (resetData) {
			JobRecord[] data = new JobRecord[jobs.size()];
			int i = 0;
			for (Job j : jobs.values()) {
				data[i] = new JobRecord(j);

				if (data[i].getAttribute(ID_ATTR).equals("" + selJobId))
					keepSel = true;

				i++;
			}

			this.jobsGrid.invalidateCache();
			this.ds.setTestData(data);
		}

		// for each entry on the jobset, find out which is to add, update or remove
		// alter the datasource accordingly 
		else {
			keepSel = true;

			for (Job newJob : jobs.values()) {
				Job oldJob = this.oldJobs.get(newJob.getId());

				// data present in both sets: either the same, or the be updated
				if (oldJob != null) {
					if (!newJob.isEqual(oldJob)) {

						JobRecord rec = null;

						JobRecord selRec = (JobRecord) jobsGrid.getSelectedRecord();
						if (selRec != null && selRec.getAttribute(ID_ATTR).equals("" + newJob.getId())) {
							// avoid changing reference to the selected entry, messes selection beyond repair
							selRec.update(newJob);
							rec = selRec;
							selRec.setAttribute("isSelected", true);
						} else {
							rec = new JobRecord(newJob);
							rec.setAttribute("isSelected", false);
						}

						if (this.submittingJobs.remove(newJob.getId()) != null) {
							if (selRec != null && selRec.getAttribute(ID_ATTR).equals("" + newJob.getId())) {
								controller.selectJob(rec.getAttribute(ID_ATTR));
							}
						}

						// data differs, update
						this.ds.updateData(rec);
					}
					this.oldJobs.remove(newJob.getId());
				}
				// data present in the new set only: was added since last time
				else {
					JobRecord oldSub = this.submittingJobs.remove(newJob.getId());
					// remove submitting job entry
					if (oldSub != null) {
						this.ds.removeData(oldSub);
					}

					JobRecord njr = new JobRecord(newJob);
					this.ds.addData(njr);
				}
			}
			// the difference between the new and old sets: the old entries no longer present in the model, to be removed
			for (Job toRemove : this.oldJobs.values()) {
				// submitting job : not included on server yet..
				if (this.submittingJobs.containsKey(toRemove.getId())) {
					continue;
				}

				this.ds.removeData(new JobRecord(toRemove));

				// removed selection
				if (selJobId == toRemove.getId()) {
					keepSel = false;
				}
			}
		}

		this.oldJobs = jobs;

		if (!keepSel) {
			this.controller.selectJob(null);
			selJobId = 0;
		}

		this.jobsGrid.filterData(this.filter);
		this.jobsLoading.hide();
		this.jobsGrid.show();
	}

	/**
	 * Construct and return the pane used to filter the job's datasource
	 * 
	 * @return a widget for filtering the grid
	 */
	Layout buildFilterPane() {
		VLayout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();

		filterBuilder = new FilterBuilder();
		filterBuilder.setDataSource(this.ds);
		filterBuilder.setTopOperatorAppearance(TopOperatorAppearance.RADIO);

		IButton ok = new IButton("Apply");
		ok.setHeight(20);
		ok.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(ClickEvent event) {
				filter = filterBuilder.getCriteria();
				jobsGrid.filterData(filter);
			}
		});
		IButton clear = new IButton("Clear");
		clear.setHeight(20);
		clear.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			public void onClick(ClickEvent event) {
				filterBuilder.clearCriteria();
				filter = filterBuilder.getCriteria();
				jobsGrid.filterData(filter);
			}
		});

		HLayout buttons = new HLayout();
		buttons.setWidth100();
		buttons.setAlign(Alignment.RIGHT);
		buttons.setHeight(20);
		buttons.setMargin(10);
		buttons.setMembersMargin(5);
		buttons.setMembers(clear, ok);

		layout.addMember(filterBuilder);
		layout.addMember(buttons);

		return layout;
	}

	/**
	 * Builds and returns the jobs list grid
	 *
	 * <pre>
	 * +- layout:HLayout -------+
	 * |+ jobsGrid:ListGrid ---+|
	 * ||    jobs details      ||
	 * |+----------------------+|
	 * +------------------------+
	 * </pre>
	 */
	public Layout build() {
		this.jobsLoading = new Label("Fetching jobs...");
		this.jobsLoading.setWidth100();
		this.jobsLoading.setAlign(Alignment.CENTER);
		this.jobsLoading.setIcon("loading.gif");
		this.jobsLoading.hide();

		this.jobsGrid = new ListGrid() {
			@Override
			protected String getCellCSSText(ListGridRecord record, int rowNum, int colNum) {
				String base = super.getCellCSSText(record, rowNum, colNum);
				String fieldName = this.getFieldName(colNum);

				/* change the color of the job status field	 */
				if (fieldName.equals(STATE_ATTR)) {
					try {
						switch (JobStatus.valueOf(record.getAttribute(STATE_ATTR).toUpperCase())) {
							case KILLED:
								return "color:#d37a11;font-weight:bold;" + base;
							case CANCELED:
							case FAILED:
								return "color:#c50000;font-weight:bold;" + base;
							case RUNNING:
								return "color:#176925;font-weight:bold;" + base;
							case PENDING:
								return "color:#1a8bba;" + base;
							case STALLED:
							case PAUSED:
								return "font-weight:bold;" + base;
							case FINISHED:
								return base;
						}
					} catch (NullPointerException npe) {
						return base;
					}
				}
				return base;
			}
		};
		this.ds = new JobDS("jobds_" + controller.getModel().getSessionId());
		this.jobsGrid.setDataSource(this.ds);
		this.jobsGrid.setCanGroupBy(false);
		this.jobsGrid.setCanReorderFields(false);
		this.jobsGrid.setCanPickFields(false);
		this.jobsGrid.setCanFreezeFields(false);
		this.jobsGrid.setSelectionProperty("isSelected");
		// this.jobsGrid.setSelectionType(SelectionStyle.SINGLE);
		this.jobsGrid.setAutoFetchData(true);
		this.jobsGrid.setSortField(0);
		this.jobsGrid.setSortDirection(SortDirection.DESCENDING);

		ListGridField idField = new ListGridField(ID_ATTR, "Id");
		idField.setType(ListGridFieldType.INTEGER);
		idField.setAlign(Alignment.LEFT);
		idField.setCellAlign(Alignment.LEFT);
		idField.setWidth(80);

		ListGridField stateField = new ListGridField(STATE_ATTR, "State");
		stateField.setWidth(100);

		ListGridField userField = new ListGridField(USER_ATTR, "User");
		userField.setWidth(140);

		ListGridField progressField = new ListGridField(PROGRESS_ATTR, "Progress");
		progressField.setType(ListGridFieldType.FLOAT);
		progressField.setAlign(Alignment.CENTER);
		progressField.setWidth(150);
		progressField.setCellFormatter(new CellFormatter() {
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				int pw = jobsGrid.getFieldWidth(PROGRESS_ATTR);
				float progress = Float.parseFloat(value.toString());
				int bx = new Double(Math.ceil(pw * progress)).intValue() - 601;
				String progressUrl = SchedulerImages.instance.progressbar().getSafeUri().asString();

				String style = "display:block; " + //
					"border: 1px solid #acbac7; " + //
					"background-image:url(" + progressUrl + ");" + //
					"background-position:" + bx + "px 0px;" + //
					"background-repeat: no-repeat;" + //
					"background-color:#a7cef6";

				String prg = ((JobRecord) record).job.getFinishedTasks() + " / " +
					((JobRecord) record).job.getTotalTasks();
				String str = "<div style='" + style + "'>" + prg + "</div>";

				return str;
			}
		});

		ListGridField priorityField = new ListGridField(PRIORITY_ATTR, "Priority");
		priorityField.setWidth(100);

		ListGridField duration = new ListGridField(DURATION_ATTR, "Duration");
		duration.setWidth(100);
		duration.setCellFormatter(new CellFormatter() {
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				long l = Long.parseLong(value.toString());
				return Job.formatDuration(l);
			}
		});

		ListGridField nameField = new ListGridField(NAME_ATTR, "Name");

		this.jobsGrid.setFields(idField, stateField, userField, progressField, priorityField, duration,
				nameField);

		this.jobsGrid.setWidth100();
		this.jobsGrid.setHeight100();

		HLayout layout = new HLayout();
		layout.addMember(jobsLoading);
		layout.addMember(jobsGrid);

		// right click on an entry : popup a menu for job-contextual operations
		this.jobsGrid.addCellContextClickHandler(new CellContextClickHandler() {
			public void onCellContextClick(CellContextClickEvent event) {

				boolean selPause = true; // ALL selected jobs are paused
				boolean selRunning = true; // ALL selected jobs are running/stalled/pending
				boolean selFinished = true; // ALL selected jobs are finished
				boolean selPauseOrRunning = true; // ALL selected jobs are running/pending/paused/stalled

				final ArrayList<String> ids = new ArrayList<String>(jobsGrid.getSelectedRecords().length);
				for (ListGridRecord rec : jobsGrid.getSelectedRecords()) {
					JobRecord j = (JobRecord) rec;
					switch (JobStatus.valueOf(j.getAttribute(STATE_ATTR).toUpperCase())) {
						case PENDING:
						case RUNNING:
						case STALLED:
							selPause = false;
							selFinished = false;
							break;
						case PAUSED:
							selFinished = false;
							selRunning = false;
							break;
						default:
							selPauseOrRunning = false;
							selRunning = false;
							selPause = false;
					}

					ids.add(j.getAttribute(ID_ATTR));
				}

				Menu menu = new Menu();
				menu.setShowShadow(true);
				menu.setShadowDepth(10);

				MenuItem pauseItem = new MenuItem("Pause", SchedulerImages.instance.scheduler_pause_16()
						.getSafeUri().asString());
				pauseItem.addClickHandler(new ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						JobsView.this.controller.pauseJobs(ids);
					}
				});
				pauseItem.setEnabled(selRunning);

				MenuItem resumeItem = new MenuItem("Resume", SchedulerImages.instance.scheduler_resume_16()
						.getSafeUri().asString());
				resumeItem.addClickHandler(new ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						JobsView.this.controller.resumeJobs(ids);
					}
				});
				resumeItem.setEnabled(selPause);

				MenuItem priorityItem = new MenuItem("Priority");
				Menu priorityMenu = new Menu();
				for (final JobPriority p : JobPriority.values()) {
					MenuItem item = new MenuItem(p.toString());
					if (!selPauseOrRunning) {
						item.setEnabled(false);
					} else {
						item.addClickHandler(new ClickHandler() {
							public void onClick(MenuItemClickEvent event) {
								JobsView.this.controller.setJobPriority(ids, p);
							}
						});
					}
					priorityMenu.addItem(item);
				}
				priorityItem.setSubmenu(priorityMenu);

				MenuItem removeItem = new MenuItem("Remove", SchedulerImages.instance.job_kill_16()
						.getSafeUri().asString());
				removeItem.addClickHandler(new ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						JobsView.this.controller.removeJob(ids);
					}
				});

				MenuItem killItem = new MenuItem("Kill", SchedulerImages.instance.scheduler_kill_16()
						.getSafeUri().asString());
				killItem.addClickHandler(new ClickHandler() {
					public void onClick(MenuItemClickEvent event) {
						JobsView.this.controller.killJob(ids);
					}
				});

				killItem.setEnabled(selPauseOrRunning);
				removeItem.setEnabled(selFinished);

				menu.setItems(pauseItem, resumeItem, priorityItem, removeItem, killItem);

				jobsGrid.setContextMenu(menu);
			}
		});

		this.jobsGrid.addCellOutHandler(new CellOutHandler() {
			public void onCellOut(CellOutEvent event) {
				// cancel contextual menu when moving the mouse away to
				// avoid keeping a menu on items we lost track of.
				// Not perfect but works in most cases
				jobsGrid.setContextMenu(null);
			}
		});

		jobsGrid.addSelectionChangedHandler(new SelectionChangedHandler() {
			public void onSelectionChanged(SelectionEvent event) {
				if (event.getState()) {
					JobRecord record = (JobRecord) event.getRecord();
					selJobId = record.getAttributeAsInt(ID_ATTR);
					JobsView.this.controller.selectJob("" + selJobId);
				}
			}
		});

		return layout;
	}
}
