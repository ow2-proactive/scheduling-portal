package org.ow2.proactive_grid_cloud_portal.scheduler.client.view.grid;

import java.util.List;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.Task;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksController;
import org.ow2.proactive_grid_cloud_portal.scheduler.client.view.TasksView;

import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.viewer.DetailViewer;
import com.smartgwt.client.widgets.viewer.DetailViewerField;
import com.smartgwt.client.widgets.viewer.DetailViewerRecord;

public class ExpandableTasksListGrid extends TasksListGrid{

    private ListGridRecord expandRecord;
    
    public ExpandableTasksListGrid(TasksController controller) {
        super(controller);
        this.setCanExpandRecords(true);
    }
    
    
    protected DetailViewerField buildDetailViewer(TasksColumns column){
        return new DetailViewerField(column.getName(), column.getTitle());
    }
    
    @Override
    protected Canvas getExpansionComponent(final ListGridRecord record) {
        if (expandRecord != null && expandRecord != record) {
            this.collapseRecord(expandRecord);
        }
        this.expandRecord = record;

        TaskRecord rec = (TaskRecord) record;
        Task t = rec.getTask();

        DetailViewer detail = new DetailViewer();
        detail.setWidth100();
        detail.setHeight100();
        detail.setCanSelectText(true);

        DetailViewerField [] fields = new DetailViewerField[4];
        fields[0] = buildDetailViewer(TasksColumns.HOST_ATTR);
        fields[1] = buildDetailViewer(TasksColumns.START_TIME_ATTR);
        fields[2] = buildDetailViewer(TasksColumns.FINISHED_TIME_ATTR);
        fields[3] = buildDetailViewer(TasksColumns.DESCRIPTION_ATTR);

        detail.setFields(fields);

        DetailViewerRecord r1 = new DetailViewerRecord();
        r1.setAttribute(TasksColumns.HOST_ATTR.getName(), (t.getHostName().equals("null") ? "" : t.getHostName()));
        r1.setAttribute(TasksColumns.DESCRIPTION_ATTR.getName(), t.getDescription());
        r1.setAttribute(TasksColumns.START_TIME_ATTR.getName(), rec.getAttribute(TasksColumns.START_TIME_ATTR.getName()));
        r1.setAttribute(TasksColumns.FINISHED_TIME_ATTR.getName(), rec.getAttribute(TasksColumns.FINISHED_TIME_ATTR.getName()));

        detail.setData(new DetailViewerRecord[]{r1});

        VLayout layout = new VLayout();
        layout.addMember(detail);

        return layout;
    }


    @Override
    public void tasksUpdating(boolean jobChanged) {
        if(jobChanged){
            this.expandRecord = null;
        }
    }

    
    @Override
    protected TaskRecord updateTaskRecord(Task task) {
        TaskRecord record = super.updateTaskRecord(task);
        String idAttr = TasksColumns.ID_ATTR.getName();
        if (this.expandRecord != null &&
                record.getAttribute(idAttr).equals(this.expandRecord.getAttribute(idAttr))) {
            this.expandRecord = record;
        }
        return record;
    }

}
