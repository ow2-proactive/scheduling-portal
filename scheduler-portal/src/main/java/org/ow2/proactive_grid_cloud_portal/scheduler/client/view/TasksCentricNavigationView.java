package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricNavigationController;

import com.smartgwt.client.data.RelativeDate;
import com.smartgwt.client.types.DateFieldLayout;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.DateRangeItem;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.toolbar.ToolStrip;

public class TasksCentricNavigationView extends TasksNavigationView{

    public TasksCentricNavigationView(TasksCentricNavigationController controller) {
        super(controller);
    }

    
    @Override
    public Layout build() {
        Layout layout = super.build();
        
        DateRangeItem dateRangeItem = new DateRangeItem();
        dateRangeItem.setShowTitle(false);
        dateRangeItem.setAllowRelativeDates(true);
        dateRangeItem.setFieldLayout(DateFieldLayout.HORIZONTAL);
        String start = RelativeDate.WEEK_AGO.getValue();
        String end = RelativeDate.NOW.getValue();
        //DateRange dateRange = new DateRange(RelativeDate.WEEK_AGO, RelativeDate.NOW);
        
        DynamicForm form = new DynamicForm();
        
        form.setItems(dateRangeItem);
        layout.addMember(form);
        
        
        
        return layout;
        
    }
}
