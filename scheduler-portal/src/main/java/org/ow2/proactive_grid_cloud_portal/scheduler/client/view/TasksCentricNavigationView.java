package org.ow2.proactive_grid_cloud_portal.scheduler.client.view;

import java.util.Date;

import org.ow2.proactive_grid_cloud_portal.scheduler.client.controller.TasksCentricNavigationController;

import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RelativeDateItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.Layout;

public class TasksCentricNavigationView extends TasksNavigationView{

    public TasksCentricNavigationView(TasksCentricNavigationController controller) {
        super(controller);
    }

    
    @Override
    public Layout build() {
        Layout layout = super.build();
        
        RelativeDateItem fromDateItem = new RelativeDateItem("fromDate", "from");
        fromDateItem.addChangedHandler(new ChangedHandler() {   
            @Override
            public void onChanged(ChangedEvent event) {
                fromDateChangedHandler(event);
            }
        });
        RelativeDateItem toDateItem = new RelativeDateItem("toDate", "to");
        toDateItem.addChangedHandler(new ChangedHandler() {
            @Override
            public void onChanged(ChangedEvent event) {
                toDateChangedHandler(event);
            }
        });
        
        DynamicForm form = new DynamicForm();
        form.setLeft(5);
        form.setNumCols(4);
        form.setItems(fromDateItem, toDateItem);
        layout.addMember(form);
        
        return layout;
    }
    
    
    protected void fromDateChangedHandler(ChangedEvent event){
        Date value = (Date) event.getValue();
        long time = value.getTime();
        ((TasksCentricNavigationController) this.controller).changeFromDate(time);
    }
    
    protected void toDateChangedHandler(ChangedEvent event){
        Date value = (Date) event.getValue();
        long time = value.getTime();
        ((TasksCentricNavigationController) this.controller).changeToDate(time);
    }
    
}
