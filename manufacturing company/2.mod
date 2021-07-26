/*********************************************
 * OPL 12.6.0.0 Model
 * Author: parisa
 * Creation Date: Dec 1, 2018 at 11:58:24 AM
 *********************************************/
//we have 3 products
range j = 1..3;

//maximum demand of production[j]
int max_d[j] = ...;
//minimum demand of production[j]
int min_d[j] = ...;
//maximum of production[j] each day
int max_p[j] = ...;
//cost of producing each production[j]
float pro_cost[j] = ...;
//price of production[j]
float price[j] = ...;
//cost of starting production[j]
float s_cost[j] = ...;

//production[j] in this month
dvar int+ p[j];
//if we produce production[j] in this month or not
dvar boolean x[j];

//goal
maximize sum(j in j) (price[j] - pro_cost[j])*p[j] - sum(j in j) s_cost[j] * x[j];

subject to {
           forall(j in j) p[j] >= min_d[j];
           
           forall(j in j) p[j] <= max_d[j] * x[j];
           
           sum(j in j) ( p[j]/ max_p[j]) <= 22;
}
 
