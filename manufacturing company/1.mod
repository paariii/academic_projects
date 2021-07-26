/*********************************************
 * OPL 12.6.0.0 Model
 * Author: parisa
 * Creation Date: Nov 30, 2018 at 11:12:38 AM
 *********************************************/
 //we have 6 months
 range i = 1..6 ;
 
//demand of each month 
 int d[i] = ...;
 //price of storing products for each month
 int ps = ...;
 //price of firing each worker
 int pf = ...;
 //price of hiring each worker
 int ph = ...;
 //price of each extra product
 int pe = ...;
 //production for each worker in normal condition
 int pw = ...;
 //maximum of extra production for each worker in a month
 int max_e = ...;
 //maximum of workers who get fired each month
 int max_f = ...; 
 //maximum of workers who get hired each month
 int max_h = ...;
 //initial workers at first month
 int i_workers = ...;
 
 //workers of each month
 dvar int+ l[i];
 //hired workers of each month
 dvar int+ hl[i];
 //fired workers of each month
 dvar int+ fl[i];
 //production in normal condition in a month
 dvar int+ npro[i];
 //extra production in a month
 dvar int+ epro[i];
 //stuck of month
 dvar int+ s[i];
 //hire workers or fire them in a month
 dvar boolean horf[i];
 
 //goal
 minimize sum(i in i) pe*epro[i] + sum(i in i) ps*s[i] + sum(i in i) ph*hl[i] + sum(i in i) pf*fl[i];
 
 subject to {
            forall(i in 2..6) l[i] == l[i-1] + hl[i] - fl[i];
            l[1] == i_workers + hl[1] - fl[1];
            
            forall(i in 2..5) s[i] == s[i-1] + npro[i] + epro[i] - d[i];
            s[1] == npro[1] + epro[1] - d[1];
            s[6] == 0;
            
            forall(i in i) fl[i] <= max_f * horf[i];
            forall(i in i) hl[i] <= max_h * (1-horf[i]);
            
            forall(i in 2..6) npro[i] + epro[i] + s[i-1] >= d[i];
            npro[1] + epro[1] >= d[1]; 
            
            forall(i in i) npro[i] == pw * l[i];
            
            forall(i in i) epro[i]  <= max_e * l[i];
}
