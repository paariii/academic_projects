sets
i month /1*6/;

parameters
d(i) demand of each month /1 700, 2 600, 3 500, 4 800, 5 900, 6 800/;

Scalar ps price of storing products for each month /10/;
Scalar pf price of firing each worker /700/;
Scalar ph price of hiring each worker /500/;
Scalar pe price of each extra product /5/;
Scalar p_w production for each worker in normal condition /20/;
Scalar max_e maximum of extra production for each worker in a month /6/;
Scalar max_f maximum of workers who get fired each month /5/;
Scalar max_h maximum of workers who got hired each month /5/;
Scalar i_workers initial workers at first month /40/;

variable
z total cost of production
c_h_f(i) total cost of hiring and firing in month(i);

integer variables
l(i) workers of each month
hl(i) hired workers of each month
fl(i) fired workers of each month
npro(i) production in normal condition in a month
epro(i) extra production in a month
s(i) stuck of month;

l.up (i) = 10000;
hl.up (i) = 10000;
fl.up (i) = 10000;
npro.up (i) = 10000;
epro.up (i) = 10000;
s.up (i) = 10000;
c_h_f.up (i) = 100000000;

Binary variables
horf(i) hire workers or fire them in a month;

Equations
goal
limit1
limit2
limit3
limit4
fired(i) to be sure of fired workers satisfaction
hired(i) to be sure of hired workers satisfaction
store(i) amount of stored product
labor(i) workers of each month
normalProduction(i) to be sure of normal production satisfaction
extraProduction(i) to be sure of extra production satisfaction
relation(i) we have to produce d(i) at least
cost_hf(i) calculating total cost of hiring and firing each month;

goal.. z =e= pe * Sum (i , epro(i)) + ps * Sum (i, s(i)) + ph * Sum (i, hl(i)) + pf * Sum (i , fl(i));
limit1.. s('6') =e= 0;
limit2.. s('1') =e= npro('1') + epro('1') - d('1');
limit3.. l('1') =e= i_workers + hl('1') - fl('1');
limit4.. npro('1') + epro('1') =g= d('1');
fired(i).. fl(i) =l= max_f * horf(i);
hired(i).. hl(i)=l= max_h * (1 - horf(i));
store(i)$(ord(i) ne 1 and ord(i) ne 6).. s(i) =e= s(i - 1) + npro(i) + epro(i) - d(i);
labor(i)$(ord(i) ne 1).. l(i) =e= l(i - 1) + hl(i) - fl(i);
normalProduction(i).. npro(i) =e= p_w * l(i);
extraProduction(i).. epro(i) =l= max_e * l(i);
relation(i)$(ord(i) ne 1).. npro(i) + epro(i) + s(i - 1) =g= d(i);
cost_hf(i).. c_h_f(i) =e= ph * hl(i) + pf * fl(i);

model problem1 comments/All/;

option optcr = 0;
option optca = 0;

solve problem1 using mip minimizing z;

display npro.l;
display epro.l;
display s.l;
display hl.l;
display fl.l;
display c_h_f.l;
display z.l;

execute_unload "results.gdx", npro.l, epro.l, s.l, hl.l, fl.l, c_h_f.l, z.l;
execute 'gdx2xls results.gdx results1.xlsx';
