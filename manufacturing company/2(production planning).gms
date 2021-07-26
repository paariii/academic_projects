sets
i day /1*22/
j production /1*3/;

parameters
max_d(j) maximum demand of production(j) /1 5300 , 2 4500 , 3 5400/
min_d(j) minimum demand of production(j) /1 20 , 2 20 , 3 16/
max_p(j) maximum of production(j) in a day /1 500, 2 450, 3 550/
pro_cost(j) cost of producing each production(j) /1 73.3 , 2 52.9 , 3 65.4/
price(j) price of production(j) /1 124 , 2 109 , 3 115/
s_cost(j) cost of statrting production(j) /1 170000 , 2 150000 , 3 100000/;

variables
z total benefit of production;

integer variables
p(j) production(j) in this month;
p.up (j) = 10000;

binary variables
x(j) if we produce production(j) in this month or not

equations
goal
min(j) minimum production(j) in this month
max(j) maximum production(j) in this month
limit_work we have only 22 working days;

goal.. z =e=  sum(j, (price(j) - pro_cost(j)) * p(j) ) - sum(j, s_cost(j) * x(j) );
min(j).. p(j) =g= min_d(j);
max(j).. p(j) =l= max_d(j) * x(j);
limit_work.. sum( j , p(j)/max_p(j) ) =l= 22;

model problem2 comments /ALL/;

Solve problem2 using mip maximizing z;

display p.l;
display z.l;
display x.l;

execute_unload "results.gdx", p.l, x.l, z.l;
execute 'gdx2xls results.gdx results2.xlsx';


