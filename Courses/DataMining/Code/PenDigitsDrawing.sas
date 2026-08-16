proc print data=datasets.pendigittest(obs=10);

run;

data test8;
input x y;
datalines;
88 92 
2 99 
16 66 
94 37 
70 0 
0 24 
42 65 
100 100
; 
run;
data test1;
input x y;
datalines;
70 100 
100 97 
70 81 
45 65 
30 49 
20 33 
0 16 
0 0 
;
data test4;
input x y;
datalines;
40 100 
0 81 
15 58 
100 57 
47 87 
50 88 
40 42 
36 0 
;
proc sgplot data=test8;
title 'The number 8';
series y=y x=x;
run;
proc sgplot data=test1;
title 'The number 1';
series y=y x=x;
run;
proc sgplot data=test4;
title 'The number 4';
series y=y x=x;
run;
