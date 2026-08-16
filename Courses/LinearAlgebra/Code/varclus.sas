

/* VARIABLE CLUSTERING IN SAS */
/* as you increase the maxeigen option, the coarseness of the clusters should increase*/
/* in other words, the number of clusters found should decrease. */
proc varclus data=datasets.ipip maxeigen=0.7;
var e1--o10;
run;

proc varclus data=datasets.ipip maxeigen=1;
var e1--o10;
run;

proc varclus data=datasets.ipip maxeigen=2;
var e1--o10;
run;
 
   *--------------------Data on Physical Fitness------------------*
   | These measurements were made on men involved in a physical   |
   | fitness course at N.C.State Univ. The variables are Age      |
   | (years), Weight (kg), Oxygen intake rate (ml per kg body     |
   | weight per minute), time to run 1.5 miles (minutes), heart   |
   | rate while resting, heart rate while running (same time      |
   | Oxygen rate measured), maximum heart rate recorded while |
   | running, and performance on the course (ordinal).                                                     |
   | 
   *--------------------------------------------------------------*;

proc varclus data=linalg.fitness ;
	var runtime age weight oxygen_consumption run_pulse 
		rest_pulse maximum_pulse performance;
run;

/*Default sets max second eigenvalue = 1. A more conservative approach is to lower the 
max eigenvalue to 0.7. These are eigenvalues of the correlation matrix. The total amount of 
variance in the correlation matrix is simply the number of variables since the data has been
*/ 

proc varclus data=linalg.fitness maxeigen=0.7;
	var runtime age weight oxygen_consumption run_pulse 
		rest_pulse maximum_pulse performance;
run;

