

/* Principal Components Regression*/

proc reg data=linalg.French;
	model Import = DoProd Stock Consum / vif;
run;
quit;

proc corr data=linalg.french;
var _all_;
run;

proc princomp data=linalg.French out=frenchPC;
	var DoProd Stock Consum;
run;


/* Principal Components Regression */

proc standard data=frenchPC mean=0 std=1 out=frenchPC2;
	var Import;
run;

proc reg data=frenchPC2;
	Reg: model Import = DoProd Stock Consum /vif;
	PCa: model Import = Prin1 Prin2 Prin3 /vif; /*BAD. you haven't really changed anything! Multicollinearity is hidden!*/
	PCb: model Import = Prin1 Prin2 /vif;
run;
quit;


/* PCR Replacement */

proc pls data=linalg.French method=pcr nfac=2;
	model Import = DoProd Stock Consum / solution;
run;
quit;

