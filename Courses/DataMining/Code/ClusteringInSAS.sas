cas;
caslib _all_ assign;

/***************************************************************/
/***************************************************************/
/********************** EXPLORE YOUR DATA **********************/
/***************************************************************/
/***************************************************************/

proc princomp 	data = public.breast_cancer 
				out=bcpc;
	var bare chromatin ct epithelial margin mitoses normal shape size ;
run;
proc sgplot data=bcpc;
	scatter x=prin1 y=prin2 /group = target;
run;


/***************************************************************/
/***************************************************************/
/********************** CLUSTER YOUR DATA **********************/
/******************** HIERARCHICAL CLUSTERING ******************/
/***************************************************************/
/***************************************************************/

/* You do not need to pass a distance matrix into proc cluster
		it will create one for you in the background */
proc cluster data = public.breast_cancer 
 			standard /*performs standardization*/
			method = centroid 
			plots(only maxpoints=700)=(dendrogram ccc pseudo)
			outtree=work.Cluster_tree
			k=2;
var bare chromatin ct epithelial margin mitoses normal shape size ;
id id; /*This statement is important if you want to get cluster assignments out
		for each observation (that can be done with proc tree)  */
run;


/***************************************************************/
/***************************************************************/
/*************** GET YOUR CLUSTER ASSIGNMENTS ******************/
/***************************************************************/
/***************************************************************/
/* In order to GET clusters, you have to cut the tree with a separate
procedure. The id statement in proc cluster is important - you need an 
id variable to identify the clusters, otherwise the default names created 
don't even let you sort easily */

proc tree 	data = work.Cluster_tree 
			nclusters=2 
			out=BCClusters;
run;

/* Since the output doesn't contain all the data, need to merge those cluster
assignments back with the original data */

proc sort data = BCClusters ;
	by  _NAME_;
run;

data BC_clusters;
	merge public.breast_cancer BCClusters;
run;
/***************************************************************/
/***************************************************************/
/****************** EXPLORE YOUR CLUSTERS **********************/
/***************************************************************/
/***************************************************************/

/* Finally we can see how well this data clusters into malignant and
benign tumors with this procedure.*/

proc freq data=bc_clusters;
	tables cluster*target;
run;


/***************************************************************/
/***************************************************************/
/********************** CLUSTER YOUR DATA **********************/
/************************** K-MEANS  ***************************/
/***************************************************************/
/***************************************************************/

proc standard 	data=public.breast_cancer 
				out=breast_cancer_std 
				mean=0 
				std=1;
	var bare chromatin ct epithelial margin mitoses normal shape size;
run;

proc fastclus 	data = breast_cancer_std 
				out=bc_kmeans 
				maxclusters =2
				replace=random /* randomly initialized centroids */
				random=111;    /* seed for random number generation */
	var bare chromatin ct epithelial margin mitoses normal shape size;
run;

/* Finally we can see how well this data clusters into malignant and
benign tumors with this procedure.*/

proc freq data=bc_kmeans;
	tables cluster*target;
run;









