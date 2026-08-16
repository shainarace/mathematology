# READ IN THE DATA
tscores = read.csv('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Linear Algebra/Linear Algebra 2019/Code/testscores.csv')
# COMPUTE THE PCA
tpca = prcomp(tscores,scale=F)
# PLOT THE SCORES ALONG PC1 AND PC2. A 2-DIMENSIONAL PROJECTION OF THE DATA ONTO THE SUBSPACE SPANNED BY THE FIRST TWO PCS
plot(tpca$x[,1],tpca$x[,2])
# THE BIPLOT ADDS THE PROJECTION OF THE ORIGINAL VARIABLE AXES. 
biplot(tpca)
