# This function scalculate the variances/covariance matrix or the correlation coefficient 
# for the joint probability of two species occurring at N locations. 
# For example, colonization by mycorrhizal fungi and pathogens at N = 100 root cross-sections
# x is the dataset with rows representing each cross-section and the following columns:
# ind -- the column with the labels for individuals; there should be > 1 location per individual
# s1 -- the column of presence/absence (1:0) for the first species
# s2 -- the column of presence/absence (1:0) for the second species
# coef -- should be cor or cov, indicating which calculation to use. 
# Cor returns a single value (correlation) 
# Cov returns a vector of 3 values representing the variance of Sp1, variance of Sp2, and their covariance
Bcor<-function(x=NA,ind=ind,s1=s1,s2=s2,coef=NA){
  # Check if user wants covariance matrix or correlation coefficient
  
  # Calculate the frequency of each possible outcome
  A<-
  B<-
  C<-
  D<-
  
  # Calculate variances, covariance and correlation
  V1<- 
  V2<-
  COV12<-
  COR12<-
    
  if (coef == cov){
    return(c(V1,V2,Cov))
  }

  if (coef == cor){
    
    return(COR)
  }
}

# Example:
rootdata<-data.frame(ID=c(rep("A1",100),rep("B1",100)),
                     myc=sample(0:1,200,prob=c(0.8,0.2),replace=T),
                     path=sample(0:1,200,prob=c(0.8,0.2),replace=T))
head(rootdata)

Bcor(rootdata,ind=ID,s1=myc,s2=path,coef=cov)

Bcor(rootdata,ind=ID,s1=myc,s2=path,coef=cor)

for(i in levels(rootdata$ID)){
  print(paste(i,round(cor(rootdata$myc[rootdata$ID==i],rootdata$path[rootdata$ID==i]),3)))
}

