library(readr)
May_2017_FloristicSurvey_summer2016 <- read_delim("~/Documents/Queens/Thesis/Data/May_2017_FloristicSurvey_summer2016.csv", 
                                                  +     "\t", escape_double = FALSE, trim_ws = TRUE)
FloristicSurvey<-May_2017_FloristicSurvey_summer2016

PlantLength <- read_delim("~/Documents/Queens/Thesis/Data/May_2017_PlantLength_summer2016.csv", 
                          +     "\t", escape_double = FALSE, trim_ws = TRUE)

####Restructuring####
library("dplyr")
head(PlantLength)
str(PlantLength)
colnames(PlantLength)[1] <- "Population"
PlantLength$Population<-as.factor(PlantLength$Population)
PlantLength$Location<-as.factor(PlantLength$Location)

# Simplified, clean plotting theme
theme_simple <- function (base_size = 12, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(size=18),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=18,angle=90),
      axis.text.y = element_text(size=12),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill="white"),
      panel.border = element_blank(),
      plot.title=element_text(face="bold", size=24),
#      legend.position="none"
    )
}

####Plant length and GM presence####
library("lme4")
mod1<-PlantLength$Length_plant~PlantLength$Location+(1|PlantLength$Species)+(1|PlantLength$Population)
lmer(mod1) # Just made a model, it didnt work. Why? NAs!
row.has.na <- apply(PlantLength, 1, function(x){any(is.na(x))})
sum(row.has.na) 
PlantLength.filtered <- PlantLength[!row.has.na,]# Removed all the rows with NAs

mod1<-lmer(log(PlantLength.filtered$Length_plant)~PlantLength.filtered$Location+(1|PlantLength.filtered$Species)+(1|PlantLength.filtered$Population))
mod2<-lmer(log(PlantLength.filtered$Length_plant)~PlantLength.filtered$Location+(1|PlantLength.filtered$Species))
mod3<-lmer(log(PlantLength.filtered$Length_plant)~1+(1|PlantLength.filtered$Species)+(1|PlantLength.filtered$Population))
# Made these three models, one is all of them, one no population, and one no in/ out
summary(mod1)
anova(mod1,mod2) # One ANOVA with model 1, one with model 2 to compare if they are statistically significantly different from each other
qplot(predict(mod1),log(PlantLength.filtered$Length_plant)) #is this model normal? NO
qplot(predict(mod1),log(PlantLength.filtered$Length_plant)-predict(mod1)) # Make this model normal by log() the y variable.
cor(predict(mod1),PlantLength.filtered$Length_plant)^2 # Are the correlation coefficients meaningful?
cor(predict(mod2),PlantLength.filtered$Length_plant)^2 # Are the correlation coefficients meaningful wihout pops?

anova(mod1,mod3) # Repeat with a model without the in/ out component.
cor(predict(mod3),PlantLength.filtered$Length_plant)^2 # how much different does it make when we consider GM?


####Floristic composition and GM presence####
library("ggplot2")
library("ggfortify")
str(FloristicSurvey)
colnames(FloristicSurvey)[1] <- "Population" # population loads in weirdly, I am just arranging it to not have quotes around. so odd
FloristicSurvey$Population<-as.factor(FloristicSurvey$Population) # Make the population a factor and not an interger
sum(row.has.na <- apply(FloristicSurvey, 1, function(x){any(is.na(x))})) #Do we have NAs? in this case the NAs are supose to be 0, its just a typo
FloristicSurvey[is.na(FloristicSurvey)] <- 0
sum(row.has.na <- apply(FloristicSurvey, 1, function(x){any(is.na(x))}))

# Start the PCA
PC <- prcomp(FloristicSurvey[,4:ncol(FloristicSurvey)], center=T)
summary(PC)
screeplot(PC)

# Variation explaination
100*sum(summary(PC)[[1]][1])/sum(summary(PC)[[1]]) #explained by just PC1
100*sum(summary(PC)[[1]][1:2])/sum(summary(PC)[[1]])#explained by PC1 and PC2
n=25
100*sum(summary(PC)[[1]][1:n])/sum(summary(PC)[[1]])

#### Linear discriminate analysis####

# Discriminant function analysis is used (general LDA format) because i have a cat. dependent and a binary independent. (it needs binary or continuous!)
# you might have to bin covers because it only works with categorical, so 0-5% none, 6-40 low etc.
FloristicSurvey$GM_Coverage_category<-cut(FloristicSurvey$GM_Coverage, c(-Inf,5,30,50,70,95,100), labels = c("None", "Low", "Med_Low", "Med_High", "High", "Overtaken"))
table(FloristicSurvey$GM_Coverage_category)
# [0-5] None
# ]5-30] Low
# ]30-50] Medium low
# ]50-70] Medium high
# ]70-95] High
# ]95-100] Overtaken

#LDA
library(MASS)
FloristicSurveysmall<-data.frame(FloristicSurvey[,4:56])
# Linear discriminant function analysis
(FloristicSurveyLDA<-lda(GM_Coverage_category ~ ., data=FloristicSurveysmall))
FloristicSurveysmall[45] <- NULL 
FloristicSurveyLDA<-lda(GM_Coverage_category ~ ., data=FloristicSurveysmall)
# Extract scaling vectors
scalvec<-data.frame(FloristicSurveyLDA$scaling)
# Extract predictions
FloristicSurveyLDAval <- data.frame(predict(FloristicSurveyLDA)$x)
par(mar = rep(2, 4))
ldahist(data = FloristicSurveyLDAval[,1], g=FloristicSurveysmall$GM_Coverage_category)

# Plot results
p<-ggplot(data=FloristicSurveyLDAval,aes(x=LD1,y=LD2,group=FloristicSurveysmall$GM_Coverage_category))+
  stat_ellipse(geom="polygon",aes(colour=FloristicSurveysmall$GM_Coverage_category),fill=NA,size=1.2,alpha=0.3)+
  stat_ellipse(geom="polygon",aes(fill=FloristicSurveysmall$GM_Coverage_category,colour=FloristicSurveysmall$GM_Coverage_category),size=1.2,alpha=0.3)+
  geom_point(aes(shape=FloristicSurveysmall$GM_Coverage_category,fill=FloristicSurveysmall$GM_Coverage_category,colour=FloristicSurveysmall$GM_Coverage_category),size=I(4),alpha=I(0.8))+
  xlab("LD Axis 1")+ylab("LD Axis 2")+theme_simple() +
  theme(legend.title=element_blank())
print(p)
dev.off()

#test significance of axis
anova(lm(FloristicSurveyLDAval$LD1~FloristicSurveysmall$GM_Coverage_category))
anova(lm(FloristicSurveyLDAval$LD2~FloristicSurveysmall$GM_Coverage_category))

#LDA WITHOUT A. PETIOLATA
library(MASS)
FloristicSurveysmall<-data.frame(FloristicSurvey[,4:56])
# Linear discriminant function analysis
FloristicSurveysmall[10]<-NULL
(FloristicSurveyLDA<-lda(GM_Coverage_category ~ ., data=FloristicSurveysmall))
FloristicSurveysmall[44] <- NULL 
FloristicSurveyLDA<-lda(GM_Coverage_category ~ ., data=FloristicSurveysmall)
# Extract scaling vectors
scalvec<-data.frame(FloristicSurveyLDA$scaling)
# Extract predictions
FloristicSurveyLDAval <- data.frame(predict(FloristicSurveyLDA)$x)
par(mar = rep(2, 4))
ldahist(data = FloristicSurveyLDAval[,1], g=FloristicSurveysmall$GM_Coverage_category)

# Plot results
p<-ggplot(data=FloristicSurveyLDAval,aes(x=LD1,y=LD2,group=FloristicSurveysmall$GM_Coverage_category))+
  stat_ellipse(geom="polygon",aes(colour=FloristicSurveysmall$GM_Coverage_category),fill=NA,size=1.2,alpha=0.3)+
  stat_ellipse(geom="polygon",aes(fill=FloristicSurveysmall$GM_Coverage_category,colour=FloristicSurveysmall$GM_Coverage_category),size=1.2,alpha=0.3)+
  geom_point(aes(shape=FloristicSurveysmall$GM_Coverage_category,fill=FloristicSurveysmall$GM_Coverage_category,colour=FloristicSurveysmall$GM_Coverage_category),size=I(4),alpha=I(0.8))+
  xlab("LD Axis 1")+ylab("LD Axis 2")+theme_simple() +
  theme(legend.title=element_blank())
print(p)
dev.off()

#test significance of axis
anova(lm(FloristicSurveyLDAval$LD1~FloristicSurveysmall$GM_Coverage_category))
anova(lm(FloristicSurveyLDAval$LD2~FloristicSurveysmall$GM_Coverage_category))

####cluster analysis####

# Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)

#do they cluster by population?
FloristicSurvey[is.na(FloristicSurvey)] <- 0
mydata<-t(data.frame(FloristicSurvey[,1],FloristicSurvey[,4:55]))
my.names <- mydata[1,]
colnames(mydata) <- my.names
mydata <- mydata[-1,]
d <- dist(t(mydata), method = "euclidean") # distance matrix
fit <- hclust(d, method = "ward.D")
plot(fit) 

#do they cluster by GM coverage?
FloristicSurvey[is.na(FloristicSurvey)] <- 0
#this will just make the dendogram easier to see
FloristicSurvey$GM_Coverage_category<-cut(FloristicSurvey$GM_Coverage, c(-Inf,5,30,50,70,95,100), labels = c("N", "L", "ML", "MH", "H", "F"))
mydata<-t(data.frame(FloristicSurvey[,56],FloristicSurvey[,4:55]))
my.names <- mydata[1,]
colnames(mydata) <- my.names
mydata <- mydata[-1,]
d <- dist(t(mydata), method = "euclidean") # distance matrix
fit <- hclust(d, method = "ward.D")
plot(fit) 
FloristicSurvey$GM_Coverage_category<-cut(FloristicSurvey$GM_Coverage, c(-Inf,5,30,50,70,95,100), labels = c("None", "Low", "Med_Low", "Med_High", "High", "Overtaken"))


####Bootleg way to PCA####
M <- as.matrix(FloristicSurvey[1:198,4:52])
M[is.na(M)] <- 0
f$groups <- c(rep(1,72),rep(72,94))
myPCA <- prcomp(M, scale. = F, center = F)
plot(myPCA$x)
PCs<-cbind(FloristicSurvey[,1:3],myPCA$x)

#### NMDS ####
# https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/
library(vegan)
#Make a matrix with no row or column equal to 0 (do not enclude the env variable (GM COVERAGE))
M <- as.matrix(FloristicSurvey[1:198,4:55])
M[is.na(M)] <- 0
rownames(M) <- FloristicSurvey$Population
which( colSums(M)==0 )
which( rowSums(M)==0 )
#Now that you know which column must be taken out, redo the matrix but with the last column, then remove the rows and columns =0
#I did this because the previous method doesnt work with the characters in the GM_COVERAGE column.
M <- as.matrix(FloristicSurvey[1:198,4:56])
M[is.na(M)] <- 0
rownames(M) <- FloristicSurvey$Population
M<-M[,-45]
M<-M[-130,]
M<-M[-133,]
M<-M[-132,]
# Now I'll make the env column a vector on its own, but the previous colde now allows it to be in the same order as the matrix im using for distances.
GM_coverage_df <- data.frame(M[,52])
M<-M[,-52]
class(M) <- "numeric"
# with vegdist from Bray to seroeson: add binary = T 
dist_FloristicSurvey <- vegdist(M, method = "bray", binary = T)
#The metaMDS analysis could have done the distance matrix internally but i would rather control it since i have presence/abscence
meta.nmds.FloristicSurvey <- metaMDS(dist_FloristicSurvey)
# Non convergence with only 20 tries. lets increase that then
meta.nmds.FloristicSurvey2D <- metaMDS(dist_FloristicSurvey, k=2, trymax = 1000)
str(meta.nmds.FloristicSurvey2D)
stressplot(meta.nmds.FloristicSurvey2D)
# Im getting a little above a stress of 0.2 (0.207), which isnt ideal, ill try it again with 3 dimension
# A good rule of thumb: stress < 0.05 provides an excellent representation in reduced dimensions, < 0.1 is great, < 0.2 is good/ok, and stress < 0.3 provides a poor representation.
meta.nmds.FloristicSurvey3D <- metaMDS(dist_FloristicSurvey, k=3, trymax = 1000)
str(meta.nmds.FloristicSurvey3D)
# stress is lower! 0.146
stressplot(meta.nmds.FloristicSurvey3D)

# envfit for the 2D
FloristicSurvey_envfit <- envfit(meta.nmds.FloristicSurvey2D, env = GM_coverage_df, perm = 999) #standard envfit
FloristicSurvey_envfit

#data for plotting 
##NMDS points
FloristicSurvey.NMDS.data<-GM_coverage_df 
FloristicSurvey.NMDS.data$NMDS1<-meta.nmds.FloristicSurvey2D$points[ ,1] 
FloristicSurvey.NMDS.data$NMDS2<-meta.nmds.FloristicSurvey2D$points[ ,2] 
colnames(FloristicSurvey.NMDS.data)[1] <- "GM_Coverage"

# data for the envfit arrows
env.scores.FloristicSurvey <- as.data.frame(scores(FloristicSurvey_envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.FloristicSurvey <- cbind(env.scores.FloristicSurvey, env.variables = rownames(env.scores.FloristicSurvey)) #and then gives them their names

# function for ellipsess - just run this, is used later
#taken from the excellent stackoverflow Q+A: http://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#data for ellipse, use GM coverage
df_ell.FSurvey.GM_coverage <- data.frame() #sets up a data frame before running the function.
for(g in levels(FloristicSurvey.NMDS.data$GM_Coverage)){
  df_ell.FSurvey.GM_coverage <- rbind(df_ell.FSurvey.GM_coverage, cbind(as.data.frame(with(FloristicSurvey.NMDS.data [FloristicSurvey.NMDS.data$GM_Coverage==g,],
                                                                                   veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                                                ,GM_coverage=g))
}

# data for labelling the ellipse
NMDS.mean.FloristicSurvey=aggregate(FloristicSurvey.NMDS.data[ ,c("NMDS1", "NMDS2")], 
                         list(group = FloristicSurvey.NMDS.data$GM_Coverage), mean)

## finally plotting.
mult <- 1 #multiplier for the arrows and text for envfit below. You can change this and then rerun the plot command.
library(ggplot2)
(FloristicSurvey.nmds.gg1 <- ggplot(data = FloristicSurvey.NMDS.data, aes(y = NMDS2, x = NMDS1))+ #sets up the plot. brackets around the entire thing to make it draw automatically
    geom_path(data = df_ell.FSurvey.GM_coverage, aes(x = NMDS1, y = NMDS2, group = df_ell.FSurvey.GM_coverage$GM_coverage, alpha=df_ell.FSurvey.GM_coverage$GM_coverage))+ #this is the ellipse, seperate ones by Site. If you didn't change the "alpha" (the shade) then you need to keep the "group 
    scale_alpha_manual(guide = FALSE,values=c(0.3, 0.5, 0.6, 0.7, 0.8, 0.9))+ #sets the shade for the ellipse
    geom_point( aes(shape = FloristicSurvey.NMDS.data$GM_Coverage), size = 1) + #puts the site points in from the ordination, shape determined by site, size refers to size of point
    # geom_text(data=spps2, aes(x=spps2$NMDS1, y=spps2$NMDS2, label=species), size = 3.3, hjust=1.1)+ #labelling the species. hjust used to shift them slightly from their points
    #annotate("text",x = NMDS.mean$NMDS1,y = NMDS.mean$NMDS2,label=NMDS.mean$group) + #labels for the centroids - I haven't used this since we have a legend. but you could also dithc the legend, but plot will get v messy
    #geom_segment(data = env.scores.FloristicSurvey,
    #             aes(x = 0, xend = mult*env.scores.FloristicSurvey$NMDS1, y = 0, yend = mult*env.scores.FloristicSurvey$NMDS2),
    #             arrow = arrow(length = unit(0.25, "cm")), colour = "grey") + #arrows for envfit.  doubled the length for similarity to the plot() function. NB check ?envfit regarding arrow length if not familiar with lengths
    #geom_text(data = env.scores.FloristicSurvey, #labels the environmental variable arrows * "mult" as for the arrows
    #          aes(x = mult*env.scores.FloristicSurvey$NMDS1, y = mult*env.scores.FloristicSurvey$NMDS2, label=env.variables),
    #          size = 5,
    #          hjust = -0.5)+
    scale_shape_manual(values = c(1,8,19,5,6,7))+ #sets the shape of the plot points instead of using whatever ggplot2 automatically provides
    coord_cartesian(xlim = c(-1,1.5))+  ## NB this changes the visible area of the plot only (this is a good thing, apparently). Can also specify ylim. Here in case you want to set xaxis manually.
    theme_simple())














# Software version info
sessionInfo()
