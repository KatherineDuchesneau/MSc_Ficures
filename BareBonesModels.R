#### *Soil abiotic qualities and location*####
var<-Soil_characteristics[,c(1,2)]
Y<-Soil_characteristics[,-c(1,2,5)]
var$Population<-as.factor(var$Population)
Location<-as.factor(var$Location)
Y$Sample <-paste(var$Population,var$Location) 
rownames(Y)<-Y$Sample
Y$Sample<-NULL

covar_cor = cor(Y)
corrplot.mixed(covar_cor, upper = "ellipse", lower = "number")
Nutrient_PCA = dudi.pca(df = Y[, c(3, 4)], scannf = FALSE, nf = 1)
Nutrient_PCA$co
Nutrient_PCA$eig/sum(Nutrient_PCA$eig)
Soil_characteristics$Nutrient_PC1 = Nutrient_PCA$li[,1]

dat<-data.frame(c(1:20))
dat[1]<-Soil_characteristics$Location
colnames(dat)[1]<-"Location"
dat$Agg_stability.s<-scale(Soil_characteristics$Agg_stability)
dat$pH.s<-scale(Soil_characteristics$pH)
dat$Nutrient_PC1.s<-scale(Soil_characteristics$Nutrient_PC1)

GLM_soil <- glm(Location~Agg_stability.s*pH.s*Nutrient_PC1.s,
                    family=binomial(link='logit'),
                    data=dat)

 ---------------------------------------------
#LRT

GLM_soil2 <- glm(Location~Agg_stability.s+pH.s+Nutrient_PC1.s+
                   Agg_stability.s:pH.s + Agg_stability.s:Nutrient_PC1.s+
                   Nutrient_PC1.s:pH.s,
                family=binomial(link='logit'),
                data=dat)
anova(GLM_soil,GLM_soil2)

GLM_soil3 <- glm(Location~Agg_stability.s+pH.s+Nutrient_PC1.s+
                   Agg_stability.s:pH.s + Agg_stability.s:Nutrient_PC1.s,
                 family=binomial(link='logit'),
                 data=dat)
anova(GLM_soil2,GLM_soil3)


GLM_soil4 <- glm(Location~Agg_stability.s+pH.s+Nutrient_PC1.s+
                   Agg_stability.s:pH.s,
                 family=binomial(link='logit'),
                 data=dat)
anova(GLM_soil3,GLM_soil4)

GLM_soil5 <- glm(Location~Agg_stability.s+pH.s+Nutrient_PC1.s,
                 family=binomial(link='logit'),
                 data=dat)
anova(GLM_soil4,GLM_soil5)

GLM_soil6 <- glm(Location~Agg_stability.s+pH.s,
                 family=binomial(link='logit'),
                 data=dat)
anova(GLM_soil5,GLM_soil6)

GLM_soil7 <- glm(Location~Agg_stability.s,
                 family=binomial(link='logit'),
                 data=dat)
anova(GLM_soil6,GLM_soil7)

GLM_soil8 <- glm(Location~1,
                 family=binomial(link='logit'),
                 data=dat)
anova(GLM_soil7,GLM_soil8)



 ---------------------------------------------
#descriptive stats
summary(GLM_soil)
confint(GLM_soil)
r.squaredGLMM(GLMER_soil)

pr<-resid(GLM_soil, type='pearson')
n<-nrow(Soil_characteristics)
c<-length(coef(GLM_soil))
(Disp<-sum(pr^2)/(n-c))

cv.glm(dat, GLM_soil)



---------------------------------------------
---------------------------------------------
#### *Garlic mustard on Mycorrhiza*####
# mod sel
hist(ndata$Lesions)

mod1.binom<-glmmadmb(binmycorr ~ species + location + species:location +
                       (1|pop),
                     data=ndata, family="binomial") 

mod1.binom.ind<-glmmadmb(binmycorr ~ species + location + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="binomial") 

mod1.poisson.ind <-glmmadmb(Mycorrhiza ~ species + location + species:location +
                    (1|pop)+ (1|indiv),
                  data=ndata, family="poisson")

mod1.poisson <-glmmadmb(Mycorrhiza ~ species + location + species:location +
                         (1|pop),
                       data=ndata, family="poisson")

mod1.nb<-glmmadmb(Mycorrhiza ~ species + location + species:location +
                    (1|pop),
                  data=ndata, family="nbinom1") 


mod1.nb.ind<-glmmadmb(Mycorrhiza ~ species + location + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="nbinom1") 

ICtab(mod1.binom, mod1.binom.ind ,mod1.poisson, mod1.poisson.ind ,mod1.nb, mod1.nb.ind)

pr<-resid(mod1.binom, type='pearson')
n<-nrow(ndata)
c<-length(coef(mod1.binom))
(Disp<-sum(pr^2)/(n-c))

pr<-resid(mod1.poisson, type='pearson')
n<-nrow(ndata)
c<-length(coef(mod1.poisson))
(Disp<-sum(pr^2)/(n-c))

pr<-resid(mod1.nb, type='pearson')
n<-nrow(ndata)
c<-length(coef(mod1.nb))
(Disp<-sum(pr^2)/(n-c))

---------------------------------------------
# LRT
mod1.1<-glmmadmb(binmycorr ~ species + location +
                   (1|pop) + (1|indiv),
               data=ndata, family="binomial")
anova(mod1.1, mod1.binom.ind)

mod1.2<-glmmadmb(binmycorr ~ species + species:location +
                   (1|pop)+ (1|indiv),
               data=ndata, family="binomial")
anova(mod1.2, mod1.binom.ind)

mod1.3<-glmmadmb(binmycorr ~ species +  
                   (1|pop)+ (1|indiv),
               data=ndata, family="binomial")
anova(mod1.3 ,mod1.2)

mod1.4<-glmmadmb(binmycorr ~ 1 + 
                   (1|pop) + (1|indiv),
                 data=ndata, family="binomial")
anova(mod1.4 , mod1.3)


# descriptive stats
pr<-resid(mod1.3, type='pearson')
n<-nrow(ndata)
c<-length(coef(mod1.3))
(Disp<-sum(pr^2)/(n-c))

summary(mod1.3)
data.frame(coef(mod1.2))
confint(mod1.2)

# no efffect on colonization... maybe the plantswere there before GM?
pdf(file="regressioncoefficients_Mycorrhiza.pdf", width=10, height=8)
coefplot2(mod1.3)
dev.off()


---------------------------------------------
---------------------------------------------
#### *Garlic mustard on Lesion*####
# model sel
hist(ndata$Lesions)
mod2.binom<-glmmadmb(binlesion ~ species + location + species:location +
                       (1|pop),
                     data=ndata, family="binomial") 

mod2.binom.ind<-glmmadmb(binlesion ~ species + location + species:location +
                       (1|pop)+ (1|indiv),
                     data=ndata, family="binomial")

mod2.poisson<-glmmadmb(Lesions ~ species + location + species:location +
                    (1|pop),
                  data=ndata, family="poisson") 

mod2.poisson.ind<-glmmadmb(Lesions ~ species + location + species:location +
                         (1|pop)+ (1|indiv),
                       data=ndata, family="poisson")

mod2.nb.ind<-glmmadmb(Lesions ~ species + location + species:location +
                 (1|pop)+ (1|indiv),
               data=ndata, family="nbinom1") 

mod2.nb<-glmmadmb(Lesions ~ species + location + species:location +
                    (1|pop),
                  data=ndata, family="nbinom1") 

ICtab(mod2.binom,mod2.binom.ind,mod2.poisson,mod2.poisson.ind,mod2.nb,mod2.nb.ind)


# LRT
mod2.1<-glmmadmb(binlesion ~ species + location +
                   (1|pop) + (1|indiv),
                 data=ndata, family="binomial")
anova(mod2.1, mod2.binom.ind)

mod2.2<-glmmadmb(binmycorr ~ species + 
                   (1|pop)+ (1|indiv),
                 data=ndata, family="binomial")
anova(mod2.2,mod2.1)

mod2.3<-glmmadmb(binmycorr ~ location +  
                   (1|pop)+ (1|indiv),
                 data=ndata, family="binomial")
anova(mod2.3 ,mod2.1)

mod2.4<-glmmadmb(binmycorr ~ 1 + 
                   (1|pop) + (1|indiv),
                 data=ndata, family="binomial")
anova(mod2.4 , mod2.1)


pr<-resid(mod2.1, type='pearson')
n<-nrow(ndata)
c<-length(coef(mod2.1))
(Disp<-sum(pr^2)/(n-c))

summary(mod2.1)
data.frame(coef(mod2.1))
confint(mod2.1)

pdf(file="regressioncoefficients_Lesions.pdf", width=10, height=8)
coefplot2(mod2.1)
dev.off()


---------------------------------------------
---------------------------------------------
####*location on the correlation of myc and lesion*####

# Yprime = corr coefficient of Myc and Lesion at every cross per indiv

CorrLMERBC<-lmer(Yprime~ species + location + mean_rootsize + 
                   location:species +
                   (1|pop),
                 data=fdata_rs.corr)

CorrLMERBC1<-lmer(Yprime~ species + location + mean_rootsize+
                   (1|pop),
                 data=fdata_rs.corr)
anova(CorrLMERBC1, CorrLMERBC)

CorrLMERBC2<-lmer(Yprime~ species + location + 
                   (1|pop),
                 data=fdata_rs.corr)
anova(CorrLMERBC2, CorrLMERBC1)

CorrLMERBC3<-lmer(Yprime~ species +  
                    (1|pop),
                  data=fdata_rs.corr)
anova(CorrLMERBC3, CorrLMERBC2)

CorrLMERBC4<-lmer(Yprime~ 1+ 
                    (1|pop),
                  data=fdata_rs.corr)
anova(CorrLMERBC3, CorrLMERBC4)

pr<-resid(CorrLMERBC3, type='pearson')
n<-nrow(fdata_rs.corr)
c<-length(coef(CorrLMERBC3))
(Disp<-sum(pr^2)/(n-c))

summary(CorrLMERBC)
confint(CorrLMERBC)

CorrLMER.dredge<-dredge(CorrLMERBC)
r.squaredGLMM(CorrLMERBC)

---------------------------------------------
---------------------------------------------
####Everything on lesion####
var<-fulldata[,c(5,6,8,9,20,21,22)]
covar_cor = cor(var)
corrplot.mixed(covar_cor, upper = "ellipse", lower = "number")

# cant include location because when pop is a random factor locatin 
# is correlated with all the other measurements! They were taken "in" and "out".

mod10.3 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species + 
                      scale(pH)+ scale(Agg_stability) + Nutrient_PC1 + 
                      (1 | pop)+
                      Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + Mycorrhiza:scale(pH) +
                      Mycorrhiza:scale(Agg_stability), 
                    data = fulldata,family = "nbinom1")


mod10.4 <- glmmadmb(Lesion ~ scale(mean_rootsize) + species + 
                          scale(pH)+ scale(Agg_stability) + Nutrient_PC1 + 
                          (1 | pop)+
                          Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                          Mycorrhiza:species + Mycorrhiza:scale(pH) +
                          Mycorrhiza:scale(Agg_stability), 
                        data = fulldata,family = "nbinom1")

ICtab(mod10, mod10.2, mod10.3, mod10.4) # mycorrhiza or not should not be discounted. delta = 2.


mod10.5 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species + 
                      scale(Agg_stability) + Nutrient_PC1 + 
                      (1 | pop)+
                      Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + Mycorrhiza:scale(pH) +
                      Mycorrhiza:scale(Agg_stability), 
                    data = fulldata,family = "nbinom1")

ICtab(mod10.3,mod10.5)

mod10.6 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species + 
                      Nutrient_PC1 + 
                      (1 | pop)+
                      Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + Mycorrhiza:scale(pH) +
                      Mycorrhiza:scale(Agg_stability), 
                    data = fulldata,family = "nbinom1")

ICtab(mod10.6,mod10.5)

mod10.7 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species + 
                      Nutrient_PC1 + 
                      (1 | pop)+
                      Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + 
                      Mycorrhiza:scale(Agg_stability), 
                    data = fulldata,family = "nbinom1")

ICtab(mod10.6,mod10.7)


mod10.8 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species + 
                      Nutrient_PC1 + 
                      (1 | pop)+
                      Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + Mycorrhiza:scale(pH),
                    data = fulldata,family = "nbinom1")
ICtab(mod10.6,mod10.8)


mod10.9 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species+
                      (1 | pop)+
                      Mycorrhiza:Nutrient_PC1 + Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + Mycorrhiza:scale(pH) +
                      Mycorrhiza:scale(Agg_stability), 
                    data = fulldata,family = "nbinom1")
ICtab(mod10.9,mod10.6)

mod10.10 <- glmmadmb(Lesion ~ Mycorrhiza + scale(mean_rootsize) + species + 
                      Nutrient_PC1 + 
                      (1 | pop)+
                      Mycorrhiza:scale(mean_rootsize) +
                      Mycorrhiza:species + Mycorrhiza:scale(pH) +
                      Mycorrhiza:scale(Agg_stability), 
                    data = fulldata,family = "nbinom1")

ICtab(mod10.10,mod10.6)

---------------------------------------------
pr<-resid(mod10.6, type='pearson')
n<-nrow(fulldata)
c<-length(coef(mod10.6))
(Disp<-sum(pr^2)/(n-c))

summary(mod10.6)
confint(mod10.6)

data.frame(coef(mod10.6))
regression<-coefplot2(mod10.6)


