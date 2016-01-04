rm(list = ls())
# load(file = "~/presentation/SensorFiles/systemFactor.RData")

source("~/presentation/SensorFiles/transformDummy.R")
source("~/presentation/SensorFiles/initialize.R")
source("~/presentation/SensorFiles/nonLinear.R")

# generate simulated failure times for dataset
# Try to standardize features before!
set.seed(1)

dat1 <- f.initialize(sdSystemAge = 7, sdBuilding = 3, sdTemp = 7, sdNonLinear = 1, sampleFlag = TRUE,
                     downSampleTime = 10, fold = 1, type = "nonLinearModel")
dat1$time <- NULL
table(dat1$isDeath)

mRSF1 <- rfsrc(formula(Surv(time2, isDeath) ~ BuildingID + System   +   SystemAge   + BuildingAge + product  +   Country +    tempDiff  ), 
               dat1, ntree = 1)
mCox2Init <- coxph(formula(Surv(time2, isDeath) ~  BuildingID + System   +   SystemAge   + BuildingAge + product  +   Country +    tempDiff), dat1)
mCox2StepAIC <- stepAIC(mCox2Init)
mCox2StepAIC
p1 <- pec( list(mCox2StepAIC, mRSF1), formula = Surv(time2, isDeath) ~ 1, data = dat1,splitMethod = "cv10")
p1

# SVM: may need to dummy transform variables
# too slow
# t100 = survsvm(formula(Surv(time2, isDeath) ~   System   +   SystemAge   + BuildingAge   +    tempDiff), 
#             data = dat1[1:600,], cox.score = TRUE, max.iter=100)
# t10000 = survsvm(formula(Surv(time2, isDeath) ~  System   +   SystemAge   + BuildingAge   +    tempDiff), 
#             data = dat1[1:600,], kernel = "linear", max.iter=10000)
# 
# predict(t100, dat1[400:798,])

# look at features
str(dat1)
table(dat1$isDeath)

# KM model ----------------------------------------------------------
# non-parametric model with no covariates
# similar to k-nearest neighbors
mKM <- prodlim(Surv(time2, isDeath) ~ 1, data=dat1)
summary(mKM)
plot(mKM)

# cox model ---------------------------------------------------------------
# semi-parametric linear model
# parametric portion: similar to linear regression 
# non-parametric portion: baseline survival curve, similar to Kaplan-Meier

mCox1 <- cph(formula(Surv(time2, isDeath) ~ .), dat1, singular.ok = TRUE)
# explain what is meant by this error
mCox1
set.seed(1)
mCox2Init <- coxph(formula(Surv(time, isDeath) ~ SystemAge + BuildingAge + product * tempDiff), dat1)
mCox2StepAIC <- stepAIC(mCox2Init)
mCox2StepAIC
summary(mCox2StepAIC)

times = .01*(1:(100*max(dat1$time) - 10))

# use fold 1 for predictions
predCox = predictSurvProb(mCox2StepAIC, dat1[1:10,], times)

# predicted survival curves
plot(predCox[1,],col=50, ylab="S(t)", xlab = "time", type = "l", main = "Predicted Survival curves using Cox Model") + 
  lines(predCox[2,], col=100) + lines(predCox[4,], col=1)

# survival tree -----------------------------------------------------------
# non-linear model
mTree2 <- rpart(formula = formula(Surv(time2, isDeath) ~ .), cp = .02, data = dat1)
mTree2
rpart.plot(mTree2, main = paste0("Survival Tree, complexity = .02"),  cex=1)

# lower complexity: possibly more overfitting
mTree1 <- rpart(formula = formula(Surv(time2, isDeath) ~ .), cp = .015, data = dat1)
mTree1
rpart.plot(mTree1, main = paste0("Survival Tree, complexity = .01"),  cex=1)

mCtree <- ctree(formula = formula(Surv(time, isDeath) ~ .), maxdepth = 2, data = dat1)
plot(mCtree)

# use fold-1 for predictions
mPecTree <- pecRpart(formula = formula(Surv(time2, isDeath) ~ .), cp = .01, data = dat1)
predTree = predictSurvProb(mPecTree, dat1[1:10,], times)[, 1:2914]

# plot(predTree[1,], ylab="S(t)", xlab = "time", type = "l", main = "Predicted Survival curves using Tree") + lines(predTree[2,]) + lines(predTree[4,])


# random survival forest --------------------------------------------------
# non-linear ensemble model
# get subset
mRSF <- rfsrc(formula(Surv(time, isDeath) ~ .), dat1, ntree = 100)
mRSF
plot(mRSF2)

predForest = predictSurvProb(mRSF, dat2[1:10,], times)
plot(predForest[1,], col=50, ylab="S(t)", xlab = "time", type = "l", main = "Predicted Survival curves using Random Survival Forest") + 
  lines(predForest[2,], col=100) + lines(predForest[4,], col=1)

# plot predicted survival curves ------------------------------------------
# predicted survival curves
plot(predTree[1,], type = "l", main = "obs 1: predicted S(t)", col = 92) 
+ lines(predForest[1,], col=50) 
+ lines(predCox[1,], col=30) 

# random survival forest output
plot.survival(mRSF) 

# pec: prediction error curve analysis ------------------------------------
fitForm <- formula(Surv(time, isDeath) ~ BuildingID  + System  +  SystemAge +  BuildingMgr + BuildingAge + product  + Country + tempDiff)
fitForm2 <- formula(Surv(time, isDeath) ~ System  +  SystemAge  + BuildingAge + product + tempDiff)

set.seed(1)
mtree1 = pecRpart(fitForm, cp = .02, data = dat1, y = TRUE)
set.seed(1)
mforest = rfsrc(fitForm, data = dat1, ntree = 1000)
set.seed(1)
mCForest <- pecCforest(fitForm, data = dat1, controls = cforest_classical(ntree = 1000))
set.seed(1)
mCoxNoStepAIC <- coxph(fitForm2, data = dat1 )

Models <- list("forest" = mforest,
               "mCForest" = mCForest,
               "mCox2StepAIC" = mCox2StepAIC,
               "mCoxNoStepAIC" = mCoxNoStepAIC)
set.seed(1)

# run 10 fold CV
# IPCW weights: use Kaplan Mier
pecOut <- pec( Models, 
               formula = Surv(time, isDeath) ~ 1,
               data = dat1,
               splitMethod = "cv10")

# save.image(file = "systemFactor.RData")

# gg_md <- gg_minimal_depth(mRSF1, lbls = st.labs)
# plot(gg_md)
# vimPlot = gg_vimp(mRSF1)
# plot.variable(mRSF1,vimPlot$vars[c(1)], surv.type = c("mort"))
# gg_v <- gg_variable(mRSF1, time = c(15, 29),time.labels = c("survival > 30 days", "survival > 90 days"))
# plot(gg_v, xvar = c("SystemAge"), se = .95, alpha = .4)
# plot(gg_rfsrc(mRSF1, by="product"))                 




# survival svm
library(survpack)
