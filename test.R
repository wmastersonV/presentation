rm(list = ls())
# load(file = "SensorFiles/systemFactor.RData")
source("SensorFiles/transformDummy.R")
source("SensorFiles/initialize.R")
source("SensorFiles/nonLinear.R")

# generate simulated failure times for dataset
# Try to standardize features before!
set.seed(1)
dat1 <- f.initialize(sdSystemAge = 7, sdBuilding = 3, sdTemp = 7, sdNonLinear = 1, sampleFlag = TRUE,
                     downSampleTime = 10, fold = 1, type = "nonLinearModel")
str(dat1)
dat1$time <- NULL
# number of terminations
table(dat1$isDeath)
str(dat1)


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
mCox2Init <- coxph(formula(Surv(time2, isDeath) ~ SystemAge + BuildingAge + product * tempDiff), dat1)
mCox2StepAIC <- stepAIC(mCox2Init)
mCox2StepAIC
summary(mCox2StepAIC)

# show mean/median expected lifetime
# obtain list of all times observed in dataset
times = sort(dat1$time)
# obtain predictions for survival probabilities
survProb <- predictSurvProb(mCox2Init, dat1, times)
survProbSort = apply(survProb, 2, function(y) sort(y, decreasing = FALSE))
# find index of survival time at 50th percentile
medLife = lapply(1:dim(survProb)[1], function(y) findInterval(0.5, sort(survProb[y,]),
                                                             rightmost.closed = FALSE,
                                                             all.inside = FALSE))
medLife = unlist(medLife)
#reformat
medLifeFinal <- times[length(times) - medLife]
# plot median survival times
hist(medLifeFinal)

# mean residual life
# vectorized computation of integral S(t) using left Riemann sum method for integration
mRes <- lapply(1:dim(survProb)[1], function(col)
  sum(unlist(lapply(1:(dim(survProb)[2] - 1), function(i)
    (survProb[col,i] * (times[i+1] - times[i]))))))
mRes = unlist(mRes)
hist(mRes)

# difference between mean residual life and median
# mean is less than median
hist(mRes - medLifeFinal)

# predict survival curves for first 10 observations
predCox = predictSurvProb(mCox2StepAIC, dat1[1:10,], times)

# plot predicted survival curves
plot(predCox[1,],col=10, ylab="S(t)", xlab = "time", type = "l", main = "Predicted Survival curves using Cox Model") +
  lines(predCox[3,], col=20) + lines(predCox[5,], col=30)


# survival trees -----------------------------------------------------------
# non-linear model
mTree2 <- rpart(formula = formula(Surv(time2, isDeath) ~ .), cp = .02, data = dat1)
mTree2
rpart.plot(mTree2, main = paste0("Survival Tree, complexity = .02"),  cex=1)

# lower complexity: possibly more overfitting
mTree1 <- rpart(formula = formula(Surv(time2, isDeath) ~ .), cp = .015, data = dat1)
mTree1
rpart.plot(mTree1, main = paste0("Survival Tree, complexity = .01"),  cex=1)

# conditional inference tree
mCtree <- ctree(formula = formula(Surv(time2, isDeath) ~ .), maxdepth = 2, data = dat1)
plot(mCtree)

# use fold-1 for predictions
mPecTree <- pecRpart(formula = formula(Surv(time2, isDeath) ~ .), cp = .01, data = dat1)
predTree = predictSurvProb(mPecTree, dat1[1:10,], times)

plot(predTree[1,], ylab="S(t)", xlab = "time", type = "l", main = "Predicted Survival curves using Tree", col = 10) +
  lines(predTree[3,],  col = 20) + lines(predTree[5,], col=30)

# random survival forest --------------------------------------------------
# non-linear ensemble model
# get subset
mRSF <- rfsrc(formula(Surv(time2, isDeath) ~ .), dat1, ntree = 100)
mRSF
plot(mRSF)

# random survival forest output
plot.survival(mRSF)

predForest = predictSurvProb(mRSF, dat1[1:10,], times)
dev.off()
plot(predForest[1,], col=10, ylab="S(t)", xlab = "time", type = "l", main = "Predicted Survival curves using Random Survival Forest") +
  lines(predForest[3,], col=20) + lines(predForest[5,], col=30)

# compare predicted survival curves ------------------------------------------
# predicted survival curves
plot(predTree[1,], type = "l", main = "obs 1: predicted S(t) for different models", col = 10) +
  lines(predForest[1,], col=20) +
  lines(predCox[1,], col=30)

# random survival forest vs Cox--------------------------------------------------
mRSF1 <- rfsrc(formula(Surv(time2, isDeath) ~ BuildingID + System   +   SystemAge   + BuildingAge + product  +   Country +    tempDiff  ),
               dat1, ntree = 100)
mCox2Init <- coxph(formula(Surv(time2, isDeath) ~  BuildingID + System   +   SystemAge   + BuildingAge + product  +   Country +    tempDiff), dat1)
mCox2StepAIC <- stepAIC(mCox2Init)
mCox2StepAIC
p1 <- pec( list(mCox2StepAIC, mRSF1), formula = Surv(time2, isDeath) ~  BuildingID + System   +   SystemAge   + BuildingAge + product  +   Country +    tempDiff ,
           data = dat1, splitMethod = "cv2", cens.model = "marginal")
p1

# SVM: may need to dummy transform variables
# too slow
# t100 = survsvm(formula(Surv(time2, isDeath) ~   System   +   SystemAge   + BuildingAge   +    tempDiff),
#             data = dat1[1:600,], cox.score = TRUE, max.iter=100)
# t10000 = survsvm(formula(Surv(time2, isDeath) ~  System   +   SystemAge   + BuildingAge   +    tempDiff),
#             data = dat1[1:600,], kernel = "linear", max.iter=10000)
#
# predict(t100, dat1[400:798,])


# pec: prediction error curve analysis ------------------------------------
fitForm <- formula(Surv(time2, isDeath) ~ BuildingID  + System  +  SystemAge +  BuildingMgr + BuildingAge + product  + Country + tempDiff)
fitForm2 <- formula(Surv(time2, isDeath) ~ System  +  SystemAge  + BuildingAge + product + tempDiff)

set.seed(1)
mtree1 = pecRpart(fitForm, cp = .02, data = dat1, y = TRUE)
set.seed(1)
mforest = rfsrc(fitForm, data = dat1, ntree = 1000)
set.seed(1)
mCForest <- pecCforest(fitForm, data = dat1, controls = cforest_classical(ntree = 1000))

Models <- list("forest" = mforest,
               "mCForest" = mCForest,
               "mCox2StepAIC" = mCox2StepAIC)
set.seed(1)

# run 10 fold CV
# IPCW weights: use Kaplan Mier
# pecOut <- pec( Models,
#                formula = fitForm,
#                data = dat1,
#                splitMethod = "cv10")

readRDS(file = "pecOut.RData")
# Integrated Brier score (crps):
#
#   IBS[0;time=8.5)
# Reference              0.108
# forest                 0.086
# mCForest               0.096
# mCox2StepAIC           0.099

# save serialized R object
# saveRDS(pecOut, file = "SensorFiles/pecOut.rds")


# gg_md <- gg_minimal_depth(mRSF1, lbls = st.labs)
# plot(gg_md)
# vimPlot = gg_vimp(mRSF1)
# plot.variable(mRSF1,vimPlot$vars[c(1)], surv.type = c("mort"))
# gg_v <- gg_variable(mRSF1, time = c(15, 29),time.labels = c("survival > 30 days", "survival > 90 days"))
# plot(gg_v, xvar = c("SystemAge"), se = .95, alpha = .4)
# plot(gg_rfsrc(mRSF1, by="product"))
