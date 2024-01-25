# install.packages(c("MatchIt","optmatch","cobalt","lmtest","sandwich","boot","devtools"))
# install.packages("https://cran.r-project.org/src/contrib/Archive/Zelig/Zelig_5.1.7.tar.gz", 
#                 repos=NULL, type="source")
# devtools::install_github("cardiomoon/webrPSM")


## ----------------------------------------------------------------------------------------------------
library(MatchIt)
library(cobalt)
library(lmtest)    # coeftest
library(sandwich)   #vcovCL
library(boot)  # boot()
library(webrPSM)


## ----------------------------------------------------------------------------------------------------
str(lalonde)


## ----------------------------------------------------------------------------------------------------
out1 <- matchit(treat ~ age + educ + race + married + nodegree + re74 + re75, data = lalonde,
                  method = "nearest", distance = "glm")
out1


## ------------------------------------------------------------------------------------------
summary(out1)


## ----------------------------------------------------------------------------------------------------
plot(summary(out1))


## ----------------------------------------------------------------------------------------------------
PSMTable(out1)


## ----------------------------------------------------------------------------------------------------
PSMTable(out1,grouplabel=c("Control","Treated"))


## ----------------------------------------------------------------------------------------------------
out2 <- matchit(treat ~ age + educ + race + married + 
                          nodegree + re74 + re75, data = lalonde,
                  method = "full",
                  link="probit",
                  distance = "glm")
out2


## ------------------------------------------------------------------------------------------
summary(out2)


## ----------------------------------------------------------------------------------------------------
plot(summary(out2))


## ----------------------------------------------------------------------------------------------------
PSMTable(out2,grouplabel=c("Control","Treated"))


## ----tidy=FALSE--------------------------------------------------------------------------------------
matchedData=match.data(out2)
head(matchedData)


## ----------------------------------------------------------------------------------------------------
fit=lm(re78~treat, 
       data=matchedData,weights=weights)


## ----------------------------------------------------------------------------------------------------
fit=lm(re78~treat+age+educ+race+married+nodegree+re74+re75,
       data=matchedData, weights=weights)


## ----highlight.output=c(6)---------------------------------------------------------------------------
coeftest(fit,vcov.=vcovCL,cluster=~subclass)


## ----highlight.output=c(3)---------------------------------------------------------------------------
coefci(fit,vcov.=vcovCL,cluster=~subclass)


## ----------------------------------------------------------------------------------------------------
pair_ids <- levels(matchedData$subclass)
est_fun <- function(pairs, i) {
        
        #Compute number of times each pair is present
        numreps <- table(pairs[i])
        
        #For each pair p, copy corresponding md row indices numreps[p] times
        ids <- unlist(lapply(pair_ids[pair_ids %in% names(numreps)],
                             function(p) rep(which(matchedData$subclass == p), 
                                             numreps[p])))
        
        #Subset md with block bootstrapped ids
        md_boot <- matchedData[ids,]
        
        #Effect estimation
        fit_boot <- lm(re78~treat+age+educ+race+married+nodegree+re74+re75,
                       data = md_boot, weights = weights)
        
        #Return the coefficient on treatment
        return(coef(fit_boot)["treat"])
}


## ----------------------------------------------------------------------------------------------------
set.seed(1)
(boot_est <- boot(pair_ids, est_fun, R = 499))
boot.ci(boot_est, type = "bca")


## ------------------------------------------------------------------------------------------
library(webrPSM)
reportPSM(out2)


## ------------------------------------------------------------------------------------------
estimateEffect(out2,dep="re78",multiple=TRUE)


## ----------------------------------------------------------------------------------------------------
estimateEffect2(out2,dep="re78",multiple=TRUE)


