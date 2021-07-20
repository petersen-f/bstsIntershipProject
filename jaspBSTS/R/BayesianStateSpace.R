#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


# TODO: - add Footnote when timeout was reached before mcmc draws were sampled completly
#       - integrate MCMC draw specification
#       - fix mcmc burn in for state component plot
# Main function ----
bayesianStateSpace <- function(jaspResults, dataset, options) {
  # Set title

  # check if results can be computed
  ready <- (options$dependent != "" & any(options[c("checkboxAr","checkboxLocalLevel","checkboxLocalLinearTrend")]==T,length(options$seasonalities)>0))
  # Init options: add variables to options to be used in the remainder of the analysis

  # read dataset



  dataset <- .bstsReadData(options,ready)
  # error checking
  .bstsErrorHandling(dataset, options)

  # Compute (a list of) results from which tables and plots can be created
  .bstsComputeResults(jaspResults, dataset, options,ready)

  # Compute burn amount and pass it to/create options$burn
  options <- .bstsBurnHelper(jaspResults,options)
  options <- .bstsTimeHelper(jaspResults,dataset,options)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  #.bstsCreateContainerMain(jaspResults,options,ready)
  .bstsCreateModelSummaryTable(jaspResults,options,ready)
  .bstsCreateCoefficientTable(jaspResults,options,ready)
  .bstsCreateStatePlots(jaspResults,dataset,options,ready)
  .bstsCreatePredictionPlot(jaspResults,options,ready)
  .bstsCreateControlPlots(jaspResults,options,ready)

  # Only to test certain plot things without having to put them in another container

  #.bstsSimplePlot(jaspResults,options)

  return()
}

# Init functions ----
.bstsInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis

  return(options)
}

.bstsReadData <- function(options,ready) {
  # Read in the dataset using the built-in functions
  if(!ready) return()
  numericVariables  <- c(options$dependent,unlist(options$covariates))
  numericVariables  <- numericVariables[numericVariables != ""]
  nominalVars       <- unlist(options$factors)
  timeVar           <- unlist(options$dates)
  timeVar           <- timeVar[timeVar != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric  = numericVariables,
                              columns.as.factor = nominalVars,
                              columns = timeVar)

  return(dataset)


}

.bstsErrorHandling <- function(dataset, options) {
  # Custom function to check whether we missing values in predictors
  # Error 1: Any missing values in predictors?
  # Doesn't work for some reason
  for (covariate in options$covariates) {
    .hasErrors(dataset = dataset,
              type = 'missingValues',
              missingValues.target = covariate,
              exitAnalysisIfErrors = TRUE)

  }

}

.bstsModelDependencies <- function(options) {
  return(c("dependent",
            "covariates",
            "postSummaryTable",
            "expectedModelSize",
            "posteriorSummaryCoefCredibleIntervalValue",
            "distFam",
            "mcmcDraws",
            "modelTerms",
            "seasonalities",
            "checkboxAr","lagSelectionMethod","noLags","maxNoLags","arSdPrior","arSigmaGuess","arSigmaWeight",
            "checkboxLocalLevel",'localLevelSdPrior','localLevelSigmaGuess','localLevelSigmaWeight',
            "checkboxLocalLinearTrend",'lltLevelPrior','lltLevelSigmaGuess','lltLevelSigmaWeight','lltSlopePrior',
            'lltSlopeSigmaGuess','lltSlopeSigmaWeight',
            "checkboxDynReg"
          ))
}
.bstsStatePlotDependencies <- function(options){
  return(c('checkboxPlotAggregatedStates','ciAggregatedStates',"actualValuesAggregatedStates",'checkboxPlotComponentStates'))
}

.bstsPredictionDependencies <- function(options){
  return(c("predictionHorizon"))
}


.bstsControlDependencies <- function(options){
  return(c('checkControlChart',"controlPeriod","controlSigma","checkControlProbPlot"))
}


# Results functions "----",

.bstsComputeResults <- function(jaspResults, dataset, options,ready) {
  if (!ready) return()

  if (is.null(jaspResults[["bstsMainContainer"]])) {
    bstsMainContainer <- createJaspContainer()
    jaspResults[["bstsMainContainer"]] <- bstsMainContainer

    jaspResults[["bstsMainContainer"]]$dependOn(.bstsModelDependencies(options))
  }

  if(is.null(jaspResults[["bstsMainContainer"]][["bstsModelResults"]])) {
    bstsModelResultsState <- createJaspState()

    bstsModelResults <- .bstsResultsHelper(dataset,options)
    bstsModelResultsState$object <- bstsModelResults
    jaspResults[["bstsMainContainer"]][["bstsModelResults"]] <- bstsModelResultsState
  }



  if (is.null(jaspResults[["bstsMainContainer"]][["bstsModelPredictions"]]) & options$predictionHorizon >0) {
    bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
    bstsModelPredictionsState <- createJaspState()

    bstsPredictionResults <- bsts::predict.bsts(object = bstsResults,horizon=options$predictionHorizon)
    bstsModelPredictionsState$object <- bstsPredictionResults
    jaspResults[["bstsMainContainer"]][["bstsModelPredictions"]] <- bstsModelPredictionsState
  }

  return()
}

.bstsResultsHelper <- function(dataset,options) {

  y     <- dataset[[encodeColNames(options$dependent)]]
  #y <- as.numeric(y)
  data <- data.frame(y=y)
  #covs <- unlist(options$covariates,options$factors)

  #for(cov in covs) {
  #  data[[cov]] <- dataset[[encodeColNames(cov)]]
  #}

  predictors = NULL
  if (length(options$covariates)>0|length(options$factors) >0)
    predictors <- .bstsGetPredictors(options$modelTerms)
  formula = .bstsGetFormula(dependent=y,predictors = predictors)

  for(predictor in predictors){
    data[[predictor]] <- dataset[[encodeColNames(predictor)]]
  }



  #data <- data.frame(y=y)
  ss   <- list()
  #AddAr
  if(options$checkboxAr){
    if(options$lagSelectionMethod == "manualAR")
      ss <- bsts::AddAr(ss,y = data$y,lags =options$noLags)

    if(options$lagSelectionMethod == "autoAR")
      ss <- bsts::AddAutoAr(ss,y=data$y,lags=options$maxNoLags)
  }

  #Add local level Component

  if(options$checkboxLocalLevel)
    ss <- bsts::AddLocalLevel(ss,y=data$y)

  # Add Local Linear trend component

  if(options$checkboxLocalLinearTrend)
    ss <- bsts::AddLocalLinearTrend(ss,y=y)


  if(options$checkboxDynReg & options$DynRegLags==0)
    ss <- bsts::AddDynamicRegression(ss,formula=formula,data=data)


  if (!is.null(options$seasonalities)) {

    for (seas in options$seasonalities) {

      sigma.prior <- if(seas$sigma.guess=="") NULL else{Boom::SdPrior(as.numeric(seas$sigma.guess),seas$sample.size)}
      normal.prior <- if(seas$sigma=="") NULL else Boom::NormalPrior(seas$mu,as.numeric(seas$sigma))
      ss <- bsts::AddSeasonal(ss,
                        y = y,
                        nseasons = seas$nSeason,
                        season.duration = seas$seasonDuration,
                        sigma.prior = sigma.prior,
                        initial.state.prior = normal.prior
                      )
      if(!seas$name == "")
        ss[[length(ss)]]$name <- seas$name

     }
  }




  model <- bsts::bsts(formula = formula,
                data=dataset,
                state.specification = ss,
                niter = options$mcmcDraws,
                timestamps=NULL,
                seed = options$seed,
                expected.model.size = options$expectedModelSize,
                model.options = bsts::BstsOptions(timeout.seconds = options$timeout )
                )

  return(model)
}





# Helper function to calculate burn-in amount and pass to options$burn-> needed for summary,plots,prediction
.bstsBurnHelper <- function(jaspResults,options) {

  #bstsResults <- jaspResults[["stateBstsResults"]]$object
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
  if(options$burnSpecification == "burnSuggested")
    options$burn <- bsts::SuggestBurn(options$propBurnSuggested,bstsResults)

  if(options$burnSpecification == "burnManual")
    options$burn <- options$numberBurnManual





  return(options)
}


.bstsTimeHelper <- function(jaspResults,dataset,options){
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  actualValues <- as.numeric(bstsResults$original.series)
  if(options$dates != "")
    options$time <- as.POSIXct(dataset[[encodeColNames(options$dates)]], tz = "UTC")
  else
    options$time <- 1:length(actualValues)

  return(options)

}








# Helper function to create regression formula that is passed into bsts functions

.bstsGetPredictors <- function(modelTerms, modelType = "alternative", encoded = TRUE) {

  if (!is.character(modelType) || !modelType %in% c("alternative", "null"))
    stop(gettext("Unknown value provided for modelType, possible values: `alternative`, `null`"))

  predictors <- NULL

  for (i in seq_along(modelTerms)) {
    components <- unlist(modelTerms[[i]]$components)
    if (encoded)
      components <- .v(components)
    predictor <- paste0(components, collapse = ":")

    if (modelType == "alternative") {
      predictors <- c(predictors, predictor)
    } else if (modelType == "null") {
      isNuisance <- modelTerms[[i]]$isNuisance
      if (isNuisance)
        predictors <- c(predictors, predictor)
    }
  }

  return(predictors)
}

.bstsGetFormula <- function(dependent, predictors = NULL, includeConstant) {


  if (is.null(predictors))
    # if we don't have any predictors, the bsts function takes a vector as input
    return(dependent)
  else
    # if bsts has regression then we need an actual formula
    dependent = "y"
    formula <- paste(dependent, "~", paste(predictors, collapse = "+"))

  return(as.formula(formula, env = parent.frame(1)))
}



# helper function that determines which percentile rank the 2*sigma threshold is in the credible interval for each state
quantInv <- function(distr, value){

  if(sum(distr < value) == length(distr))
    return(1) # 1 if valuelarger than all MCMC draws
  else
    ecdf(distr)(value)
}





# Output functions ----
.bstsCreateContainerMain <- function(jaspResults, options,ready) {
  #if (!ready) return()

  if (!is.null(jaspResults[["bstsMainContainer"]])) return()

  bstsMainContainer <- createJaspContainer()
  jaspResults[["bstsMainContainer"]] <- bstsMainContainer

  jaspResults[["bstsMainContainer"]]$dependOn(.bstsModelDependencies(options))

  return()
}


.bstsCreateModelSummaryTable <- function(jaspResults,options,ready){
  if(!is.null(jaspResults[["bstsMainContainer"]][["bstsModelSummaryTable"]])) return()

  #bstsResults <- jaspResults[["stateBstsResults"]]$object
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  bstsTable <- createJaspTable(title = gettext("Model Summary"))
  bstsTable$position <- 1

  bstsTable$addColumnInfo(name="resSd",   title=gettext("Residual SD"),               type= "number")
  bstsTable$addColumnInfo(name="predSd",  title=gettext("Prediction SD"),             type= "number")
  bstsTable$addColumnInfo(name="R2",      title =gettextf("R%s", "\u00B2"),           type= "number")
  bstsTable$addColumnInfo(name="relGof",  title=gettext("Harvey's goodness of fit"),  type= "number")

  #if (bstsResults$niter < options$mcmcDraws)
  #  bstsTable$addFootnote(message=)

  .bstsFillModelSummaryTable(bstsTable,bstsResults,ready)

  jaspResults[["bstsMainContainer"]][["bstsModelSummaryTable"]] <- bstsTable

  return()
}


.bstsFillModelSummaryTable <- function(bstsTable,bstsResults,ready) {
  if(!ready) return()

  res <- summary(bstsResults)

  bstsTable$addRows(list(
    resSd   = res$residual.sd,
    predSd   = res$prediction.sd,
    R2      = res$rsquare,
    relGof  = res$relative.gof

  ))

}

# table for regression coefficients"
.bstsCreateCoefficientTable <- function(jaspResults,options,ready){
  if(!is.null(jaspResults[["bstsMainContainer"]][["bstsCoefficientSummaryTable"]]) | !length(options$modelTerms) >0 | !ready | !options$postSummaryTable) return()


  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object
  bstsCoefficientTable <- createJaspTable(title = gettext("Posterior Summary of Coefficients"))
  bstsCoefficientTable$dependOn(c("postSummaryTable","showCoefMeanInc","posteriorSummaryCoefCredibleIntervalValue"))
  bstsCoefficientTable$position <- 2

  overtitle <- gettextf("%s%% Credible Interval", format(100*options[["posteriorSummaryCoefCredibleIntervalValue"]], digits = 3))
  bstsCoefficientTable$addColumnInfo(name = "coef",       title = gettext("Coefficients"),           type = "string")
  bstsCoefficientTable$addColumnInfo(name = "priorIncP",  title = gettext("P(incl)"),                type = "number")
  bstsCoefficientTable$addColumnInfo(name = "postIncP",   title = gettext("P(incl|data)"),           type = "number")
  bstsCoefficientTable$addColumnInfo(name = "BFinc",      title = gettext("BF<sub>inclusion</sub>"), type = "number")
  bstsCoefficientTable$addColumnInfo(name = 'mean',       title = gettext("Mean"),                   type = "number", format= "dp:3")
  bstsCoefficientTable$addColumnInfo(name = "sd",         title = gettext("SD"),                     type = "number",format= "dp:3")
  if (options$showCoefMeanInc){
    bstsCoefficientTable$addColumnInfo(name = 'meanInc',  title = gettext("Mean<sub>inclusion</sub>"), type = "number")
    bstsCoefficientTable$addColumnInfo(name = "sdInc",    title = gettext("SD<sub>inclusion</sub>"), type = "number")
  }
  bstsCoefficientTable$addColumnInfo(name = "lowerCri",   title = gettext("Lower"),         type = "number", overtitle = overtitle)
  bstsCoefficientTable$addColumnInfo(name = "upperCri",   title = gettext("Upper"),         type = "number", overtitle = overtitle)

  .bstsFillCoefficientTable(bstsResults,bstsCoefficientTable,options,ready)

  jaspResults[["bstsMainContainer"]][["bstsCoefficientSummaryTable"]] <- bstsCoefficientTable

}

.bstsFillCoefficientTable <- function(bstsResults,bstsCoefficientTable,options,ready) {
  res <- as.data.frame(summary(bstsResults,order = F)$coefficients)
  res$priorInc <- bstsResults$prior$prior.inclusion.probabilities
  res$BFinc <- res$inc.prob/(1-res$inc.prob)


  # TODO: figure out how to get CI for factor variables
  condQuantile <- function(beta,ci){
    #beta <- beta[beta != 0]
    if (length(beta)>0)
      return(quantile(beta,ci))
    return(0)
  }
  ci <- options$posteriorSummaryCoefCredibleIntervalValue
  res$lo_ci <- apply(bstsResults$coefficients, 2,condQuantile,((1- ci)/2))
  res$hi_ci <- apply(bstsResults$coefficients, 2,condQuantile,1-((1- ci)/2))
  res <- res[order(res$inc.prob,decreasing = T),]


  for (i in 1:nrow(res)) {
    row <- list(
      coef = rownames(res[i,]),
      priorIncP = res$priorInc[i],
      postIncP = res$inc.prob[i],
      BFinc = res$BFinc[i],
      mean = res$mean[i],
      sd = res$sd[i],
      #meanInc = res$mean.inc[i],
      #sdInc = res$sd.inc[i],
      lowerCri = res$lo_ci[i],
      upperCri = res$hi_ci[i]
    )

    if(options$showCoefMeanInc){
      row <- append(row,c(meanInc = res$mean.inc[i],
                          sdInc = res$sd.inc[i]) )
    }

      bstsCoefficientTable$addRows(row)

    }


}


.bstsCreateStatePlots <- function(jaspResults,dataset,options,ready) {


  if (!is.null(jaspResults[["bstsStatePlots"]])) return()


  bstsStatePlots <- createJaspContainer(title = gettext("State Plots"))


  bstsStatePlots$dependOn(.bstsStatePlotDependencies(options))

  #bstsResults <- jaspResults[["stateBstsResults"]]$object
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  if(options$checkboxPlotAggregatedStates) .bstsAggregatedStatePlot(bstsStatePlots,bstsResults,dataset,options,ready)
  if(options$checkboxPlotComponentStates)
    .bstsComponentStatePlot(bstsStatePlots,bstsResults,options,ready)


  jaspResults[["bstsMainContainer"]][["bstsStatePlots"]] <- bstsStatePlots
  return()
}

.bstsAggregatedStatePlot <- function(bstsStatePlots,bstsResults,dataset,options,ready) {
  if (!ready | !options$checkboxPlotAggregatedStates) return()


  bstsAggregatedStatePlot <- createJaspPlot(title= gettext("Aggregated State"), height = 320, width = 480)
  #bstsAggregatedStatePlot$dependOn(c("ciAggregatedStates"))

  # get all states
  state <- bstsResults$state.contribution
  # discard burn ins
  state <- state[-(1:options$burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)
  actualValues <- as.numeric(bstsResults$original.series)



  ymin <- apply(state,2,quantile,probs= ((1- options$ciAggregatedStates)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- options$ciAggregatedStates)/2))

  time <- options$time

  mean <-colMeans(state)


  p <-  ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean)) +
        ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax),
                            fill ="blue",alpha=0.5) + ggplot2::xlab("Time") +
        ggplot2::ylab("Distribution") +
        ggplot2::geom_line(size=0.7)
      #  ggplot2::theme_classic()


  if(options$actualValuesAggregatedStates)
    p <- p + ggplot2::geom_point(ggplot2::aes(y=actualValues))

  p <- jaspGraphs::themeJasp(p)

  bstsAggregatedStatePlot$plotObject <- p

  bstsStatePlots[["bstsAggregatedStatePlot"]] <- bstsAggregatedStatePlot

  return()
}



.bstsComponentStatePlot <- function(bstsStatePlots,bstsResults,options,ready){

  bstsComponentStatePlot <- createJaspPlot(title= gettext("Component States"), height = 320, width = 480)



  means <- as.data.frame(apply(bstsResults$state.contribution, 2, colMeans))

  means$time <- options$time

  ymin <- as.data.frame(apply(bstsResults$state.contribution, 2,matrixStats::colQuantiles,probs=c(0.025)))
  ymax <- as.data.frame(apply(bstsResults$state.contribution, 2,matrixStats::colQuantiles,probs=c(0.975)))
  ymin$time <- options$time
  ymax$time <- options$time

  ymin <- reshape2::melt(ymin,id.vars='time',variable.name="component")
  ymax <- reshape2::melt(ymax,id.vars='time',variable.name="component")
  means2 <- reshape2::melt(means,id.vars='time',variable.name="component")

  p <-  ggplot2::ggplot(data=means2, ggplot2::aes(x=time, y=value)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=ymin$value,ymax=ymax$value),
                        fill ="blue",alpha=0.5) +
                        ggplot2::theme_bw() + ggplot2::theme(legend.title = ggplot2::element_blank()) + ggplot2::ylab("") + ggplot2::xlab("")   +
                        ggplot2::facet_grid(component ~ ., scales="free") +
                        ggplot2::guides(colour=FALSE) +
                        ggplot2::theme(axis.text.x=ggplot2::element_text(angle = -90, hjust = 0))


  bstsComponentStatePlot$plotObject <- p

  bstsStatePlots[["bstsComponentStatePlot"]] <- bstsComponentStatePlot

  return()
}


.bstsCreatePredictionPlot <- function(jaspResults,options,ready) {
  if(!ready | !options$predictionHorizon>0) return()

  bstsPredictionPlot <- createJaspPlot(title="Prediction plot", height = 320, width = 480)


  bstsModelResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object

  bstsPredictionResults <- jaspResults[["bstsMainContainer"]][["bstsModelPredictions"]]$object

  predictionRange <- (length(bstsPredictionResults$original.series)+1):(length(bstsPredictionResults$original.series)+options$predictionHorizon)

  predDF <- data.frame(time=seq_along(bstsPredictionResults$original.series),mean=bstsPredictionResults$original.series)
  predDF[predictionRange,"mean"] <-bstsPredictionResults$mean
  predDF[predictionRange,"LL"] <- bstsPredictionResults$interval[1,]
  predDF[predictionRange,"UL"] <- bstsPredictionResults$interval[2,]
  predDF[predictionRange,"time"] <- predictionRange

  p <- ggplot2::ggplot(data=predDF,ggplot2::aes(x=time,y=mean)) +
  ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=LL,ymax=UL),
  fill ="blue",alpha=0.5) +
  ggplot2::xlab("Time") +
  ggplot2::geom_line(size=0.7) +
  ggplot2::geom_vline(xintercept=length(bstsPredictionResults$original.series),linetype=2) +
  ggplot2::theme_classic()

  p <- jaspGraphs::themeJasp(p)

  bstsPredictionPlot$plotObject <- p
  jaspResults[["bstsMainContainer"]][["bstsPredictionPlot"]] <- bstsPredictionPlot

  return()
}


# control plot

.bstsCreateControlPlots <- function(jaspResults,options,ready){
  if (!is.null(jaspResults[["bstsControlPlots"]]) | !ready) return()


  bstsControlPlots <- createJaspContainer(title = gettext("Control Plots"))

  bstsControlPlots$dependOn(.bstsControlDependencies(options))
  bstsResults <- jaspResults[["bstsMainContainer"]][["bstsModelResults"]]$object



  if(options$checkControlChart)
    .bstsFillControlPlotThreshold(bstsControlPlots,bstsResults,options,ready)

  if(options$checkControlProbPlot)
    .bstsFillControlPlotProbability(bstsControlPlots,bstsResults,options,ready)


  jaspResults[["bstsMainContainer"]][["bstsControlPlots"]] <- bstsControlPlots
  return()

}

.bstsFillControlPlotThreshold <- function(bstsControlPlots,bstsResults,options,ready){

  control = c(1:options$controlPeriod)
  L = options$controlSigma
  CI=.95 # add option in JASP later
  bstsControlPlotThreshold <- createJaspPlot(title="Threshold Plot", height = 320, width = 480)

  # extract model components
  state <- bstsResults$state.contribution
  # discard burn ins
  state <- state[-(1:options$burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)

  # determine 0.95% credible interval
  ymin <- apply(state,2,quantile,probs= ((1- CI)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- CI)/2))

  time <- options$time
  mean <-colMeans(state)

  # compute limits based on the estimated state
  state_mean <- mean(mean[control])
  state_sd <- sd(mean[control])


  ul_state <- state_mean + L* state_sd
  ll_state <- state_mean - L* state_sd

  # ymin is lower 95 cI bound of state
  # ul is the threshold that has to be exceeded according to 2 sigmas
  # thus ymin must be larger  than the ul threshold
  CI_reached <- ymin > ul_state # cases where 95% confident that above threshold


  #print(paste0("First date where 2*sigma is exeeded by ",CI, " % state credible interval: ",
  #             dat$date[which(ymin > ul_state)[1]]))



  # now let's compute probability of exeeding the threshold by getting the percentile rank from the MCMC draws

  #threshold_prob <- 1-apply(state, 2, quantInv,ul_state)

  #plot(threshold_prob,type = 'l')



  #p_probs <- ggplot(NULL,aes(time,threshold_prob)) +
  #  geom_line() + theme_classic()




  p <- ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean))+
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax ),
                         alpha=0.5,fill ="blue") + ggplot2::xlab("Time") +
    ggplot2::ylab("Distribution") +
    ggplot2::geom_line(size=0.7) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept=ul_state, linetype="dashed", color = "red") +
    ggplot2::geom_hline(yintercept=ll_state, linetype="dashed", color = "red")


    p <- jaspGraphs::themeJasp(p)

    bstsControlPlotThreshold$plotObject <- p


  bstsControlPlots[["bstsControlPlotThreshold"]] <- bstsControlPlotThreshold
  return()
}

.bstsFillControlPlotProbability <- function(bstsControlPlots,bstsResults,options,ready){

  bstsControlPlotProbability <- createJaspPlot(title="Probability Plot", height = 320, width = 480)


  control = c(1:options$controlPeriod)
  L = options$controlSigma
  CI=.95 # add option in JASP later
  bstsControlPlotThreshold <- createJaspPlot(title="Threshold Plot")

  # extract model components
  state <- bstsResults$state.contribution
  # discard burn ins
  state <- state[-(1:options$burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)

  # determine 0.95% credible interval
  ymin <- apply(state,2,quantile,probs= ((1- CI)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- CI)/2))

  time <- options$time
  mean <-colMeans(state)

  # compute limits based on the estimated state
  state_mean <- mean(mean[control])
  state_sd <- sd(mean[control])


  ul_state <- state_mean + L* state_sd
  ll_state <- state_mean - L* state_sd

  # ymin is lower 95 cI bound of state
  # ul is the threshold that has to be exceeded according to 2 sigmas
  # thus ymin must be larger  than the ul threshold
  CI_reached <- ymin > ul_state # cases where 95% confident that above threshold


  #print(paste0("First date where 2*sigma is exeeded by ",CI, " % state credible interval: ",
  #             dat$date[which(ymin > ul_state)[1]]))



  # now let's compute probability of exeeding the threshold by getting the percentile rank from the MCMC draws

  threshold_prob <- 1-apply(state, 2, quantInv,ul_state)

  #plot(threshold_prob,type = 'l')



  p <- ggplot2::ggplot(NULL,ggplot2::aes(time,threshold_prob)) + ggplot2::xlab("Time") +
  ggplot2::ylab("Probability") + ggplot2::geom_line() + ggplot2::theme_classic()


  p <- jaspGraphs::themeJasp(p)

  bstsControlPlotProbability$plotObject <- p

  bstsControlPlots[["bstsControlPlotProbability"]] <- bstsControlPlotProbability
  return()
}
