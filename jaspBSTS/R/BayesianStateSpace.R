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

# Main function ----
bayesianStateSpace <- function(jaspResults, dataset, options) {
  # Set title

  # check if results can be computed
  ready <- (options$dependent != "")
  # Init options: add variables to options to be used in the remainder of the analysis
  #options <- .mockInitOptions(jaspResults, options)
  # read dataset



  dataset <- .bstsReadData(options,ready)
  # error checking
  .bstsErrorHandling(dataset, options)

  # Compute (a list of) results from which tables and plots can be created
  .bstsComputeResults(jaspResults, dataset, options,ready)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  # "Test"
  #.bstsSimplePlot(jaspResults,options)

  .bstsCreateContainerMain(jaspResults,options,ready)
  .bstsCreateModelSummaryTable(jaspResults,options,ready)
  .bstsCreateStatePlots(jaspResults,options,ready)

  #.mockContainerMain( jaspResults, options, mockResults)
  #.mockTableSomething(jaspResults, options, mockResults)
  #.mockTableSthElse(  jaspResults, options, mockResults)
  #.mockPlotSomething( jaspResults, options, mockResults)

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
    #numericVariables <- c(options$dependent,unlist(options$covariates))
  dataset <- .readDataSetToEnd(columns.as.numeric  = options$dependent)

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
            "distFam",
            "mcmcDraws",
            "checkboxAr","lagSelectionMethod","noLags","maxNoLags","arSdPrior","arSigmaGuess","arSigmaWeight",
            "checkboxLocalLevel",'localLevelSdPrior','localLevelSigmaGuess','localLevelSigmaWeight',
            "checkboxLocalLinearTrend",'lltLevelPrior','lltLevelSigmaGuess','lltLevelSigmaWeight','lltSlopePrior',
            'lltSlopeSigmaGuess','lltSlopeSigmaWeight'
          ))
}




# Results functions "----",

.bstsComputeResults <- function(jaspResults, dataset, options,ready) {
  if (!ready) return()

  if (is.null(jaspResults[["stateBstsResults"]])) {

    stateBstsResults <- createJaspState()

    jaspResults[["stateBstsResults"]] <- stateBstsResults
    jaspResults[["stateBstsResults"]]$dependOn(.bstsModelDependencies(options))
    #jaspResults[["stateBstsResults"]]$dependOn(c("dependent"))

    bstsResults <- .bstsResultsHelper(dataset,options)
    jaspResults[["stateBstsResults"]]$object <- bstsResults
  }

  return()
}

.bstsResultsHelper <- function(dataset,options) {

  y     <- dataset[[encodeColNames(options$dependent)]]
  if(!is.numeric(y))
    stop(gettext('lol'))
  data <- data.frame(y=y)
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
    ss <- bsts::AddLocalLinearTrend(ss,y=data$y)





  model <- bsts::bsts(formula = y,
                data=data$y,
                state.specification = ss,
                niter = 500,
                timestamps=NULL,
                )

  return(model)
}

# Helper function to calculate burn-in amount -> needed for results,plots,prediction
.bstsBurnHelper <- function(bstsResults,options) {

  if(options$burnSpecification == "burnSuggested")
    return(bsts::SuggestBurn(options$propBurnSuggested,bstsResults))

  if(options$burnSpecification == "burnManual")
    return(burn=options$numberBurnManual)
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

  bstsResults <- jaspResults[["stateBstsResults"]]$object

  bstsTable <- createJaspTable(title = gettext("Model Summary"))
  bstsTable$position <- 1

  bstsTable$addColumnInfo(name="resSd",title=gettext("Residual SD"),type= "number")
  bstsTable$addColumnInfo(name="predSd",title=gettext("Prediction SD"),type= "number")
  bstsTable$addColumnInfo(name="R2",title=gettext("R^2"),type= "number") # need proper R^2 notation
  bstsTable$addColumnInfo(name="relGof",title=gettext("Harvey's goodness of fit"),type= "number")


  .bstsModelSummaryTableFill(bstsTable,bstsResults,ready)

  jaspResults[["bstsMainContainer"]][["bstsModelSummaryTable"]] <- bstsTable

  return()
}


.bstsModelSummaryTableFill <- function(bstsTable,bstsResults,ready) {
  if(!ready) return()

  res <- summary(bstsResults)

  bstsTable$addRows(list(
    resSd   = res$residual.sd,
    predSd   = res$prediction.sd,
    R2      = res$rsquare,
    relGof  = res$relative.gof

  ))

}


.bstsCreateStatePlots <- function(jaspResults,options,ready) {


  if (!is.null(jaspResults[["bstsStatePlots"]])) return()


  bstsStatePlots <- createJaspContainer(title = gettext("State Plots"))


  #bstsStatePlots$dependOn(c("dependent","ciAggregatedStates"))

  bstsResults <- jaspResults[["stateBstsResults"]]$object

  if(options$checkboxPlotAggregatedStates) .bstsAggregatedStatePlot(bstsStatePlots,bstsResults,options,ready)


  jaspResults[["bstsMainContainer"]][["bstsStatePlots"]] <- bstsStatePlots
  return()
}

.bstsAggregatedStatePlot <- function(bstsStatePlots,bstsResults,options,ready) {
  if (!ready | !options$checkboxPlotAggregatedStates) return()

  burn <- .bstsBurnHelper(bstsResults,options)

  bstsAggregatedStatePlot <- createJaspPlot(title= gettext("Aggregated State"))
  bstsAggregatedStatePlot$dependOn(c("ciAggregatedStates"))

  #return()
  # get all states
  state <- bstsResults$state.contribution
  # discard burn ins
  state <- state[-(1:burn), , , drop = FALSE]
  # sum to final state
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)
  actualValues <- as.numeric(bstsResults$original.series)



  ymin <- apply(state,2,quantile,probs= ((1- options$ciAggregatedStates)/2))
  ymax <- apply(state,2,quantile,probs= 1-((1- options$ciAggregatedStates)/2))


  time <- 1:length(actualValues)
  mean <-colMeans(state)


  p <-  ggplot2::ggplot(NULL,ggplot2::aes(x=time,y=mean)) +
        ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=ymin,ymax =ymax),
                            fill ="blue",alpha=0.5) + ggplot2::xlab("Time") +
        ggplot2::ylab("Distribution") +
        ggplot2::geom_line(size=0.7) +
        ggplot2::theme_classic()


  if(options$actualValuesAggregatedStates)
    p <- p + ggplot2::geom_point(ggplot2::aes(y=actualValues))

  bstsAggregatedStatePlot$plotObject <- p

  bstsStatePlots[["bstsAggregatedStatePlot"]] <- bstsAggregatedStatePlot

  return()
}







.mockTableSomething <- function(jaspResults, options, mockResults) {
  if (!is.null(jaspResults[["mockMainContainer"]][["mockTable"]])) return()

  # Below is one way of creating a table
  mockTable <- createJaspTable(title = "Mock Table")
  mockTable$dependOnOptions(c("variables", "someotheroption")) # not strictly necessary because container

  # Bind table to jaspResults
  jaspResults[["mockMainContainer"]][["mockTable"]] <- mockTable

  # Add column info
  mockTable$addColumnInfo(name = "chisq",  title = "\u03a7\u00b2", type = "number", format = "sf:4")
  mockTable$addColumnInfo(name = "pvalue", title = "p",            type = "number", format = "dp:3;p:.001")
  mockTable$addColumnInfo(name = "BF",     title = "Bayes Factor", type = "number", format = "sf:4")
  mockTable$addColumnInfo(name = "sth",    title = "Some Title",   type = "string")

  # Add data per column
  mockTable[["chisq"]]  <- mockResults$column1
  mockTable[["pvalue"]] <- mockResults$column2
  mockTable[["BF"]]     <- mockResults$column3
  mockTable[["sth"]]    <- mockResults$sometext
}

.mockTableSthElse <- function(jaspResults, options, mockResults) {
  if (!is.null(jaspResults[["mockMainContainer"]][["mockTable2"]])) return()

  # Below is one way of creating a table
  mockTable2 <- createJaspTable(title = "Mock Table Something Else")
  mockTable2$dependOnOptions(c("variables", "someotheroption"))

  # Bind table to jaspResults
  jaspResults[["mockMainContainer"]][["mockTable2"]] <- mockTable2

  # Add column info
  mockTable2$addColumnInfo(name = "hallo", title = "Hallo", type = "string")
  mockTable2$addColumnInfo(name = "doei",  title = "Doei",  type = "string")

  # Calculate some data from results
  mockSummary <- summary(mockResults$someObject)

  # Add data per column. Calculations are allowed here too!
  mockTable2[["hallo"]] <- ifelse(mockSummary$hallo > 1, "Hallo!", "Hello!")
  mockTable2[["doei"]]  <- mockSummary$doei^2
}




#Plots for testing that don't serve a real function.
#


.bstsSimplePlot <- function(jaspResults,options) {
  if(!is.null(jaspResults[["bstsSimplePlot"]])) return()
  bstsSimplePlot <- createJaspPlot(title="Test")

  bstsSimplePlot$dependOn(c("checkboxPlotAggregatedStates","ciAggregatedStates",'propBurnSuggested'))




  jaspResults[["bstsSimplePlot"]] <- bstsSimplePlot

  .bstsFillSimplePlot(bstsSimplePlot,jaspResults,options)
}


.bstsFillSimplePlot <- function(bstsSimplePlot,jaspResults,options) {
  bstsResults <- jaspResults[["stateBstsResults"]]$object

  burn <- .bstsBurnHelper(bstsResults,options)
  p <- ggplot2::ggplot(NULL,ggplot2::aes(y=bstsResults$original.series,x=1:length(bstsResults$original.series))) +
  ggplot2::theme_classic() + ggplot2::geom_line() +
  ggplot2::xlab(paste(1- options$ciAggregatedStates)) #test for burn

  bstsSimplePlot$plotObject <- p

  return()
}
