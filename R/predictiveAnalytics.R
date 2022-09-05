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

predictiveAnalytics <- function(jaspResults, dataset, options) {
    ready <- options$dependent != ""

    dataset <- .predanReadData(options,ready)

    .predanContainerSetup(jaspResults, ready)
    .predanComputeResults(jaspResults, dataset, options, ready)
    .predanPlotsDescriptives(jaspResults,dataset,options,ready)

    .predanACFDescriptives(jaspResults,dataset,options,ready)
    .predanHistogramPlot(jaspResults,dataset,options,ready)
    .predanDiagnosticTables(jaspResults, dataset, options,ready)
    .predanComputeBinaryResults(jaspResults,dataset,options,ready)
    .predanBinaryControlChart(jaspResults,dataset,options,ready)

    #.predanComputeControlPrediction(jaspResults,dataset,options,ready)
    #.predanControlPredictionChart(jaspResults,dataset,options,ready)
    .predanModelSelectionFitHelper(jaspResults,dataset,options,ready)
    .predanSelectedModelPlots(jaspResults,dataset,options,ready)

    .predanForecastVerificationModelsHelper(jaspResults,dataset,options,ready)
    .predanForecastVerificationModelsTable(jaspResults,dataset,options,ready)

    .predanBMAHelperResults(jaspResults,dataset,options,ready)
    .predanBMAWeightsTable(jaspResults,dataset,options,ready)
    .predanBMAPlots(jaspResults,dataset,options,ready)
    .predanFuturePredictionHelper(jaspResults,dataset,options,ready)

    return()
}

.extractQuantiles <-function(state){
  data.frame(mean = colMeans(state,na.rm = T),

                                                lowerCI = apply(state,2,quantile,probs= 0.025,na.rm = T),
                                                higherCI= apply(state,2,quantile,probs= 0.975,na.rm = T),
                                                time = 1:ncol(state)
)
}
.predanReadData <- function(options,ready) {
  if(!ready) return()
  numericVariable <- c(options$dependent)
  numericVariable <- numericVariable[numericVariable != ""]
  #timeVar <- unlist(options$time)
  #timeVar <- timeVar[timeVar != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVariable)

  options$dataset <- dataset

  return(dataset)

}

.predanContainerSetup <- function(jaspResults, options) {

  if (is.null(jaspResults[["predanMainContainer"]])){
    predanMainContainer <- createJaspContainer()
    jaspResults[["predanMainContainer"]] <- predanMainContainer
    ##TODO: add depenencies
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]])){
    predanDescriptivesContainer <- createJaspContainer("Descriptives", position = 1)
    jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]] <- predanDescriptivesContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]])){
    predanDiagnosticsContainer <- createJaspContainer("Diagnostics", position = 2)
    jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]] <- predanDiagnosticsContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanForecastVerifContainer"]])){
    predanForecastVerifContainer <- createJaspContainer("Forecast Verification", position = 3)
    jaspResults[["predanMainContainer"]][["predanForecastVerifContainer"]] <- predanForecastVerifContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]])){
    predanModelSelectionContainer <- createJaspContainer("Model Selection", position = 4)
    jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]] <- predanModelSelectionContainer
  }

  if (is.null(jaspResults[["predanMainContainer"]][["predanBMAContainer"]])){
    predanBMAContainer <- createJaspContainer("Bayesian Model Averaging", position = 5)
    jaspResults[["predanMainContainer"]][["predanBMAContainer"]] <- predanBMAContainer
  }


  return()
}


###### Helper Functions
.extractState <- function(model,logit=F){
  state <- model$state.contributions
  burn <- bsts::SuggestBurn(0.1,model)
  state <- state[-(1:burn), , , drop = FALSE]
  state <- rowSums(aperm(state, c(1, 3, 2)), dims = 2)
  if(logit)
    state <- plogis(state)
  res <- data.frame(mean = colMeans(state),
                    lowerCI = apply(state,2,quantile,probs= 0.025),
                    higherCI= apply(state,2,quantile,probs= 0.975),
                    actualData = model$original.series,
                    time = 1:length(model$original.series))
  return(res)
}

# finds intersection between lines
.findCross <- function(time,y,limit){
  d <- y - limit
  signChanges <- (d[1:(length(d)-1)] * d[2:length(d)]) <= 0
  left <- c(signChanges, FALSE)
  right <- c(FALSE, signChanges)
  t <- (limit - y[left])/(y[right] - y[left])
  x2 <- (1 - t)*time[left] + t*time[right]

  if(length(x2 ) > 0)
    return(data.frame(y=limit,time  = x2, include=0,outBound=T))
  else
    return(data.frame(y=NULL, time=NULL,  include=NULL,outBound=NULL))
}



##### model selection helper functions


#.modelSelection










###### one step ahead predictions

# baseline model with running Variance and Mean

.baslinePredictionModel <- function(y,window,niter,runningMean=F,k=1){

  sdRunning <- c()

  baselineDraws <- matrix(NA,nrow = niter,ncol = length(y))
  for (i in 2:(length(y)-k)) {
    yTrain <- y[1:i]
    sdRunning[i] <- sd(tail(yTrain,n = window),na.rm = T)

    if(runningMean)
      yPrevious <- mean(tail(yTrain,n = window),na.rm = T)
    else
      yPrevious <- y[i]

    baselineDraws[,i+k] <- rnorm(niter,mean=yPrevious,sd = sdRunning[i] )
  }
  return(baselineDraws)
}

# bsts models

.bstsModel <- function(y_model,niter,mod="bsts-local"){

  if(sd(y_model) == 0)
    y_model <- y_model + rnorm(length(y_model),0,0.000001)
  ss <- list()
  if(mod=="bsts-local")
    ss <- bsts::AddLocalLevel(state.specification = ss,y=y_model)
  if(mod=="bsts-trend")
    ss <- bsts::AddLocalLinearTrend(state.specification = ss,y=y_model)
  if(mod=="bsts-ar")
    ss <- bsts::AddAr(ss,y_model)
  if(mod == "bsts-semi")
    ss <- bsts::AddSemilocalLinearTrend(ss,y_model)

  mod <- bsts::bsts(formula = y_model,ss,niter = niter,seed = 1,ping = 0)
  return(mod)
}

.switchModelNames <- function(modelName){
  switch (modelName,
          "bsts - autoregressive model" = "bsts-ar",
          "bsts - local level model" = "bsts-local",
          "bsts - linear trend model" = "bsts-trend",
          "bsts - semi local trend model" = "bsts-semi",
          "baseline - running variance" = "baselineVar",
          "baseline - running variance & mean" = "baselineVarMean")
}
.switchModelNamesReverse <- function(fullModelName){
  switch (fullModelName,
          "bsts-ar" = "bsts - autoregressive model",
          "bsts-local" = "bsts - local level model",
          "bsts-trend" = "bsts - linear trend model",
          "bsts-semi" = "bsts - semi local trend model",
          "baselineVar" = "baseline - running variance",
          "baselineVarMean" = "baseline - running variance & mean")
}

.createPredictions <- function(y, niter, parallel,model_function = .bstsModel,full_pred = T, mod = "bsts-local", model_window = 0  , k = 1) {
  if (parallel == FALSE) {
    one_step_matrix <- matrix(ncol = niter, nrow = length(y))

    for (i in 2:(length(y) - k)) {
      y_train <- y[1:i]
      if (model_window > 0) {
        y_train <- tail(y_train, model_window)
      }


      model <- .bstsModel(y_model = y_train, niter = niter, mod = mod)
      if (full_pred) {
        one_step_pred <- predict(model, horizon = 1, burn = 0, seed = 1)
      } else {
        one_step_pred <- predict(model, horizon = 1)
      }
      one_step_matrix[(i + 1), 1:niter] <- c(
        one_step_pred$distribution,
        rep(NA, niter - length(one_step_pred$distribution))
      )
    }
  } else if(Sys.info()[['sysname']] != "Windows"){ # par for linux based systems
    one_step_pred <- mcsapply(mc.cores = 7, X = 2:(length(y) - k), FUN = function(x) {
      y_train <- y[1:x]
      if (model_window > 0) {
        y_train <- tail(y_train, model_window)
      }


      model <- .bstsModel(y_model = y_train, niter = niter, mod = mod)
      # if(full_pred)
      one_step_pred <- predict(model, horizon = k, burn = 0, seed = 1)
      # else
      #  one_step_pred <- predict(model,horizon = 1)
      one_step_matrix <- one_step_pred$distribution[, k]
      return(one_step_matrix)
    })
    #print(one_step_pred)
    if(!is.matrix(one_step_pred))
      stop(gettextf("Models didn't work. Instead of prediction matrix we got: %s", one_step_pred))
    one_step_matrix <- cbind(matrix(NA, nrow = niter, ncol = k + 1), one_step_pred)
  } else {
    future::plan("future::multisession")
    one_step_matrix <- future.apply::future_sapply(future.seed = NULL,X= 2:(length(y) - k),FUN = function(x){
      y_train <- y[1:x]
      if(model_window > 0)
        y_train <- tail(y_train,model_window)
      model <- .bstsModel(y_model = y_train, niter = niter, mod = mod)
      # if(full_pred)
      one_step_pred <- predict(model, horizon = k, burn = 0, seed = 1)
      # else
      #  one_step_pred <- predict(model,horizon = 1)
      one_step_matrix <- one_step_pred$distribution[, k]
      return(one_step_matrix)
    })

    future::plan("future::sequential")
    one_step_matrix <- cbind(matrix(NA, nrow = niter, ncol = k + 1), one_step_matrix)
  }

  return(one_step_matrix)
}


.dssScore <- function(y,dat){
  m <- mean(dat,na.rm = T)
  v <- mean(dat^2,na.rm = T) - m^2

  return(sapply(y, function(s) (s - m)^2 / v + log(v)))
}

.brierScore <- function(pred,real,calc_mean = F){

  brier <- (real-pred)^2
  if(calc_mean)
    brier <- mean(brier,na.rm=T)
  return(brier)
}

.crpsScore <- function(y,dat){
  c_1n <- 1 / length(dat)
  x <- sort(dat)
  a <- seq.int(0.5 * c_1n, 1 - 0.5 * c_1n, length.out = length(dat))
  f <- function(s) 2 * c_1n * sum(((s < x) - a) * (x - s),na.rm = T)
  return(  sapply(y, f))
}


# https://stackoverflow.com/questions/31050556/parallel-version-of-sapply
mcsapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X = X, FUN = FUN, ...)
  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

# determines quantile rank of value against ecdf
quantInv <- function(distr, value){
  if(all(is.na(distr))) return(NA)
  if(sum(distr < value) == length(distr))
    return(1) # 1 if valuelarger than all MCMC draws
  else if (sum(distr > value) == length(distr))
    return(0) # 0 if value smaller than all MCMC draws
  else
    ecdf(distr)(value)
}

quantInvVec <- function(distrMatrix,value) apply(distrMatrix, 1, quantInv,value)

.computeOutOfBoundProbability <- function(modelList,upperBound,lowerBound,simplify = F){
  outBoundProbList <- lapply(modelList, function(x) {
    data.frame(upper= 1 - quantInvVec(t(x),upperBound),lower = quantInvVec(t(x),lowerBound))
  })
  dim(outBoundProbList)
  names(outBoundProbList) <- names(modelList)
  if(simplify)
    outBoundProbList <- do.call(cbind, outBoundProbList)
  return(outBoundProbList)
}

###esemble functions

.computeBMAHelper <- function(y, modelList, trainingPeriod,minimiseCRPS){
  predictionMeans <- as.data.frame(sapply(modelList,rowMeans))
  dates <- as.character(seq.Date(as.Date("2022-05-15"),by="days",length.out =  nrow(predictionMeans)))

  dates <- paste0(gsub("-","",dates),"00")
  ensembleDataFrame <- ensembleBMA::ensembleData(forecasts = predictionMeans,
                                                 observations = y,
                                                 dates = dates,
                                                 forecastHour = 24,initializationTime = 0)

  fitBMAObject <- ensembleBMA::ensembleBMAnormal(ensembleData = ensembleDataFrame,minCRPS = minimiseCRPS,
                                                 trainingDays  = trainingPeriod)

  return(list(fitBMAObject= fitBMAObject,ensembleDataFrame = ensembleDataFrame))

}





#### Future Prediction Helper Functions
.createFutureModelList <- function(y,niter,horizonFuture,modelWindow,modelsToCalculate){


  if(modelWindow >0)
    y <- tail(y,modelWindow)

  modelListFuture <- list()
  if(any(modelsToCalculate == "all"))
    modelsToCalculate <- c("bsts-ar","bsts-local","bsts-trend" ,"bsts-semi", "baselineVar","baselineMean")

  if("bsts-ar"     %in% modelsToCalculate)
    try(modelListFuture$"bsts-ar" <- predict(.bstsModel(y_model = y,niter = niter,mod = "bsts-ar"),horizon = horizonFuture,burn=100)$distribution)

  if("bsts-local"  %in% modelsToCalculate)
    try(modelListFuture$"bsts-local" <- predict(.bstsModel(y_model = y,niter = niter,mod = "bsts-local"),horizon = horizonFuture,burn=100)$distribution)

  if("bsts-trend"  %in% modelsToCalculate)
    try(modelListFuture$"bsts-trend" <- predict(.bstsModel(y_model = y,niter = niter,mod = "bsts-trend"),horizon = horizonFuture,burn=100)$distribution)

  if("bsts-semi"   %in% modelsToCalculate)
    try(modelListFuture$"bsts-semi" <- predict(.bstsModel(y_model = y,niter = niter,mod = "bsts-semi" ),horizon = horizonFuture,burn=100)$distribution)

  if("baselineVar" %in% modelsToCalculate)
    modelListFuture$"baselineVar" <- matrix(.baslinePredictionModel(y, niter = niter-100, window  = length(y), k = 1,runningMean = F)[length(y),],
                                            ncol = horizonFuture,
                                            nrow = niter-100)
  if("baselineRunVarMean" %in% modelsToCalculate)
    modelListFuture$"baselineVarMean" <- matrix(.baslinePredictionModel(y, niter = niter-100, window  = length(y), k = 1,runningMean = T)[length(y),],
                                                ncol = horizonFuture,
                                                nrow = niter-100)

  return(modelListFuture)
}

### helper function that applies BMA weights to predictions
.bmaFuturePrediction <- function(fitBMAObject, modelListFuturePredictions, weightWindow,limits) {
  modelWeights <- colMeans(tail(t(fitBMAObject$weights), weightWindow))
  aWeights <- colMeans(tail(t(fitBMAObject$biasCoefs[1, , ]), weightWindow))
  bWeights <- colMeans(tail(t(fitBMAObject$biasCoefs[2, , ]), weightWindow))

  biasCorrectedPredictionList <- lapply(X = 1:length(modelListFuturePredictions), FUN = function(x) aWeights[x] + bWeights[x] * modelListFuturePredictions[[x]])
  names(biasCorrectedPredictionList) <- names(modelListFuturePredictions)
  weightedPredictions <- lapply(X = 1:length(modelListFuturePredictions), FUN = function(x) biasCorrectedPredictionList[[x]] * modelWeights[x])
  # add BMA model
  biasCorrectedPredictionList$BMA <- Reduce("+", weightedPredictions)
  sumPred2 <- lapply(biasCorrectedPredictionList, .extractQuantiles)
  sumPredLimitsUpper <- lapply(biasCorrectedPredictionList,function(x) 1-quantInvVec(t(x),limits[1]))
  sumPredLimitsLower <- lapply(biasCorrectedPredictionList,function(x) quantInvVec(t(x),limits[2]))
  sumPred2 <- lapply(X = 1:length(sumPred2),function(x) cbind(sumPred2[[x]],upperProb = sumPredLimitsUpper[[x]]))
  sumPred2 <- lapply(X = 1:length(sumPred2),function(x) cbind(sumPred2[[x]],lowerProb = sumPredLimitsLower[[x]]))

  sumPred2 <- lapply(X = 1:length(sumPred2), function(x) cbind(sumPred2[[x]], model = names(biasCorrectedPredictionList)[x]))

  sumPred2 <- do.call("rbind", sumPred2)
  return(sumPred2)
}




##### dependencies
.boundDependencies <- function(){
  return(c("dependent","errorBoundMethodDrop",
           "manualBoundMethod",
           "manualBoundMean",
           "manualBoundErrorBound",
           "manualUpperBound",
           "manualUpperLower",
           "sigmaBound",
           "controlPeriodCheck",
           "controlPeriodStart",
           "controlPeriodEnd",
           "trimmedMeanCheck",
           "trimmedMeanPercent"))
}



.forecastVeriDependencies <- function(){
  return(c("forecastModelBstsSemiLocalCheck",
             "forecastVerificationModelWindow",
             "forecastVerificationDraws",
             "modelsToPlot"))
}



.forecastMetricDependencies <- function(){
  return(c("forecastMetricsCRPS",
           "forecastMetricsDSS",
           "forecastMetricsAUC",
           "forecastMetricsPR",
           "forecastMetricsBrier"))
}





.predanComputeResults <- function(jaspResults, dataset, options,ready) {

  if(!ready) return()

  if (is.null(jaspResults[["predanResults"]])) {
    predanResults <- createJaspContainer()

    jaspResults[["predanResults"]] <- predanResults
    #jaspResults[["predanResults"]]$dependsOn(c("dependent","time"))

  }
  if (is.null(jaspResults[["predanResults"]][["predanBounds"]])){
    predanBoundsState <- createJaspState()

    #TODO: insert depend on all boundary setting
    predanBounds <- .predanComputeBounds(dataset,options)
    predanBoundsState$object <- predanBounds
    predanBoundsState$dependOn(.boundDependencies())
    jaspResults[["predanResults"]][["predanBounds"]] <- predanBoundsState
  }

}

.predanComputeBounds <- function(dataset,options) {

  dataControl <- data.frame(y = dataset[,options[["dependent"]]])
  dataControl$time <- 1:nrow(dataControl)

  if(options$errorBoundMethodDrop == "manualBound" & options$manualBoundMethod == "manualBoundUniform") {

    upperLimit <- options$manualBoundMean + options$manualBoundErrorBound
    lowerLimit <- options$manualBoundMean - options$manualBoundErrorBound
    plotLimit <- options$manualBoundMean + c(2*options$manualBoundErrorBound,-2*options$manualBoundErrorBound)
  } else if(options$errorBoundMethodDrop == "manualBound" && options$manualBoundMethod == "manualBoundCustom"){

    upperLimit <- options$manualUpperBound
    lowerLimit <- options$manualLowerBound
    boundMean <- (upperLimit + lowerLimit)/2
    plotLimit <- boundMean + c(1,-1)*2*(upperLimit-boundMean)

  } else {

    if (options$controlPeriodCheck & options$controlPeriodEnd > 0){
      controlPeriod <- seq(options$controlPeriodStart,options$controlPeriodEnd,1)
    } else
      controlPeriod <- seq_len(nrow(dataControl))

    trimMean <- ifelse(options$trimmedMeanCheck,options$trimmedMeanPercent,0)
    upperLimit <- mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) + sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound
    lowerLimit <- mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) - sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound
    plotLimit <- c(mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) + 2*sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound,
                   mean(dataControl$y[controlPeriod],trim=trimMean,na.rm=T) - 2*sd(dataControl$y[controlPeriod],na.rm=T)*options$sigmaBound)
  }



  dataControl$outBound <- ifelse(dataControl$y > upperLimit | dataControl$y < lowerLimit,T,F)
  dataControl$outBoundNum <- as.numeric(dataControl$outBound)
  dataControl$outBoundArea[!is.na(dataControl$y)] <- "Inside"
  dataControl$outBoundArea[!is.na(dataControl$outBound)] <- ifelse(dataControl$y[!is.na(dataControl$outBound)] > upperLimit,"Above",ifelse(dataControl$y[!is.na(dataControl$outBound)] < lowerLimit,"Below","Inside"))

  dataControl$distance[!is.na(dataControl$outBound)] <- ifelse(dataControl$y[!is.na(dataControl$outBound)] > upperLimit,
                                                       dataControl$y[!is.na(dataControl$outBound)] - upperLimit,
                                                       dataControl$y[!is.na(dataControl$outBound)] - lowerLimit)


  results <- list(dataControl = dataControl,
                  upperLimit = upperLimit,
                  lowerLimit = lowerLimit,
                  plotLimit = plotLimit)

  return(results)
}

.predanPlotsDescriptives <- function(jaspResults,dataset,options,ready) {
  if(!ready) return()

  predanDescriptivesContainer <- jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]]

  #predanDescriptivePlots <- createJaspContainer(title=gettext("Descriptives"),position =1)

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]][["object"]]



  if(options$controlPlotCheck)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivesContainer,options,zoom=F)

  if(options$controlPlotZoomCheck && options$zoomPeriodEnd >0)
    .predanBasicControlPlot(jaspResults,predanResults,predanDescriptivesContainer,options,zoom=T)


  #jaspResults[["predanMainContainer"]][["predanDescriptivePlots"]] <- predanDescriptivePlots
  return()
}



.predanBasicControlPlot <- function(jaspResults,predanResults,predanDescriptivesContainer,options,zoom){

  controlData <- predanResults[["dataControl"]]

  upperLimit <- predanResults[["upperLimit"]]
  lowerLimit <- predanResults[["lowerLimit"]]
  plotLimit <- predanResults[["plotLimit"]]

  title <- "Basic Control Plot"
  if(zoom){
    start <- options$zoomPeriodStart
    end <- options$zoomPeriodEnd
    if(end > nrow(controlData))
      end <- nrow(controlData)

    controlData <- controlData[start:end,]
    title <- "Basic Control Plot - Focused"
  }


  plotData <- controlData[1:3]
  plotData$include = 1

  plotData <- rbind(plotData,
                    .findCross(plotData$time,plotData$y,upperLimit),
                    .findCross(plotData$time,plotData$y,lowerLimit))


  plotData <- plotData[order(plotData$time),]

  plotData$includeLine <- NA
  plotData$includeLine[1] <- plotData$outBound[1]
  for (i in 2:nrow(plotData)) {
    plotData$includeLine[i] <- ifelse((plotData$include[i] == 0 & plotData$include[i+1] &plotData$outBound[i+1]==T)|
                                        plotData$outBound[i]&plotData$outBound[i+1]==T,T,F)

  }

  xBreaks <- pretty(plotData$time)
  yBreaks <- pretty(plotData$y)



    predanControlPlot <- createJaspPlot(title= title, height = 480, width = 720)
    p <-ggplot2::ggplot(plotData[plotData$include==1,],ggplot2::aes(time,y,group=1,colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit))) +
      ggplot2::geom_hline(yintercept = upperLimit,linetype="dashed",color="darkred") +
      ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred") +
      ggplot2::scale_color_manual(guide="none",values=c("#4E84C4","#D16103"))


  if(options$controlLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=1,data=plotData,ggplot2::aes(time,y,colour=ggplot2::after_stat(plotData$includeLine)))
  if(options$controlLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=2)

  if(options$xAxisLimit == "controlBounds")
    p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                             plotLimit[[1]]))

  p <- p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::scale_y_continuous(name = "y",breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = "Time",breaks = xBreaks,limits = range(xBreaks))

  predanControlPlot$plotObject <- p

  # Report whether warning is important or not based on whether at least 10 observations are out of bound
  predanDescriptivesContainer[["controlPlotReport"]] <- createJaspReport(
    text     = gettextf("%f Observations are out of control", sum(controlData$outBound)),
    report   = sum(controlData$outBound) > 10,
    position = 0
  )

  if(!zoom)
    predanDescriptivesContainer[["predanControlPlot"]] <- predanControlPlot
  else
    predanDescriptivesContainer[["predanControlPlotZoom"]] <- predanControlPlot
  return()
}



.predanACFDescriptives <- function(jaspResults,dataset,options,ready){
  if((!options$acfPlotCheck || !ready) || !is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["acfPlots"]])) return()

  acfPlots <- createJaspContainer(title= gettext("Autocorrelation Function Plots"))
  # TODO add dependencies
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  .predanACFPlots(jaspResults,dataControl,options,acfPlots)

  jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["acfPlots"]] <- acfPlots

  return()
}

.predanACFPlots <- function(jaspResults,dataControl,options,acfPlots){
  y <- na.omit(dataControl$y)

  ac <- acf(y, plot = F, lag.max = options$acfLagsMax)
  acP <- pacf(y, plot = F, lag.max = options$acfLagsMax)
  xBreaks <- jaspGraphs::getPrettyAxisBreaks(ac$lag)
  yRange <- ac$acf
  ci <- T
  ciValue = 0.95
  p <- ggplot2::ggplot()
  if (ci) {
    clim      <- qnorm((1 + ciValue) / 2) / sqrt(ac$n.used)
    dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks), y = c(clim, -clim))
    yRange    <- c(yRange, clim, -clim)

    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = "dashed", color = "blue", data = dfSegment)
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(yRange)

  dat <- data.frame(acf = ac$acf, lag = ac$lag, pacf = c(acP$acf,NA))


  acfPlot <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf), size = 1) +
    ggplot2::labs(x = "Lag", y = "ACF") +
    ggplot2::geom_hline(yintercept = 0) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()
  pacfPlot <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = pacf), size = 1) +
    ggplot2::labs(x = "Lag", y = "Partial ACF") +
    ggplot2::geom_hline(yintercept = 0) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  predanACFPlot <- createJaspPlot(title= "Autocorrelation Function", height = 340, width = 480)

  predanPACFPlot <- createJaspPlot(title= "Partial Autocorrelation Function", height = 340, width = 480)

  predanACFPlot$plotObject <- acfPlot
  predanPACFPlot$plotObject <- pacfPlot

  acfPlots[["predanACFPlot"]] <- predanACFPlot
  acfPlots[["predanPACFPlot"]] <- predanPACFPlot
  return()
}


.predanHistogramPlot <- function(jaspResults,dataset,options,ready){
  if((!ready || !options$outlierHistogramCheck)|| !is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["histogramPlot"]]) ) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["histogramPlot"]] ))
    histogramPlot <- createJaspPlot(title= gettext("Histogram Plot"), height = 360, width = 450)

  histogramPlot$dependOn(c(.boundDependencies(),"outlierHistogramCheck","outlierHistogramDensity"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  .predanHistogramPlotFill(jaspResults,dataControl,options,histogramPlot)

  jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["histogramPlot"]] <- histogramPlot

  return()


}


.predanHistogramPlotFill <- function(jaspResults,dataControl,options,histogramPlot){

  sigma.g1 <- sqrt((6*(length(dataControl$y) - 2)) / ((length(dataControl$y) + 1)*(length(dataControl$y) + 3)))
  g1 <- mean(abs(dataControl$y)^3)
  k <- 1 + log2(length(dataControl$y)) + log2(1 + (g1 / sigma.g1))
  binWidthType <- k

  h <- hist(dataControl$y, plot = F, breaks = binWidthType)
  xticks <- base::pretty(c(dataControl$y, h$breaks), min.n = 3)

  if (options$outlierHistogramDensity)
    yhigh <- max(h$counts)

  p <- ggplot2::ggplot(dataControl,
                       ggplot2::aes(x= y,
                  binwidth = 0.5))

  if(!options$outlierHistogramDensity){
    p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(
      y= ..count.. ,
      fill = outBound),
      colour="black",
      binwidth = binWidthType,
      breaks = h[["breaks"]],
      position = "stack") +
      ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF"))  +
      ggplot2::guides(color = "none") +
      ggplot2::scale_y_continuous(name = "Count",breaks = base::pretty(c(0, h$counts))) +
      ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103"))

  } else {
    dftmp <- data.frame(x = range(xticks), y = range( c(0,  max(h$density))))
    p <- p + ggplot2::geom_line(data = dftmp,
                                mapping = ggplot2::aes(x = .data$x,
                                                       y = .data$y), color = "white", alpha = 0)
    p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(
      y= ..count.. / (sum(..count..) * binwidth) ,
      fill = outBound),
      colour= "black",
      size     = .7,
      binwidth = binWidthType,
      breaks = h[["breaks"]],
      position = "stack") +
      ggplot2::geom_density() +
      ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF"))  +
      ggplot2::scale_x_continuous( breaks = xticks,limits = c(xticks[1],max(xticks))) +
      ggplot2::guides(color = "none") +
      ggplot2::scale_y_continuous(name = "Density") +
      ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103")) +
      ggplot2::guides(color = FALSE)
  }

  p <- jaspGraphs::themeJasp(p)

  if(options$outlierHistogramDensity){
    p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank())
  }

  histogramPlot$plotObject <- p

  return()
}

.predanDiagnosticTables <- function(jaspResults,dataset,options,ready){
  if(!ready) return()

  diagnosticTables <- createJaspContainer(title = "Diagnostic Tables")

  diagnosticTables$dependOn(c("summaryStatsTableCheck","outlierTableCheck"))
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  dataControl <- predanResults[[1]]

  if(options$summaryStatsTableCheck)
    .predanSummaryTable(dataControl, options, diagnosticTables)

  if(options$outlierTableCheck)
    .predanOutlierTable(dataControl,options, diagnosticTables)

  jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["diagnosticTables"]] <- diagnosticTables

  return()

}


.predanSummaryTable <- function(dataControl, options, diagnosticTables){

  summaryTable <- createJaspTable("Control Summary Table")


  summaryTable$dependOn("summaryStatsTableCheck")
  summaryTable$transpose<- TRUE
  summaryTable$addColumnInfo(name="variable",  title="Control Area", type="string")
  #summaryTable$addColumnInfo(name="Level",     title="", type="string")

  summaryTable$addColumnInfo(name="mean",     title=gettext("Mean"), type="number")
  summaryTable$addColumnInfo(name="sd",       title=gettext("SD"),   type="number")
  summaryTable$addColumnInfo(name="min",      title=gettext("Minimum"),   type="number")
  summaryTable$addColumnInfo(name="max",      title=gettext("Maximum"),   type="number")
  summaryTable$addColumnInfo(name="valid",    title=gettext("Valid"),   type="integer")
  #summaryTable$addColumnInfo(name="missing",  title=gettext("Missing"),   type="integer")
  summaryTable$addColumnInfo(name="percent",  title=gettext("Percent"),   type="number", format= "dp:1;pc")
  summaryTable$addColumnInfo(name="deviation",title=gettext("Average Deviation"),   type="number")

  tableRes <- do.call(data.frame,
                      aggregate( y ~ outBoundArea,
                                 data = dataControl,
                                 FUN = function(x) c(mean = mean(x),
                                                     sd = sd(x),
                                                     min = min(x),
                                                     max = max(x),
                                                     valid = sum(!is.na(x)),
                                                     missing = sum(is.na(x)))
                                 ))


  tableResAll <- do.call(data.frame,
                      aggregate( y ~ 1,
                                 data = dataControl,
                                 FUN = function(x) c(mean = mean(x),
                                                     sd = sd(x),
                                                     min = min(x),
                                                     max = max(x),
                                                     valid = sum(!is.na(x)),
                                                     missing = sum(is.na(x)))
                      ))

  tableResAll$outBoundArea = "All"

  tableRes <- rbind(tableRes,tableResAll)
  tableRes$percent = tableRes[,6]/nrow(dataControl)

  tableDeviation <- do.call(data.frame,
                            aggregate( distance~ outBoundArea,
                                       data = dataControl,
                                       FUN = function(x) c(mean = mean(x)
                                       ) ))


  tableRes <- merge(tableRes,tableDeviation,all.x = T)


  for(i in 1:nrow(tableRes)){
    row <- list(
      variable = tableRes$outBoundArea[i],
      mean = tableRes[i,2],
      sd = tableRes[i,3],
      min = tableRes[i,4],
      max = tableRes[i,5],
      valid = tableRes[i,6],
      #missing = tableRes[i,7],
      percent = tableRes[i,8],
      deviation = tableRes[i,9]
    )
    summaryTable$addRows(row)
  }


  diagnosticTables[["summaryTable"]] <- summaryTable

  return()
}



.predanOutlierTable <- function(dataControl,options, diagnosticTables){


  if(options$outlierTableFocusCheck & options$outLierTableEnd >1){
    start <- options$outLierTableStart
    end <- options$outLierTableEnd
    if(end > nrow(dataControl))
      end <- nrow(dataControl)

    dataControl <- dataControl[start:end,]
    title <- "Control Summary Table - Focused"
  } else
    title <- "Control Summary Table"

  outlierTable <- createJaspTable(title = title)

  outlierTable$dependOn("outlierTableTransposeCheck")

  if(options$outlierTableTransposeCheck)
    outlierTable$transpose <- TRUE


  outlierTable$addColumnInfo(name= "time",      title = "Time point",             type = "integer")

  outlierTable$addColumnInfo(name="variable",   title = "Control area",           type = "string")

  outlierTable$addColumnInfo(name= "value",     title = "Value",                  type = "number", format= "dp:2")

  outlierTable$addColumnInfo(name= "deviation", title = "Deviation",   type = "integer", format= "dp:2")

  for(i in dataControl$time[dataControl$outBound]){
    row <- list(
      time = dataControl$time[i],
      variable = dataControl$outBoundArea[i],
      value = dataControl$y[i],
      deviation = dataControl$distance[i])

    outlierTable$addRows(row)
  }


  diagnosticTables[["outlierTable"]] <- outlierTable

  return()

}









.predanComputeBinaryResults <- function(jaspResults,dataset,options,ready){
  if(!ready | !options$binaryControlChartCheck) return()

  if (is.null(jaspResults[["predanResults"]][["predanBinaryBounds"]])){
    predanBinaryBoundsState <- createJaspState()

    predanBinaryBoundsState$dependOn(c(.boundDependencies(),"binaryControlChartCheck"))

    predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
    controlData <- predanResults[[1]]

    #TODO: insert depend on all boundary setting
    #if(options$binaryControlMethod=="state space")
      predanBinaryBounds <- .predanBinaryStateSpaceResults(jaspResults,controlData,dataset,options)
    #else if (options$binaryControlMethod == "beta distribution")

    predanBinaryBoundsState$object <- predanBinaryBounds


    jaspResults[["predanResults"]][["predanBinaryBounds"]] <- predanBinaryBoundsState
  }
  return()
}


.predanBinaryStateSpaceResults <- function(jaspResults,controlData,dataset,options){

  y <- controlData$outBoundNum

  ss <- list()
  ss <- bsts::AddLocalLevel(ss,y,sigma.prior = Boom::SdPrior(sigma.guess = .1,
                                                       sample.size = 1,
                                                       upper.limit = 1),
                            initial.state.prior = Boom::NormalPrior(0, 5))

  ts.model <- bsts::bsts(y , ss, niter = options$binaryStateSpaceNiter,
                   family = "logit")

  return(ts.model)

}








.predanBinaryControlChart <- function(jaspResults,dataset,options,ready) {
  if(!ready | !options$binaryControlChartCheck) return()

  predanBinaryControlPlots <- createJaspContainer(title=gettext("Binary Control Plots"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object

  predanBinaryControlPlots$dependOn(c(.boundDependencies(),"binaryControlChartCheck"))

  #if(options$binaryControlMethod == "state space"){
  tsModel <- jaspResults[["predanResults"]][["predanBinaryBounds"]]$object

  .predanBinaryControlStateSpacePlot(jaspResults,predanBinaryControlPlots,tsModel,predanResults,dataset,options)
  #}


  jaspResults[["predanMainContainer"]][["predanBinaryControlPlots"]] <- predanBinaryControlPlots
  return()
}
#
.predanBinaryControlStateSpacePlot <- function(jaspResults,predanBinaryControlPlots,tsModel,predanResults,dataset,options) {

  predanBinaryControlStateSpacePlot <- createJaspPlot(title= "Binary state space plot", height = 320, width = 480)

  results <- .extractState(tsModel,T)

  xBreaks <- pretty(results$time)

  p <- ggplot2::ggplot(results,ggplot2::aes(x=time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=lowerCI,ymax =higherCI),
                         fill ="blue",alpha=0.5) + ggplot2::xlab("Time") +
    ggplot2::ylab("Distribution") +
    ggplot2::geom_line(size=0.7) +
    ggplot2::geom_point(ggplot2::aes(y=actualData),size=0.5) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = range(xBreaks)) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))



  if(options$binaryControlOutPropLimit > 0) {

    # importance decided by whether out-bound proportion boundary ever was reached
    predanBinaryControlPlots[["binaryControlPlotReport"]] <- createJaspReport(
      text     = gettextf("Out of control proportion has reached maximum of %f%%", round(max(results$mean),2) ),
      report   = any(results$mean > options$binaryControlOutPropLimit),
      position = 0
    )

    p <- p + ggplot2::geom_hline(yintercept = options$binaryControlOutPropLimit,linetype="dashed",color="darkred")

  }

  predanBinaryControlStateSpacePlot$plotObject <- p

  predanBinaryControlPlots[["predanBinaryControlStateSpacePlot"]] <- predanBinaryControlStateSpacePlot
  return()
}



.predanComputeControlPrediction <- function(jaspResults,dataset,options,ready){
  if(!ready || options$controlPredictionEnd==0 || !options$controlPredictionCheck) return()

  predanControlPredictions <- createJaspState()

  predanControlPredictions$dependOn(c("predanControlPredictions","controlPredictionCheck","controlPredictionHorizon"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  controlData <- predanResults[[1]]


  controlPredictionResults <- .predanControlPredictionResultsHelper(jaspResults,controlData,dataset,options)

  predanControlPredictions$object <- controlPredictionResults


  jaspResults[["predanResults"]][["predanControlPredictions"]] <- predanControlPredictions

  return()

}


.predanControlPredictionResultsHelper <- function(jaspResults,controlData,dataset,options){

  oneStepPredMatrix <- .createPredictions(y = controlData$y[options$controlPredictionStart:options$controlPredictionEnd],
                                         niter = options$predDraws,
                                         full_pred = T,
                                         model_function = .bstsModel,
                                         mod = "trend")

  fullModel <- .bstsModel(controlData$y[options$controlPredictionStart:options$controlPredictionEnd],
                          niter = 1000,mod = "trend")

  dataMatrix <- data.frame(mean = rowMeans(oneStepPredMatrix,na.rm=T),
                           lowerCI = apply(oneStepPredMatrix,1,quantile,probs= 0.025,na.rm=T),
                           higherCI= apply(oneStepPredMatrix,1,quantile,probs= 0.975,na.rm=T),
                           actualData = fullModel$original.series,
                           time = 1:length(fullModel$original.series),
                           type = "real")


  predictionsControl <- predict(fullModel,horizon = options$controlPredictionHorizon)

  predActual <- rep(NA,options$controlPredictionHorizon)
  if(length(controlData$y)> (options$controlPredictionEnd+options$controlPredictionHorizon))
    predActual <- controlData$y[(options$controlPredictionEnd+1):(options$controlPredictionEnd+options$controlPredictionHorizon)]

  predictionMatrix <- data.frame(mean = predictionsControl$mean,
                                 lowerCI = predictionsControl$interval[1,1:options$controlPredictionHorizon],
                                 higherCI = predictionsControl$interval[2,1:options$controlPredictionHorizon],
                                 actualData = predActual,
                                 time = (options$controlPredictionEnd+1):(options$controlPredictionEnd+options$controlPredictionHorizon),
                                 type = "prediction")


  combinedPredictions <- rbind(dataMatrix,predictionMatrix)
  return(combinedPredictions)


}

.predanControlPredictionChart <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["predanControlPredictions"]])) return()
  controlPredictionPlot <- createJaspPlot(title=gettext( "Control Predictions"))

  controlPredictionPlot$dependOn(c("predanControlPredictions","controlPredictionCheck","controlPredictionHorizon","controlPredictionFocus"))

  predanResults <- jaspResults[["predanResults"]]

  combinedPredictions <- jaspResults[["predanResults"]][["predanControlPredictions"]]$object


  controlData <- predanResults[["dataControl"]]

  upperLimit <- predanResults[["upperLimit"]]
  lowerLimit <- predanResults[["lowerLimit"]]
  plotLimit <- predanResults[["plotLimit"]]

  controlPredictionPlot <-  createJaspPlot(title= "Control Prediction", height = 320, width = 480)

  xBreaks <- pretty(combinedPredictions$time)
  yBreaks <- pretty(combinedPredictions$mean)

  p <- ggplot2::ggplot(combinedPredictions,ggplot2::aes(x = time,y=mean)) +
    ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=lowerCI,ymax =higherCI ),
                         alpha=0.5,fill ="blue") + ggplot2::xlab("Time") +
    ggplot2::ylab("Distribution") +
    ggplot2::geom_line(size=0.7) +
    ggplot2::geom_point(ggplot2::aes(time,actualData),size=0.7) +
    ggplot2::geom_vline(xintercept = options$controlPredictionEnd,linetype="dashed") +
    ggplot2::scale_y_continuous(breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(breaks = xBreaks,limits = range(xBreaks))

  p <- p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))




  p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                    plotLimit[[1]]))

  p <- p + ggplot2::geom_hline(yintercept = upperLimit,linetype="dashed",color="darkred") +
    ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred")


  if (options$controlPredictionFocus && length(controlData$y) <= (options$controlPredictionEnd +options$controlPredictionHorizon))
    p <- p + ggplot2::coord_cartesian(xlim=c(1,length(dataControl$y)))


  controlPredictionPlot$plotObject <- p

  jaspResults[["predanMainContainer"]][["controlPredictionPlot"]] <- controlPredictionPlot
  return()
}



.predanModelSelectionFitHelper <- function(jaspResults,dataset,options,ready){
  if(!ready || length(options[["selectedModels"]]) < 1 ) return()

  predanModelSelectionFit <- createJaspState()
  predanModelSelectionFit$dependOn(c("forecastModelBstsLocalLevelCheck",
                                  "forecastModelBstsLinearTrendCheck",
                                  "forecastModelBstsArCheck",
                                  "forecastModelBstsSemiLocalCheck",
                                  "selectedModels"))

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  controlData <- predanResults[[1]]

  y <- controlData$y


  niter = 500
  modelWindow = 500

  if(modelWindow >0)
    y <- tail(y,modelWindow)

  modelFitList <- list()

  modelsToCalculate <- unlist(options[["selectedModels"]])

  if("bsts - autoregressive model"     %in% modelsToCalculate)
    try(modelFitList$"bsts-ar" <- .bstsModel(y_model = y,niter = niter,mod = "bsts-ar"))

  if("bsts - local level model"  %in% modelsToCalculate)
    try(modelFitList$"bsts-local" <- .bstsModel(y_model = y,niter = niter,mod = "bsts-local"))

  if("bsts - linear trend model"  %in% modelsToCalculate)
    try(modelFitList$"bsts-trend" <- .bstsModel(y_model = y,niter = niter,mod = "bsts-trend"))

  if("bsts - semi local trend model"   %in% modelsToCalculate)
    try(modelFitList$"bsts-semi" <- .bstsModel(y_model = y,niter = niter,mod = "bsts-semi"))

  if("baseline - running variance" %in% modelsToCalculate)
    try(modelFitList$baselineVar <- .baslinePredictionModel(y=y,window = 30,niter = niter,k=1))

  if("baseline - running variance & mean" %in% modelsToCalculate)
    try(modelFitList$baselineVarMean <- .baslinePredictionModel(y = y, window = 30,runningMean = T, niter = niter,k=1))

  # TODO: add summary statistics for baseline models

  summaryModelIndex <- !names(modelFitList) %in% c("baselineVar","baselineVarMean")
  modelFitSummary <- lapply(modelFitList[summaryModelIndex],summary)
  modelFitSummary <- as.data.frame(do.call(rbind,modelFitSummary))

  modelFitSummaryTable <- createJaspTable("Model Fit Summary Table")
  modelFitSummaryTable$addColumnInfo(name= "Model", title = "", type = "string")

  modelFitSummaryTable$addColumnInfo(name= "residual.sd", title = "Residual SD", type = "number")
  modelFitSummaryTable$addColumnInfo(name= "prediction.sd", title = "Prediction SD", type = "number")
  modelFitSummaryTable$addColumnInfo(name= "rsquare", title = gettextf("R%s", "\u00B2"), type = "number")
  modelFitSummaryTable$addColumnInfo(name= "relative.gof",  title=gettext("Harvey's goodness of fit"),  type= "number")

  modelFitSummaryTable[["Model"]] <- rownames(modelFitSummary)
  modelFitSummaryTable[["residual.sd"]] <- modelFitSummary$residual.sd
  modelFitSummaryTable[["prediction.sd"]] <- modelFitSummary$prediction.sd
  modelFitSummaryTable[["rsquare"]] <- modelFitSummary$rsquare
  modelFitSummaryTable[["relative.gof"]] <- modelFitSummary$relative.gof


  predanModelSelectionFit$object <- modelFitList

  jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]][["modelFitTable"]] <- modelFitSummaryTable

  jaspResults[["predanResults"]][["modelFitState"]] <- predanModelSelectionFit
  # add plotable models to qml list
  modelNamesFull <- sapply(names(modelFitList),.switchModelNamesReverse)
  jaspResults[["plottableModelsQml"]] <- createJaspQmlSource(sourceID="plottableModelsQml", value=modelNamesFull)

  return()
}





.predanSelectedModelPlots <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["modelFitState"]])) return()

  selectedModelPlots <- createJaspContainer("Selected Model Plots")
  selectedModelPlots$dependOn("modelsToPlot")
  modelFitState <- jaspResults[["predanResults"]][["modelFitState"]]$object
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object

  upperLimit <- predanResults[[2]]
  lowerLimit <-  predanResults[[3]]

  #limits <- c(upperLimit,lowerLimit)
  plotLimits <- predanResults[[4]]

  plotLimits <- predanResults[[4]]
  selectedModelsPlot <- unlist(options[["modelsToPlot"]])

  selectedModelsLoop <- sapply(selectedModelsPlot, .switchModelNames)

  for (modelName in selectedModelsLoop){
    singleModel <- modelFitState[[modelName]]
    modelPlot <- createJaspPlot(title=modelName)
    if(grepl("bsts",modelName))
      singleModel <- .extractState(singleModel)
    else next

    #View(singleModel)
    xBreaks <- pretty(singleModel$time)
    yBreaks <- pretty(unlist(singleModel[,c("lowerCI","higherCI")]))

    p <- ggplot2::ggplot(singleModel,ggplot2::aes(x=time,y=mean)) +
      ggplot2::geom_ribbon(mapping=ggplot2::aes(ymin=lowerCI,ymax =higherCI),
                           fill ="blue",alpha=0.5) + ggplot2::xlab("Time") +
      ggplot2::ylab("Distribution") +
      ggplot2::geom_line(size=0.7) +
      ggplot2::geom_point(ggplot2::aes(y=actualData),size=0.5) +
      jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
      ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
      ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = range(xBreaks)) +
      #ggplot2::coord_cartesian(ylim = rev(plotLimits)) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))

    modelPlot$plotObject <- p
    selectedModelPlots[[modelName]] <- modelPlot
    #print(p)

  }

  jaspResults[["predanMainContainer"]][["selectedModelPlots"]] <- selectedModelPlots

  return()
}



.predanForecastVerificationModelsHelper <- function(jaspResults,dataset,options,ready){
  if(!ready || length(options[["forecastVerificationSelectedModels"]]) < 1) return() ##any(!options$forecastModelBstsLocalLevelCheck || !options$forecastModelBstsLinearTrendCheck || !options$forecastModelBstsArCheck || !options$forecastModelBstsSemiLocalCheck)) return()

  if (is.null(jaspResults[["predanResults"]][["predanForecastVerificationModels"]])){

    predanForecastVerificationModels <- createJaspState()

    predanForecastVerificationModels$dependOn(.forecastVeriDependencies())

    predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
    controlData <- predanResults[[1]]

    y <- controlData$y

    if(options$forecastVerificationModelHistory >0)
      y <- tail(y,options$forecastVerificationModelHistory)

    checkedBstsModels <- unlist(options[c("forecastModelBstsLocalLevelCheck",
                                          "forecastModelBstsLinearTrendCheck",
                                          "forecastModelBstsArCheck",
                                          "forecastModelBstsSemiLocalCheck")])
    modelNamesAll <- unlist(options[["forecastVerificationSelectedModels"]])

    modelNamesAll <- sapply(modelNamesAll, .switchModelNames)
    modelNames <- modelNamesAll[!modelNamesAll %in% c("baselineVar","baselineVarMean")]
    #modelNames <- c("bsts-local","bsts-trend","bsts-ar","bsts-semi")[checkedBstsModels]

    modelList <- list()

    k <- options$forecastVerificationPredictionSteps



    for (j in seq_along(modelNames)) {

      startProgressbar(length(modelNames),gettextf(paste0("Running forecasting model ", j, " / ",length(modelNames)," :",modelNames[j])))



      modelList[[modelNames[j]]] <- .createPredictions(y = y,niter = options$forecastVerificationDraws,parallel = T,mod = modelNames[j],model_function = options$forecastVerificationModelWindow,k = k)
      #modelList[[modelNames[j]]] <-  cbind(matrix(NA,nrow = options$forecastVerificationDraws,ncol = k+1),modelList[[modelNames[j]]] )
      progressbarTick()
      }
    # baselinemodels

    if("baselineVar" %in% modelNames)
      modelList$baselineRunVar <- .baslinePredictionModel(y = y, window = 20,niter = options$forecastVerificationDraws,k=k)

    if("baselineVarMean" %in% modelNames)
      modelList$baselineRunVarMean <- .baslinePredictionModel(y = y, window = 20,niter = options$forecastVerificationDraws,runningMean = T,k = k)

    predanForecastVerificationModels$object <- modelList


    jaspResults[["predanResults"]][["predanForecastVerificationModels"]] <- predanForecastVerificationModels


    return()

  }
}

.predanForecastVerificationModelsTable <- function(jaspResults,dataset,options,ready){

  if(!ready || is.null(jaspResults[["predanResults"]][["predanForecastVerificationModels"]])) return()

  forecastMetricTable <- createJaspTable(title = "Forecast Verification Metrics Table")
  forecastMetricData <- createJaspState()

  forecastMetricTable$dependOn(c(.forecastMetricDependencies(),.forecastVeriDependencies()))
  forecastMetricData$dependOn(c(.forecastMetricDependencies(),.forecastVeriDependencies()))

  modelListForecast <-  jaspResults[["predanResults"]][["predanForecastVerificationModels"]]$object

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  controlData <- predanResults[[1]]


  if(options$forecastVerificationModelHistory >0)
    controlData <- tail(controlData,options$forecastVerificationModelHistory)

  y <- controlData$y


  forecastMetricTable$addColumnInfo(name= "Model", title = "", type = "string")

  #if(options$forecastMetricsLog) forecastMetricTable$addColumnInfo(name = "Log", title= "Log Score", type = "number")
  if(options$forecastMetricsCRPS) forecastMetricTable$addColumnInfo(name = "CRPS", title= "CRPS", type = "number")
  if(options$forecastMetricsDSS) forecastMetricTable$addColumnInfo(name = "DSS", title= "DSS", type = "number")
  ##if(options$forecastMetricsRSME) forecastMetricTable$addColumnInfo(name = "RMSE", title= "RMSE", type = "number")
  if(options$forecastMetricsAUC){
    forecastMetricTable$addColumnInfo(name = "aucUp",title = "Upper Bound", type = "number",
                                      overtitle = "Receiver Operator Characteristic")
    forecastMetricTable$addColumnInfo(name = "aucLo",title = "Lower Bound", type = "number",
                                      overtitle = "Receiver Operator Characteristic")
  }
  if(options$forecastMetricsPR){
    forecastMetricTable$addColumnInfo(name = "prUp",title = "Upper Bound", type = "number",
                                      overtitle = "Precision-Recall Score")
    forecastMetricTable$addColumnInfo(name = "prLo",title = "Lower Bound", type = "number",
                                      overtitle = "Precision-Recall Score")
  }
  if(options$forecastMetricsBrier){
    forecastMetricTable$addColumnInfo(name = "brierUp", title= "Upper Bound", type = "number",
                                      overtitle = "Brier Score",format = "sf:4;dp:4")
    forecastMetricTable$addColumnInfo(name = "brierLo", title= "Lower Bound", type = "number",
                                      overtitle = "Brier Score",format = "sf:4;dp:4")

  }

  forecastMetricData$object <- .predanForecastVerificationModelsTableFill(y,controlData,jaspResults,forecastMetricTable,modelListForecast,options)


  jaspResults[["predanMainContainer"]][["predanForecastVerifContainer"]][["forecastMetricTable"]] <- forecastMetricTable
  jaspResults[["predanResults"]][["forecastMetricData"]] <- forecastMetricData

  return()


}

.predanForecastVerificationModelsTableFill <- function(y,controlData,jaspResults,forecastMetricTable,modelListForecast,options){

  scoringResults <- list()



  k <- options$forecastVerificationPredictionSteps

  indexMetrics <- -c(1:(options$forecastVerificationModelWindow+2))

  #if(options$forecastMetricsLog) scoringResults[["Log"]] <- sapply(modelListForecast,function(x) scoringutils::logs(y[2:length(y)],t(x)))

  if(options$forecastMetricsCRPS) scoringResults[["CRPS"]] <- sapply(modelListForecast,function(x) .crpsScore(as.numeric(y[indexMetrics]),t(x[,indexMetrics])))

  if(options$forecastMetricsDSS) scoringResults[["DSS"]] <- sapply(modelListForecast,function(x) .dssScore(as.numeric(y[indexMetrics]),t(x[,indexMetrics])))
  #if(options$forecastMetricsRSME) scoringResults[["RSME"]] <-



  upperLimit <- jaspResults[["predanResults"]][["predanBounds"]][["object"]][["upperLimit"]]
  lowerLimit <- jaspResults[["predanResults"]][["predanBounds"]][["object"]][["lowerLimit"]]

  outOfBoundProbability <- .computeOutOfBoundProbability(modelList = modelListForecast,upperBound = upperLimit,lowerBound = lowerLimit,simplify = T)


  outOfBoundUpper <- outOfBoundProbability[,grepl("upper",colnames(outOfBoundProbability))]
  outOfBoundLower <- outOfBoundProbability[,grepl("lower",colnames(outOfBoundProbability))]


  #View(controlData)
  yUpper <- as.numeric(controlData$outBoundArea == "Above")
  yLower <- as.numeric(controlData$outBoundArea == "Below")



  if(options$forecastMetricsBrier){

    #brierUpper <- .brierScore(pred = outOfBoundUpper$`bsts-local.upper`[indexMetrics],yUpper[indexMetrics])
    brierUpper <- apply(outOfBoundUpper, 2, function(x) .brierScore(pred = x[indexMetrics],real = yUpper[indexMetrics]))
    brierLower <- apply(outOfBoundLower, 2, function(x) .brierScore(pred = x[indexMetrics],real = yLower[indexMetrics]))

    scoringResults[["brierUp"]] <- brierUpper
    scoringResults[["brierLo"]] <- brierLower
    scoringResults$CRPS  <- scoringResults$CRPS
    scoringResults$DSS  <- scoringResults$DSS
    #View(scoringResults)
  }

  scoringResultsMean <- as.data.frame(lapply(scoringResults,colMeans,na.rm=T))
  #View(scoringResultsMean)

  ##if(options$forecastMetricsRSME) scoringResultsMean$RSME  <- sapply(modelListForecast,function(x) sqrt(sum((y[2:length(y)] - colMeans(x,na.rm = T))^2,na.rm=T)/(length(y)-1)))
  #stop(gettext(paste0(scoringResultsMean)))

  #rownames(scoringResultsMean)[rownames(scoringResultsMean)==""] <- "climatology"



  if(options$forecastMetricsAUC | options$forecastMetricsPR){
    #View(outOfBoundLower)
    aucUpper <- try(precrec::evalmod(scores=outOfBoundUpper[indexMetrics,],
                                     labels = yUpper[indexMetrics],
                                     modnames = c(colnames(outOfBoundUpper))))
    aucLower <- try(precrec::evalmod(scores=outOfBoundLower[indexMetrics,],
                                     labels = yLower[indexMetrics],
                                     modnames = c(colnames(outOfBoundUpper))))


    if(options$forecastMetricsAUC){
      if(!jaspBase::isTryError(aucUpper)){
        rocUpperScore <- subset(precrec::auc(aucUpper),curvetypes=="ROC")[,"aucs"]
        scoringResultsMean$aucUp <- rocUpperScore
      }
      if(!jaspBase::isTryError(aucLower)){
        rocLowerScore <- subset(precrec::auc(aucLower),curvetypes=="ROC")[,"aucs"]
        scoringResultsMean$aucLo <- rocLowerScore
      }
    }
    if(options$forecastMetricsPR){
      if(!jaspBase::isTryError(aucUpper)){
        prUpperScore <- subset(precrec::auc(aucUpper),curvetypes=="PRC")[,"aucs"]
        scoringResultsMean$prUp <- prUpperScore
      }
      if(!jaspBase::isTryError(aucLower)){
        prLowerScore <- subset(precrec::auc(aucLower),curvetypes=="PRC")[,"aucs"]
        scoringResultsMean$prLo <- prLowerScore

      }
    }


  }

  if(!is.null(jaspResults[["predanResults"]][["predanBMAResults"]])){
    fitBMAList <- jaspResults[["predanResults"]][["predanBMAResults"]]$object

    bmaScores <- as.data.frame(matrix(NA,nrow = 2),row.names = c("ensemble","BMA"))
    if(options$forecastMetricsBrier){
      brierBMA <- ensembleBMA::brierScore(fitBMAList$fitBMAObject,fitBMAList$ensembleDataFrame,thresholds = c(upperLimit,lowerLimit))[-2]
      brierBMA <-  t(brierBMA)[2:3,1:2]
      colnames(brierBMA) <- c("brierUp","brierLo")
      bmaScores <- cbind(bmaScores,brierBMA)
    }

    if(options$forecastMetricsCRPS){
      crpsBMA <- c(colMeans(ensembleBMA::crps(fitBMAList$fitBMAObject,fitBMAList$ensembleDataFrame)))
      bmaScores <- cbind(bmaScores,CRPS =crpsBMA)
    }
    if(options$forecastMetricsDSS) {
      dssBMA <- c(NA,NA)
      bmaScores <- cbind(bmaScores,DSS =dssBMA)
    }
    #else
    #  dssBMA <- NULL



    if(options$forecastMetricsPR | options$forecastMetricsAUC){
      outOfBoundBMA <- ensembleBMA::cdf(fitBMAList$fitBMAObject,fitBMAList$ensembleDataFrame,values = c(upperLimit,lowerLimit))
      outOfBoundBMA[,1] <- 1-outOfBoundBMA[,1]
      outOfBoundUpperBMA <- outOfBoundBMA[,1]
      outOfBoundLowerBMA <- outOfBoundBMA[,2]

      try({


        curvesUpper <- precrec::evalmod(scores=outOfBoundUpperBMA,
                                        labels = yUpper[as.numeric(rownames(outOfBoundBMA))])
        curvesUpper <- attr(curvesUpper,"aucs")$aucs
        bmaScores[2,c("aucUp","prUp")] <- curvesUpper
      })

      try({
        curvesLower <- precrec::evalmod(scores=outOfBoundLowerBMA,
                                        labels = yLower[as.numeric(rownames(outOfBoundBMA))])
        curvesLower <- attr(curvesLower,"aucs")$aucs
        bmaScores[2,c("aucLo","prLo")] <- curvesLower
      })

    }
    scoringResultsMean <- rbind(scoringResultsMean,bmaScores[,colnames(bmaScores) %in% colnames(scoringResultsMean)])
  }


  forecastMetricTable[["Model"]] <- rownames(scoringResultsMean)

  if(options$forecastMetricsDSS) forecastMetricTable[["DSS"]] <- scoringResultsMean$DSS

  if(options$forecastMetricsCRPS) forecastMetricTable[["CRPS"]] <- scoringResultsMean$CRPS

  #if(options$forecastMetricsRSME) forecastMetricTable[["RSME"]] <- scoringResultsMean$RSME


  if(options$forecastMetricsBrier){
    forecastMetricTable[["brierUp"]] <- scoringResultsMean$brierUp
    forecastMetricTable[["brierLo"]] <- scoringResultsMean$brierLo
  }

  if(options$forecastMetricsAUC){
    try(forecastMetricTable[["aucLo"]] <- scoringResultsMean$aucLo)
    try(forecastMetricTable[["aucUp"]] <- scoringResultsMean$aucUp)
  }
  if(options$forecastMetricsPR){
    try(forecastMetricTable[["prLo"]] <- scoringResultsMean$prLo)
    try(forecastMetricTable[["prUp"]] <- scoringResultsMean$prUp)
  }


  return(scoringResultsMean)

}


.predanBMAHelperResults <- function(jaspResults,dataset,options,ready) {
  if(!ready | !options$checkBMA) return()
  if(is.null(jaspResults[["predanResults"]][["predanBMAResults"]])){

    predanBMAResults <- createJaspState()
    predanBMAResults$dependOn(.forecastVeriDependencies())
    predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
    controlData <- predanResults[[1]]
    yModel <- controlData$y

    if(options$forecastVerificationModelHistory >0)
      yModel <- tail(yModel,options$forecastVerificationModelHistory)

    modelListForecast <-  jaspResults[["predanResults"]][["predanForecastVerificationModels"]]$object

    modelListForecast <- lapply(modelListForecast, t)

    startProgressbar(1,paste0("Performing BMA - might take a while!"))


    fitBMAList <- .computeBMAHelper(y = yModel,
                                    modelList = modelListForecast,
                                    trainingPeriod = ifelse(options$forecastVerificationModelWindow == 0,
                                                            1,
                                                            options$forecastVerificationModelWindow),
                                    minimiseCRPS = T)

    progressbarTick()

    predanBMAResults$object <- fitBMAList




    jaspResults[["predanResults"]][["predanBMAResults"]] <- predanBMAResults
    # add to table
    .predanForecastVerificationModelsTable(jaspResults,dataset,options,ready)


  }
  return()
}

.predanBMAWeightsTable <- function(jaspResults,dataset,options,ready){
  if(!ready || !options$checkBMAmodelWeights || is.null(jaspResults[["predanResults"]][["predanBMAResults"]])) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["BMAWeightsTable"]] )){



    BMAWeightsTable <- createJaspTable(title = "Posterior Model Weights")


    fitBMAList <- jaspResults[["predanResults"]][["predanBMAResults"]]$object

    fitBMAObject <- fitBMAList$fitBMAObject

    if(!is.array(fitBMAObject$weights))
      stop(gettext(paste0("BMA unsuccessfull. BMA weights look like: ",fitBMAObject$weights)))

    weightsModel <- rowMeans(fitBMAObject$weights)

    BMAWeightsTable$addColumnInfo(name= "Model", title = "Model", type = "string")

    BMAWeightsTable$addColumnInfo(name= "Weight", title = "Weight", type = "number")

    BMAWeightsTable[["Model"]] <- names(weightsModel)
    BMAWeightsTable[["Weight"]] <- weightsModel


    jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["BMAWeightsTable"]] <- BMAWeightsTable


  }
  return()

}


.predanBMAPlots <- function(jaspResults,dataset,options,ready){
  if(!ready |is.null(jaspResults[["predanResults"]][["predanBMAResults"]])) return()

  fitBMAList <- jaspResults[["predanResults"]][["predanBMAResults"]]$object

  fitBMAObject <- fitBMAList$fitBMAObject
  fitBMAData <- fitBMAList$ensembleDataFrame

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  controlData <- predanResults[[1]]
  yModel <- controlData$y

  #if(options$forecastVerificationModelHistory >0)
  #  yModel <- tail(yModel,options$forecastVerificationModelHistory)



  if(options$checkBMATrainingPlot)
    .predanBMATrainingPlot(jaspResults,fitBMAObject,fitBMAData,yModel)
  if(options$checkmodelWeightsPlot)
    .predanBMAWeightsPlot(jaspResults,controlData,fitBMAObject,options,ready)

}


.predanBMATrainingPlot <- function(jaspResults,fitBMAObject,fitBMAData,yModel){

  predanBMATrainingPlot <- createJaspPlot(title = "Model Averaged Prediction",height = 480, width = 720)

  quantilesBMA <- as.data.frame(ensembleBMA::quantileForecast(fit =fitBMAObject,ensembleData = fitBMAData,quantiles = c(0.025,0.5,0.975) ),)
  colnames(quantilesBMA) <- c("lowerCI","median","higherCI")
  trainingIndex <- as.numeric(rownames(quantilesBMA))
  quantilesBMA$actual = yModel[trainingIndex]
  quantilesBMA$time <- trainingIndex

  xBreaks <- pretty(quantilesBMA$time)
  yBreaks <- pretty(c(min(quantilesBMA$lowerCI), max(quantilesBMA$higherCI)))

  p <- ggplot2::ggplot(data = quantilesBMA,mapping = ggplot2::aes(x=time,y=median)) +
    ggplot2::geom_line(col="red") + ggplot2::geom_line(ggplot2::aes(y=lowerCI))  +
    ggplot2::geom_line(ggplot2::aes(y=higherCI)) + ggplot2::geom_point(ggplot2::aes(y=actual)) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +

    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_y_continuous(breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(breaks = xBreaks,limits = range(xBreaks)) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))

  predanBMATrainingPlot$plotObject <- p

  jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["predanBMATrainingPlot"]] <- predanBMATrainingPlot

  return()

}

.predanBMAWeightsPlot <- function(jaspResults,controlData,fitBMAObject,options,ready){

  rolling <- options$modelWeightRunningWindow
  predanModelWeightPlot <- createJaspPlot(title = paste0("Rolling average of model weights with window: ",rolling),height = 320, width = 480)

  weights <- fitBMAObject$weights
  weights <- apply(weights, 1,stats::filter,rep(1/rolling,rolling))
  #filter(weights, rep(1/2,2))


  t <- tail(controlData$time,options$forecastVerificationModelHistory)[-c(1:(fitBMAObject$training$days+1))]


  rownames(weights) <- t
  weights <- reshape2::melt(weights)

  colnames(weights) <- c("time","model","value")

  xBreaks <- pretty(weights$time)

  p <-  ggplot2::ggplot(weights,ggplot2::aes(x = time,y = value,color=model)) + ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = xBreaks,limits = range(xBreaks)) +
    ggplot2::ylab("Time") + ggplot2::xlab("Weight") +
    ggplot2::ylim(0,1) + jaspGraphs::themeJaspRaw(legend.position = "right")  + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) + ggplot2::labs(color="Models")

  predanModelWeightPlot$plotObject <- p

  jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["predanModelWeightPlot"]] <- predanModelWeightPlot

  return()



}




.predanFuturePredictionHelper <- function(jaspResults,dataset,options,ready){
  if(!ready| !is.null(jaspResults[["predanResults"]][["predanFutureForecast"]])|!options$checkBoxFuturePredictions|
     (is.null(jaspResults[["predanResults"]][["predanForecastVerificationModels"]]) & is.null(jaspResults[["predanResults"]][["predanBMAResults"]]))) return()


  futurePredictionContainer <- createJaspContainer("Future Predictions")
  predanResults <- jaspResults[["predanResults"]][["predanBounds"]]$object
  modelListForecast <-  jaspResults[["predanResults"]][["predanForecastVerificationModels"]]$object

  forecastMetricData <- jaspResults[["predanResults"]][["forecastMetricData"]]$object

  controlData <- predanResults[[1]]
  upperLimit <- predanResults[[2]]

  lowerLimit <-  predanResults[[3]]
  limits <- c(upperLimit,lowerLimit)
  plotLimits <- predanResults[[4]]
  yModel <- controlData$y



  if(options$predictionModelChoice == "forecastBMA"){

    modelListFuturePredictions <- .createFutureModelList(y = yModel,
                                                         horizonFuture = options$futurePredictions,
                                                         niter = 500,modelWindow = 0,
                                                         modelsToCalculate = names(modelListForecast))

  fitBMAList <- jaspResults[["predanResults"]][["predanBMAResults"]]$object

  fitBMAObject <- fitBMAList$fitBMAObject
  fitBMAData <- fitBMAList$ensembleDataFrame

  futurePredictions <- .bmaFuturePrediction(fitBMAObject = fitBMAObject,
                                               modelListFuturePredictions = modelListFuturePredictions,
                                               weightWindow = options$modelWeightWindow,
                                               limits = c(upperLimit,lowerLimit))
  selectedModel <- "BMA"

  } else {


    metricMatch <- grepl(options$forecastModelSelectionMetric,colnames(forecastMetricData))

    if(all(!metricMatch))
      stop(gettext("'Selection metric' must be one of the metrics calculated in the forecast verification section!" ))
    orderColumns <- colnames(forecastMetricData)[metricMatch]
    forecastMetricDataOrdered <- forecastMetricData[orderColumns]
    selectedModel <-  rownames(forecastMetricDataOrdered)[with(forecastMetricDataOrdered,order(forecastMetricDataOrdered))][1]

    modelListFuturePredictions <- .createFutureModelList(y = yModel,
                                                         horizonFuture = options$futurePredictions,
                                                         niter = 500,modelWindow = 0,
                                                         modelsToCalculate = selectedModel)
    futurePredictions <- modelListFuturePredictions[[selectedModel]]



    futurePredictions <- cbind(upperProb = 1- quantInvVec(t(futurePredictions),upperLimit),
                                lowerProb = quantInvVec(t(futurePredictions),lowerLimit),
                                .extractQuantiles(futurePredictions),
                                model = selectedModel)
  }

  if(options$checkBoxOutBoundProbabilities)
    .futurePredictionsProbabilityTable(futurePredictionContainer,jaspResults,controlData,futurePredictions,selectedModel)

  if(options$checkBoxOutBoundPlot)
    .futurePredictionPlot(futurePredictionContainer,jaspResults,controlData,futurePredictions,options,plotLimits,limits,selectedModel)


  jaspResults[["predanMainContainer"]][["futurePredictionContainer"]] <- futurePredictionContainer

}

.futurePredictionsProbabilityTable <- function(futurePredictionContainer,jaspResults,controlData,futurePredictions,selectedModel){

  futurePredictions <- futurePredictions[futurePredictions$model==selectedModel,]

  outBoundTable <- createJaspTable(title= "Future out-of-bound probabilities")

  outBoundTable$addColumnInfo(name= "time", title = "Time point", type = "integer")

  outBoundTable$addColumnInfo(name= "value", title = "Prediction", type = "number")
  outBoundTable$addColumnInfo(name = "upperPred", title= "Upper", type = "number",
                              overtitle = "Prediction Interval",format = "sf:4;dp:2")
  outBoundTable$addColumnInfo(name = "lowerPred", title= "Lower", type = "number",
                              overtitle = "Prediction Interval",format = "sf:4;dp:2")

  outBoundTable$addColumnInfo(name = "upperProb", title= "Upper bound", type = "number",
                                    overtitle = "Out-of-bound Probability",format = "sf:4;dp:3")
  outBoundTable$addColumnInfo(name = "lowerProb", title= "Lower Bound", type = "number",
                                    overtitle = "Out-of-bound Probability",format = "sf:4;dp:3")


  outBoundTable[["time"]] <- futurePredictions$time + nrow(controlData)
  outBoundTable[["value"]] <- futurePredictions$mean
  outBoundTable[["upperPred"]] <- futurePredictions$higherCI
  outBoundTable[["lowerPred"]] <- futurePredictions$lowerCI

  outBoundTable[["upperProb"]] <- futurePredictions$upperProb
  outBoundTable[["lowerProb"]] <- futurePredictions$lowerProb

  futurePredictionContainer[["outBoundTable"]] <- outBoundTable

  return()
}


.futurePredictionPlot <- function(futurePredictionContainer,jaspResults,controlData,futurePredictions,options,plotLimits,limits,selectedModel){
  futurePredictionPlot <- createJaspPlot(title = paste0("Future Predictions - ",selectedModel), height = 320,width = 480)
  futurePredictions <- futurePredictions[futurePredictions$model==selectedModel,]
  futurePredictions$time <- futurePredictions$time+ nrow(controlData)

  #if(options$forecastVerificationModelHistory>0)
  #  controlDataPlot <- tail(controlData,options$forecastVerificationModelHistory)

  controlDataPlot <- controlData

  predictionData <- data.frame(time = controlDataPlot$time,
                               y = controlDataPlot$y,
                               higherCI = NA,
                               lowerCI = NA,
                               mean= NA)
  futurePredictions$y <- NA
  predictionData <- rbind(predictionData,futurePredictions[c("mean","y","time","higherCI","lowerCI")])

  xBreaks <- pretty(predictionData$time)
  yBreaks <- pretty(predictionData$y)

  p <- ggplot2::ggplot(predictionData,ggplot2::aes(time,y)) + ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes(time,mean))+
    ggplot2::geom_ribbon(ggplot2::aes(ymin=lowerCI,ymax=higherCI),alpha=0.2,fill="blue") +
    jaspGraphs::themeJaspRaw() +
    jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::coord_cartesian(ylim = rev(plotLimits)) +
    ggplot2::geom_hline(yintercept = limits[1],linetype="dashed",color="darkred") +
    ggplot2::geom_hline(yintercept = limits[2],linetype="dashed",color="darkred") +
    ggplot2::scale_y_continuous(breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(breaks = xBreaks,limits = range(xBreaks)) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))


  futurePredictionPlot$plotObject <- p

  futurePredictionContainer[["predictionFutureReport"]] <- createJaspReport(
    text     = gettextf("Future predictions indicate that process will go out of control above the upper limit with a probability of %f%%.\nAnd below the lower limit with a probability of %f%% ", round(max(futurePredictions$upperProb),2), round(max(futurePredictions$lowerProb),2)),
    report   = any(futurePredictions$upperProb > 0.5) | any(futurePredictions$lowerProb > 0.5),
    position = 0
  )

  futurePredictionContainer[["futurePredictionPlot"]] <- futurePredictionPlot

  return()

}
