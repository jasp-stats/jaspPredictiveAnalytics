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
    ready <- options$dependent != "" & options$time != ""

    dataset <- .predanReadData(options,ready)

    .predanContainerSetup(jaspResults, ready)
    .predanComputeResults(jaspResults, dataset, options, ready)
    .predanPlotsDescriptives(jaspResults,dataset,options,ready)

    .predanACFDescriptives(jaspResults,dataset,options,ready)
    .predanHistogramPlot(jaspResults,dataset,options,ready)
    .predanDiagnosticTables(jaspResults, dataset, options,ready)


    .predanFeatureEngineeringHelper(jaspResults,dataset,options,ready)
    .predanForecastVerificationHelper(jaspResults,dataset,options,ready)

    .predanMetricTable(jaspResults,options,ready)
    .predanPitPlots(jaspResults,options,ready)
    .predanForecastVerificationResultPlot(jaspResults,options,ready)

    .predanBMAHelperResults(jaspResults,dataset,options,ready)
    .predanBMAWeightsTable(jaspResults,dataset,options,ready)

    .predanFuturePredictionResults(jaspResults,dataset,options,ready)
    .predanFuturePredictionPlot(jaspResults,dataset,options,ready)
    .predanFuturePredictionTable(jaspResults,dataset,options,ready)

    return()
}

.extractQuantiles <-function(state){
  data.frame(mean = colMeans(state,na.rm = T),

             lowerCI = apply(state,2,quantile,probs= 0.025,na.rm = T),
             higherCI= apply(state,2,quantile,probs= 0.975,na.rm = T),
             tt = 1:ncol(state)
  )
}
.predanReadData <- function(options,ready) {
  if(!ready) return()
  numericVariable <- c(options$dependent,unlist(options$covariates),options$trainingIndicator)
  numericVariable <- numericVariable[numericVariable != ""]
  nominalVars     <- unlist(options$factors)
  timeVar <- unlist(options$time)
  timeVar <- timeVar[timeVar != ""]
  dataset <- .readDataSetToEnd(columns.as.numeric = numericVariable,
                               columns.as.factor = nominalVars,
                               columns = timeVar)

  #options$dataset <- dataset

  return(dataset)

}

.predanContainerSetup <- function(jaspResults, options) {

  if (is.null(jaspResults[["predanMainContainer"]])){
    predanMainContainer <- createJaspContainer()
    jaspResults[["predanMainContainer"]] <- predanMainContainer

  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]])){
    predanDescriptivesContainer <- createJaspContainer("Descriptives", position = 1)
    jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]] <- predanDescriptivesContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]])){
    predanDiagnosticsContainer <- createJaspContainer("Diagnostics", position = 2)
    jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]] <- predanDiagnosticsContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["cvContainer"]])){
    cvContainer <- createJaspContainer("Forecast Verification", position = 3)
    jaspResults[["predanMainContainer"]][["cvContainer"]] <- cvContainer
  }

  if(is.null(jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]])){
    predanModelSelectionContainer <- createJaspContainer("Model Selection", position = 4)
    jaspResults[["predanMainContainer"]][["predanModelSelectionContainer"]] <- predanModelSelectionContainer
  }

  if (is.null(jaspResults[["predanMainContainer"]][["predanBMAContainer"]])){
    predanBMAContainer <- createJaspContainer("Bayesian Model Averaging", position = 5)
    jaspResults[["predanMainContainer"]][["predanBMAContainer"]] <- predanBMAContainer
  }
  if (is.null(jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]])){
    predanBMAContainer <- createJaspContainer("Future Prediction", position = 6)
    jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]] <- predanBMAContainer
  }


  return()
}


###### Helper Functions


# finds intersection between lines
.findCross <- function(tt,y,time,limit){
  d <- y - limit
  signChanges <- (d[1:(length(d)-1)] * d[2:length(d)]) <= 0
  left <- c(signChanges, FALSE)
  right <- c(FALSE, signChanges)
  t <- (limit - y[left])/(y[right] - y[left])

  x2 <- (1 - t)*tt[left] + t*tt[right]

  time2 <- as.numeric(time)
  x2time <- (1 - t)*time2[left] + t*time2[right]
  time3 <- as.POSIXct(x2time,origin = "1970-01-01")

  if(length(x2 ) > 0)
    return(data.frame(y=limit, time = time3,tt  = x2, include=0,outBound=T))
  else
    return(data.frame(y=NULL,time = NULL, tt=NULL,  include=NULL,outBound=NULL))
}



##### model selection helper functions


#.modelSelection




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




##### dependencies


.modelDependencies <- function(){
  return(c("dependent",
           "time",
           "covariates",
           "factors",
           "trainingIndicator",
           "featEngLags",
           "featEngAutoTimeBased"
           ))
}


.boundDependencies <- function(){
  return(c("dependent","time","errorBoundMethodDrop",
           "manualBoundMethod",
           "manualBoundMean",
           "manualBoundErrorBound",
           "manualUpperBound",
           "manualLowerBound",
           "sigmaBound",
           "controlPeriodCheck",
           "controlPeriodStart",
           "controlPeriodEnd",
           "trimmedMeanCheck",
           "trimmedMeanPercent"))
}



.forecastVeriDependencies <- function(){
  return(c("covariates",
           "factors",
           "resampleForecastHorizon",
           "resampleInitialTraining",
           "resampleSkip",
           "resampleMaxSlice",
           "resampleCumulativeCheck",
           "resampleSliceStart"))
}



.forecastMetricDependencies <- function(){
  return(c("forecastMetricsCRPS",
           "forecastMetricsDSS",
           "forecastMetricsAUC",
           "forecastMetricsPR",
           "forecastMetricsBrier"))
}


.bmaResultsDependencies <- function(){
  return(c("checkPerformBma",
           "bmaMethod",
           "bmaTestPeriod",
           "bmaTestProp"
           ))
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
    predanBoundsState$dependOn(c(.modelDependencies(),.boundDependencies()))
    jaspResults[["predanResults"]][["predanBounds"]] <- predanBoundsState
  }

}

.predanComputeBounds <- function(dataset,options) {

  dataControl <- data.frame(y =  dataset[,options[["dependent"]]], time = as.POSIXct( dataset[,options[["time"]]]))
  dataControl$tt <- 1:nrow(dataControl)

  if (options$trainingIndicator != "")
    idx <- as.logical(dataset[[encodeColNames(options$trainingIndicator)]])
  else
    idx <- T

  print(idx)

  #View(dataControl)
  dataControl <- dataControl[idx,]

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
  #if(!ready) return()

  predanDescriptivesContainer <- jaspResults[["predanMainContainer"]][["predanDescriptivesContainer"]]

  #predanDescriptivePlots <- createJaspContainer(title=gettext("Descriptives"),position =1)

  predanResults <- jaspResults[["predanResults"]][["predanBounds"]][["object"]]



  if(options$controlPlotCheck){

    title <- gettextf("Basic control plot")
    predanControlPlot <- createJaspPlot(title = title, height = 480, width = 720,dependencies = c("controlPlotGrid","controlPlotReportingCheck"))

    predanDescriptivesContainer[["predanControlPlot"]] <- predanControlPlot

    .predanBasicControlPlotFill(jaspResults,predanResults,predanDescriptivesContainer,options,ready,zoom=F)

  }

  if(options$controlPlotZoomCheck && options$zoomPeriodEnd >0){
    title <- "Basic Control Plot - Focused"
    predanControlPlot <- createJaspPlot(title= title, height = 480, width = 720,dependencies = "controlPlotGrid")

    predanDescriptivesContainer[["predanControlPlotZoom"]] <- predanControlPlot


    .predanBasicControlPlotFill(jaspResults,predanResults,predanDescriptivesContainer,options,ready,zoom=T)

  }

  #jaspResults[["predanMainContainer"]][["predanDescriptivePlots"]] <- predanDescriptivePlots
  return()
}



.predanBasicControlPlotFill <- function(jaspResults,predanResults,predanDescriptivesContainer,options,ready,zoom){
  if(!ready) return()

  controlData <- predanResults[["dataControl"]]

  upperLimit <- predanResults[["upperLimit"]]
  lowerLimit <- predanResults[["lowerLimit"]]
  plotLimit <- predanResults[["plotLimit"]]


  if(zoom){
    start <- options$zoomPeriodStart
    end <- options$zoomPeriodEnd
    if(end > nrow(controlData))
      end <- nrow(controlData)

    controlData <- controlData[start:end,]
  }

  #jaspReport feature



  percOutControl <- sum(controlData$outBoundNum)/sum(!is.na(controlData$y))

  if(options$controlPlotReportingCheck){


    # compute boundary breach
    warningIndicator <- percOutControl > options$controlPlotReportingPercent

    outBoundMax <- round(percOutControl,3)*100

    warningText <- ifelse(warningIndicator,
                          paste0("Warning! The out-of-bounds percent threshold has been crossed. ",outBoundMax, "% of the data is out of bounds."),
                          paste0("No warning. Only ",outBoundMax, "% of the data is out of control"))


    predanDescriptivesContainer[["controlPlotReport"]] <- createJaspReport(
      text =  warningText,
      report = warningIndicator,
      dependencies = c("controlPlotReportingCheck","controlPlotReportingPercent"),
      position = 1)

  }



  plotData <- controlData[1:4]
  plotData$include = 1
  # add intersections between lines and the control boundaries
  plotData <- rbind(plotData,
                    .findCross(plotData$tt,plotData$y,plotData$time,upperLimit),
                    .findCross(plotData$tt,plotData$y,plotData$time,lowerLimit))


  plotData <- plotData[order(plotData$tt),]
  plotData$includeLine <- NA
  plotData$includeLine[1] <- plotData$outBound[1]
  for (i in 2:nrow(plotData)) {
    plotData$includeLine[i] <- ifelse((plotData$include[i] == 0 & plotData$include[i+1] &plotData$outBound[i+1]==T)|
                                        plotData$outBound[i]&plotData$outBound[i+1]==T,T,F)

  }

  t_var <- ifelse(options$"controlSpreadPointsEqually","tt","time")

  xBreaks <- pretty(plotData[[t_var]])
  if(options$xAxisLimit =="controlBounds")
    yBreaks <- pretty(plotLimit)
  else
    yBreaks <- pretty(plotData$y)



  #predanControlPlot <- createJaspPlot(title= title, height = 480, width = 720,dependencies = "controlPlotGrid")
  p <-ggplot2::ggplot(plotData[plotData$include==1,],ggplot2::aes(.data[[t_var]],y,group=1,colour=ggplot2::after_stat(y>upperLimit|y<lowerLimit))) +
    ggplot2::geom_hline(na.rm = T,yintercept = upperLimit,linetype="dashed",color="darkred") +
    ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred") +
    ggplot2::scale_color_manual(guide="none",values=c("#4E84C4","#D16103"))


  if(options$controlLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=1,data=plotData,ggplot2::aes(.data[[t_var]],y,colour=ggplot2::after_stat(plotData$includeLine)))
  if(options$controlLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=2)

  if(options$xAxisLimit == "controlBounds")
    p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                             plotLimit[[1]]))

  p <- p + jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::scale_y_continuous(name = "Y",breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = "Time",breaks = xBreaks,limits = range(xBreaks))

  if(options$controlPlotGrid)
    p <- p + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid)

  #predanControlPlot$plotObject <- p




  #jaspResults[["testPlot"]] <- predanControlPlot
  if(!zoom)
    predanDescriptivesContainer[["predanControlPlot"]]$plotObject <- p
  else
    predanDescriptivesContainer[["predanControlPlotZoom"]]$plotObject <- p
  return()
}



.predanACFDescriptives <- function(jaspResults,dataset,options,ready){
  if((!options$acfPlotCheck || !ready) || !is.null(jaspResults[["predanMainContainer"]][["predanDiagnosticsContainer"]][["acfPlots"]])) return()

  acfPlots <- createJaspContainer(title= gettext("Autocorrelation Function Plots"),dependencies = "acfPartialCheck")
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
    jaspGraphs::themeJaspRaw() + ggplot2::ylim(c(NA,1))

  predanACFPlot <- createJaspPlot(title= "Autocorrelation Function", height = 340, width = 480)

  predanPACFPlot <- createJaspPlot(title= "Partial Autocorrelation Function", height = 340, width = 480,dependencies = "acfPartialCheck")

  predanACFPlot$plotObject <- acfPlot
  predanPACFPlot$plotObject <- pacfPlot

  acfPlots[["predanACFPlot"]] <- predanACFPlot
  if(options$acfPartialCheck)
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

  #if (options$outlierHistogramDensity)
  #  yhigh <- max(h$counts)

  p <- ggplot2::ggplot(dataControl,
                       ggplot2::aes(x= y,
                                    binwidth = 0.5))




  #if(!options$outlierHistogramDensity){
    p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(
      y= ..count.. ,
      fill = outBound),
      colour="black",
      binwidth = binWidthType,
      breaks = h[["breaks"]],
      position = "stack") +
      ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF"))  +
      ggplot2::guides(color = "none") +
      ggplot2::scale_x_continuous(name = "Y",breaks = xticks) +
      ggplot2::scale_y_continuous(name = "Count",breaks = base::pretty(c(0, h$counts))) +
      ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103"))

  #} else {
  #  #dftmp <- data.frame(x = range(xticks), y = range( c(0,  max(h$density))))
  #  #p <- p + ggplot2::geom_line(data = dftmp,
  #  #                            mapping = ggplot2::aes(x = .data$x,
  #  #                                                   y = .data$y), color = "white", alpha = 0)
  #  p <- p + ggplot2::geom_histogram(mapping = ggplot2::aes(
  #    y= ..density..,
  #    fill = outBound),
  #    colour= "black",
  #    size     = .7,
  #    binwidth = binWidthType,
  #    breaks = xticks,
  #    position = "stack") +
  #    ggplot2::geom_density(mapping = ggplot2::aes(fill=NULL)) +
  #    ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF"))  +
  #    ggplot2::scale_x_continuous( breaks = xticks,limits = c(xticks[1],max(xticks))) +
  #    ggplot2::guides(color = "none") +
  #    ggplot2::scale_y_continuous(name = "Density") +
  #    ggplot2::scale_fill_manual(values=c("#4E84C4","#D16103")) +
  #    ggplot2::guides(color = FALSE)
  #}
#
  p <- jaspGraphs::themeJasp(p)
#
  #if(options$outlierHistogramDensity){
  #  p <- p + ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
  #                          axis.text.y = ggplot2::element_blank())
  #}

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
  summaryTable$addColumnInfo(name="percent",  title=gettext("Percent"),   type="number", format= "pc;dp:2")
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
  tableRes$percent = round(tableRes[,6]/nrow(dataControl),4)

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
    title <- "Outlier Table - Focused"
  } else
    title <- "Outlier Table"

  outlierTable <- createJaspTable(title = title)

  outlierTable$dependOn("outlierTableTransposeCheck")

  if(options$outlierTableTransposeCheck)
    outlierTable$transpose <- TRUE


  outlierTable$addColumnInfo(name= "time",      title = "Time",             type = "integer")

  outlierTable$addColumnInfo(name="variable",   title = "Control area",           type = "string")

  outlierTable$addColumnInfo(name= "value",     title = "Value",                  type = "number", format= "dp:2")

  outlierTable$addColumnInfo(name= "deviation", title = "Deviation",   type = "integer", format= "dp:2")

  for(i in dataControl$tt[dataControl$outBound]){
    row <- list(
      time = dataControl$tt[i],
      variable = dataControl$outBoundArea[i],
      value = dataControl$y[i],
      deviation = dataControl$distance[i])

    outlierTable$addRows(row)
  }


  diagnosticTables[["outlierTable"]] <- outlierTable

  return()

}

### featureEng Helper

lagit <- function(a,k) {
  tmp <- lapply(k,function(i) c(rep(NA,i),head(a,length(a)-i)))
  res <- as.data.frame(cbind(a,do.call(cbind,tmp))[,-1])
  colnames(res) <- paste0("y_lag",c(k))
  res
}




.predanFeatureEngineeringHelper <- function(jaspResults,dataset,options,ready){
  if(!ready || !is.null(jaspResults[["predanResults"]][["featureEngState"]])) return()

  featureEngState <- createJaspState()
  featureEngState$dependOn(c("featEngLags","featEngAutoTimeBased","featEngImputeTS","covariates","factors","dependent"))
  #featEngData <- data.frame(y =  dataset[,options[["dependent"]]], time = as.POSIXct( dataset[,options[["time"]]]))

  if (options$trainingIndicator != ""){
    idx <- as.logical(dataset[[encodeColNames(options$trainingIndicator)]])
    featureEngStateFuture <- createJaspState()

  }else
    idx <- T



  featEngData <- dataset[,c(options[["dependent"]],jaspBase::encodeColNames(options$time))]
  if(length(options$covariates)>0)
    featEngData <- cbind(featEngData,dataset[,jaspBase::encodeColNames(options$covariates),drop=F])
  colnames(featEngData)[1:2] <- c("y","time")

  featEngData$time <- as.POSIXct(featEngData$time)
  print(colnames(featEngData))
    #convert factors to dummy vars via model.matrix

  if(length(options$factors) > 0){

    fctrs <- dataset[,jaspBase::encodeColNames(options$factors),drop=F]
    colnames(fctrs) <- decodeColNames(colnames(fctrs))
    featEngData <- as.data.frame(cbind(featEngData,model.matrix(~.-1,fctrs)))

  }


  if(options$featEngImputeTS)
    featEngData$y <- imputeTS::na_interpolation(featEngData$y)

  ##TODO: add
  if(options$featEngLags > 0)
    featEngData <- cbind(featEngData,as.data.frame(lagit(featEngData$y,k = 1:options$featEngLags)))

  if(options$featEngAutoTimeBased)
    featEngData <- cbind(featEngData,get_timeseries_signature_date(featEngData$time))






  #stop(gettext(paste0(colnames(featEngData))))

  #colnames(featEngData) <- decodeColNames(colnames(featEngData))


  featEngDataFuture <- featEngData[!idx,]
  featEngData <- featEngData[idx,]


  featureEngState$object <- as.data.frame(featEngData)
  jaspResults[["predanResults"]][["featureEngState"]] <- featureEngState

  if(options$trainingIndicator != ""){
    featureEngStateFuture$object <- featEngDataFuture
    jaspResults[["predanResults"]][["featureEngStateFuture"]] <- featureEngStateFuture
  }
  return()
}

######  CV Helper Function ######


.predanForecastVerificationHelper <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["featureEngState"]])) return()

  dataControl <- jaspResults[["predanResults"]][["predanBounds"]]$object[[1]]

  if(is.null(jaspResults[["predanResults"]][["cvPlanState"]])){
    cvPlanState <- createJaspState(dependencies = c(.modelDependencies(),.forecastVeriDependencies()))
    cvPlanState$object <- .crossValidationPlanHelper(data = na.omit(dataControl),
                                                     initial = options$resampleInitialTraining,
                                                     assess = options$resampleForecastHorizon,
                                                     cumulative = options$resampleCumulativeCheck,
                                                     lag = options$featEngLags,
                                                     skip = options$resampleSkip,
                                                     max_slice = options$resampleMaxSlice,
                                                     from = options$"resampleSliceStart")
    jaspResults[["predanResults"]][["cvPlanState"]] <- cvPlanState
  }

  if(is.null(jaspResults[["predanMainContainer"]][["cvContainer"]][["cvPlanPlot"]]) && options$resampleCheckShowTrainingPlan){
    cvPlot <- createJaspPlot(title = "Forecast Evaluation Plan" ,width = 720,height = 180*min(c(
      length(jaspResults[["predanResults"]][["cvPlanState"]]$object), options$resamplePlanPlotMaxPlots)))
    cvPlot$dependOn(c(.modelDependencies(),
                      .forecastVeriDependencies(),
                      "resampleCheckShowTrainingPlan",
                      "resamplePlanPlotEqualDistance",
                      "resamplePlanPlotMaxPlots"))
    cvPlot$plotObject <- .cvPlanPlot(data = dataControl,
                                     cvPlan = jaspResults[["predanResults"]][["cvPlanState"]]$object,
                                     equal_distance = options$resamplePlanPlotEqualDistance,
                                     maxSlices = options$resamplePlanPlotMaxPlots,ncol=1)
    jaspResults[["predanMainContainer"]][["cvContainer"]][["cvPlanPlot"]] <- cvPlot
  }


  if(is.null(jaspResults[["predanResults"]][["cvResultsState"]])){
    if(length(options$selectedModels) == 0) return()
    cvResultsState <- createJaspState(dependencies = c(.modelDependencies(),
                                                       .forecastVeriDependencies(),
                                                       "selectedModels"))

    dataEng <- na.omit(jaspResults[["predanResults"]][["featureEngState"]]$object)

    modelList <- .createModelListHelper(dataEng,unlist(options$selectedModels))

    cvResults <- list()
    print(paste0("models:",sapply(modelList,"[","model")))


    # TODO: specify which model is running
    startProgressbar(
      length(modelList),
      gettextf("Running models")
    )
    for (i in 1:length(modelList)) {


      cvResults[[i]] <- .crossValidationHelperSlices(model = modelList[[i]]$model,
                                                     formula = modelList[[i]]$modelFormula,
                                                     data = dataEng,
                                                     cvPlan = jaspResults[["predanResults"]][["cvPlanState"]]$object,
                                                     preProList = T,keepModels = "summary",keepMetrics = "fully")


      progressbarTick()
      cvResults[[i]]$modelName <- options$selectedModels[[i]]
    }

    names(cvResults) <- paste0(sapply(modelList, function(x) x$model),seq_along(modelList))

    cvResultsState$object <- cvResults

    jaspResults[["predanResults"]][["cvResultsState"]] <- cvResultsState

   # if(options$checkPerformBma)
   #   selectedMod <- options$selectedModels
   # else
   #   selectedMod <- c(unlist(options$selectedModels),"BMA")
    jaspResults[["plottableModelsQml"]] <- createJaspQmlSource(sourceID="plottableModelsQml", value= options$selectedModels,dependencies = "checkPerformBma")
  }

  return()
}


.crossValidationPlanHelper <- function(data,initial = 20,assess = 10,cumulative = TRUE,skip = 10,lag = 0, max_slice = 5,from = c("head","tail")){
  from <- match.arg(from)
  n <- nrow(data)

  if(from == "tail"){
    stops <- n - seq(assess, (n - initial), by = skip)
    if (!cumulative) {
      starts <- stops - initial + 1
    } else {
      starts <- rep(1, length(stops))
    }

  } else {
    stops <- seq(initial, (n - assess), by = skip + 1)
    starts <- if (!cumulative) {
      stops - initial + 1
    } else {
      starts <- rep(1, length(stops))
    }
  }
  in_ind <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  out_ind <- mapply(seq, stops + 1 - lag, stops + assess, SIMPLIFY = FALSE)
  merge_lists <- function(a, b) list(analysis = a, assessment = b)
  indices <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
  names(indices) <- paste0("slice",length(indices):1)

  if(from == "tail"){
    indices <- rev(head(indices,max_slice))
  }
  else {
    names(indices) <- rev(names(indices))
    indices <- head(indices,max_slice)
  }

  return(indices)

}

.cvPlanPlot <- function(data,cvPlan,maxSlices=2,equal_distance = T,...){

  t_var <- ifelse(equal_distance,"tt","time")

  data$X <- 1:nrow(data)
  dataPlot <- dplyr::bind_rows(.id = "id",lapply(head(cvPlan,maxSlices), function(x) data.frame( tt = c(x$analysis,x$assessment),
                                                                                                 type = rep(c("Analysis","Assessment"),c(length(x$analysis),length(x$assessment)) ))))

  dataPlot <- dplyr::left_join(dataPlot,data[,c("tt","time","y")],by = c("tt"))
  dataPlot$id <- factor(dataPlot$id,levels = names(cvPlan),ordered = F)

  xBreaks <- pretty(dataPlot[[t_var]])
  yBreaks <- pretty(dataPlot$y)


  p <- ggplot2::ggplot(dataPlot,ggplot2::aes_string(t_var,"y",color="type",group = "1")) + ggplot2::geom_line() +
    ggplot2::scale_color_manual(name = "Time period",values=c("#4E84C4","#D16103")) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::scale_y_continuous(name = "Y",breaks = yBreaks,limits = range(yBreaks)) +
    ggplot2::scale_x_continuous(name = "Time",breaks = xBreaks,limits = range(xBreaks)) +
    ggplot2::facet_wrap(facets  = "id",scales = "free",...) +jaspGraphs::geom_rangeframe() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid,
                   panel.background = ggplot2::element_rect(fill = "white"),
                   legend.position = "bottom")

  return(p)
}

.createModelListHelper <- function(dataEng, selectedModels){

  modNames <- c("linear regression - y ~ time"= "lmSpike",
              "linear regression - regression"= "lmSpikeReg",
              "linear regression - regression + lag"= "lmSpikeRegLag",
              "bsts - linear trend model"= "bstsLinear",
              "bsts - linear trend model - regression"= "bstsLinearReg",
              "bsts - linear trend model - regression + lag"= "bstsLinearLag",
              "bsts - autoregressive model"= "bstsAr",
              "bsts - autoregressive model - regression"= "bstsArReg",
              "bsts - autoregressive model - regression + lag"= "bstsArRegLag",
              "prophet"= "prophet",
              "prophet - regression"= "prophetReg",
              "prophet - regression + lag"= "prophetRegLag",
              "xgboost - regression"= "xgboostReg",
              "xgboost - regression + lag"= "xgboostRegLag",
              "bart - regression" = "bartReg",
              "bart - regression + lag" = "bartRegLag",
              "bart - stack" = "bartStackReg")



  shortNames <- modNames[selectedModels]

  res <- lapply(shortNames, function(x){
    names(selectedModels) <- selectedModels
    model <- gsub("Lag|Reg","",x)

    # check whether model is only y ~ time, regression or has lag component
    # create formula accordingly
    modelFormula <- switch(
      sum(unlist(sapply(c(model,"Reg", "Lag"),grep,x))),
      "1" = formula(y ~ time),
      "2" = formula(dataEng[,!grepl("lag",colnames(dataEng))]),
      "3" = formula(dataEng)
    )
    # currently unused but option to extend model
    #with custom parameters such as MCMC draws, exptected model size,...
    #model_args = NULL
    #stop(gettext(paste0(modelFormula)))

    return(list(model = model,modelFormula = modelFormula,modelName = names(modNames)[modNames==x]))
  })
  print(res)
}



# wrapper that performs preprocessing, trains model and does prediction
.predAnModelFit <- function(trainData, formula, method, fit,predictFuture = F, testData,model_args = list(),preProList = NULL,...){


  lags <- sum(grepl("y_lag",labels(terms(formula))))
  #if(lags>0){
  #  trainData <- na.omit(cbind(trainData,lagit(trainData$y,1:lags)[,-1]))
  #  print("lag")
#
  #}

  #if(method == "lmSpike"){
  #  preProSpec <- preProcess.default(trainData[,!grepl("y",colnames(trainData))],verbose = F)
  #  trainData <- predict.preProcess(preProSpec,trainData)
  #  if(!is.null(testData)) testData <- predict(preProSpec,testData)
#
  #}

  # only perform preprocessing if predictors present apart from ~ time
  if(sum(!grepl("y|time",colnames(trainData))) > 0){
    preProSpec <- preProcess.default(trainData[,!grepl("y",colnames(trainData))],verbose = F,method = c("center", "scale","zv"))

    #identify factors with too
    #fctr <- sapply(trainData,is.factor)
    #factorsToRemove <-names(sapply(trainData[,fctr], function(x) length(unique(x)) < 2))
    #preProSpec$method$remove <- append(preProSpec$method$remove,factorsToRemove)
    #print(paste0("to remove:",preProSpec$method$remove,collapse = " "))
    trainData <- predict.preProcess(preProSpec,trainData)

    # remove args if they have zero var from formula
    if(length(preProSpec$method$remove) >0 )
      formula <- update(formula, paste0(".~." , paste0("-",preProSpec$method$remove,collapse = " ")))

    if(!is.null(testData)) testData <- predict.preProcess(preProSpec,testData)



  }

  args <- c(model_args,list(...))
  pred <- NULL
  fit <- switch (method,
                 "lmSpike" = do.call(.lmSpikeFitHelper,c(list(trainData,formula,"lmSpike",...),model_args)),
                 "bstsAr" = do.call(.bstsFitHelper,c(list(trainData,formula,"bstsAr",...),model_args)),
                 "bstsLinear" = do.call(.bstsFitHelper,c(list(trainData,formula,"bstsLinear",...),model_args)),
                 "prophet" = do.call(.prophetFitHelper,c(list(trainData,formula,...),model_args)),
                 "xgboost" = do.call(.xgbFitHelper,c(list(trainData = trainData,formula = formula),model_args,...)),
                 "bart" = do.call(.bartFitHelper,c(list(trainData = trainData,formula = formula),model_args,...)),
                 "bartStack" = do.call(.bartStackFit,c(list(trainData = trainData,formula = formula,testData = testData),model_args,...))
  )
  if(predictFuture & lags == 0){

    pred <- switch (method,
                    "lmSpike" = do.call(.lmSpikePredictHelper,c(list(fit$model,testData,trainData,formula,...),model_args)),
                    "bstsAr" = .bstsPredictHelper(fit$model,testData, horizon,formula,...),
                    "bstsLinear" = .bstsPredictHelper(fit$model,testData, horizon,formula,...),
                    "prophet" = .prophetPredictHelper(fit$model,testData,horizon,...),
                    "xgboost" = do.call(.xgbPredHelper,list(fit$model,testData,formula = formula)),
                    "bart" = do.call(.bartPredictHelper,list(fit$model,testData,formula = formula,...)),
                    "bartStack" = do.call(.bartStackPredictHelper,list(fit = fit$model,testData = testData,formula = formula,trendPred = fit$trendPred,...))
    )

  } else if(lags > 0){

    pred <- .recursivePredict(testData,trainData,formula,method,model = fit$model,lags = lags)

  }
  if(is.null(pred))
    return(list(fit = fit))

  return(list(fit = fit, pred=pred))


}



# helper function that wraps around model wrapper and performs cv across slices
# option to discard models fully and only keep prediction of state MCMC draws
# option to calculate metrics directly and then discard full MCMC and only keep sample to save memory
.crossValidationHelperSlices <- function(model,
                                        formula,
                                        data,
                                        cvPlan,
                                        keepModels = c("fully","distr","summary","none"),
                                        keepMetrics = c("fully", "summary","none"),
                                        metrics = c("crps","dss","log","coverage","bias", "pit","mae","rmse","rsq"),
                                        model_args=list(),
                                        ...) {


  keepModels <- match.arg(keepModels)
  keepMetrics <- match.arg(keepMetrics)
  cvResList <- list()
  cvModelObject <- list()

  cvModelObject <- lapply(X = 1:length(cvPlan),function(i){
    system(sprintf('echo "\n%s\n"', paste0("fitting slice " , i, " of ",model)))
    .predAnModelFit(trainData = data[as.character(cvPlan[[i]]$analysis),],
                    testData = data[as.character(cvPlan[[i]]$assessment),],
                    predictFuture = T,
                    method = model,
                    formula = formula,
                    model_args =model_args,...
  )
  })

  l <- lapply(cvModelObject, function(x) x$pred$dist)
  # time,draw,slice array
  predArray <- array(unlist(l),dim=c(dim(l[[1]]),length(l)),dimnames = list(1:dim(l[[1]])[1],1:dim(l[[1]])[2],names(cvModelObject)))


  realMatrix <- matrix(unlist(lapply(cvModelObject, function(x) x$pred$trueVal)),ncol = length(cvModelObject)#,
                       #dimnames = list(1:dim(l[[1]])[1],names(cvModelObject))
                       )

  predSummary <- aperm(apply(X = predArray,c(1,3) ,
                             function(x) c(mean = mean(x),
                                           upr = quantile(x,0.975),
                                           lwr = quantile(x,0.025)),
                             simplify = T),perm = c(2,1,3))

  if(keepMetrics %in% c("summary","fully")){
    scoringArray <- sapply(X = 1:length(l),simplify = "array",function(x) .scorePred(predMatrix = predArray[,,x],real = realMatrix[,x],metrics) )
    dimnames(scoringArray)[3] <- list(names(l))
    cvResList$scoringSummary <- apply(scoringArray, c(3,2), mean)
    if(keepMetrics == "fully")
      cvResList$scoringArray <- scoringArray
  }


  if(keepModels %in% c("fully","distr","summary")){
    cvResList$predSummary <- predSummary
    if(keepModels %in% c("fully","distr")){
      cvResList$predArray <- predArray
      if(keepModels == "fully")
        cvResList$slices <- cvModelObject
    }
  }
  cvResList$realMatrix <- realMatrix
  dimnames(cvResList$realMatrix)[2] <- list(names(cvPlan))
  return(cvResList)
}



.scorePred <- function(predMatrix,real,metrics = c("crps","dss","log","coverage","bias", "pit","mae","rmse","rsq"),SD=NULL){
  #metrics <- match.arg(metrics,several.ok = T)
  resScoringMatrix <- matrix(ncol = length(metrics),nrow = length(real),dimnames = list(NULL,metrics))
  if(is.matrix(predMatrix)){
    if("crps" %in% metrics)
      resScoringMatrix[,"crps"] <- scoringRules::crps_sample(real,predMatrix)
    if("dss" %in% metrics)
      resScoringMatrix[,"dss"] <- scoringRules::dss_sample(real,predMatrix)
    if("log" %in% metrics)
      resScoringMatrix[,"log"] <- scoringRules::logs_sample(real,predMatrix)
    if("coverage"  %in% metrics)
      resScoringMatrix[,"coverage"] <- apply(predMatrix, 1, quantile,0.25) < real & real < apply(predMatrix, 1, quantile,0.75)

    if("bias" %in% metrics)
      resScoringMatrix[,"bias"] <- scoringutils::bias_sample(true_values = real,predictions = predMatrix)

    if("pit" %in% metrics)
      resScoringMatrix[,"pit"] <- scoringutils::pit_sample(true_values = real,predictions = predMatrix)

    predMatrix <- rowMeans(predMatrix)

  } else if(hasArg(SD)){
    if("crps" %in% metrics)
      resScoringMatrix[,"crps"] <- scoringRules::crps_norm(real,predMatrix,sd = SD)
    if("dss" %in% metrics)
      resScoringMatrix[,"dss"] <- scoringRules::dss_norm(real,predMatrix,sd = SD)
    if("log" %in% metrics)
      resScoringMatrix[,"log"] <- scoringRules::logs_norm(real,predMatrix,sd = SD)
    if("coverage"  %in% metrics)
      resScoringMatrix[,"coverage"] <- qnorm(0.25,predMatrix,SD) < real & real < qnorm(0.25,predMatrix,SD,lower.tail = F)

    if("pit" %in% metrics)
      resScoringMatrix[,"pit"] <- NA
  }
  if("rmse" %in% metrics)
    resScoringMatrix[,"rmse"] <- sqrt(mean((real- predMatrix)^2))

  if("mae" %in% metrics)
    resScoringMatrix[,"mae"] <- mean(abs(real -predMatrix))

  if("rsq" %in% metrics)
    resScoringMatrix[,"rsq"] <- cor(predMatrix,real)^2

  return(resScoringMatrix)
}


.predanMetricTable <- function(jaspResults,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  metricSummaryTable <- createJaspTable("Forecast Evaluation Metric Table")
  metricSummaryTable$dependOn(c("checkPerformBma",
                                "bmaTestPeriod",
                                'fromR',
                                "bmaTestProp"))
  scoreSummary <- as.data.frame(t(sapply(jaspResults[["predanResults"]][["cvResultsState"]]$object, function(x) colMeans(x$scoringSummary))))
  bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object


  metricSummaryTable$addColumnInfo(name= "model", title = "", type = "string")
  if(options$metricCrps)      metricSummaryTable$addColumnInfo(name = "crps"    , title = "CRPS"                    ,type = "number")
  if(options$metricDss)       metricSummaryTable$addColumnInfo(name = "dss"     , title = "DSS"                     ,type = "number")
  if(options$metricLog)       metricSummaryTable$addColumnInfo(name = "log"     , title = "Log score"               ,type = "number")
  if(options$metricCoverage)  metricSummaryTable$addColumnInfo(name = "coverage", title = "Coverage"                ,type = "number")
  if(options$metricBias)      metricSummaryTable$addColumnInfo(name = "bias"    , title = "Bias"                    ,type = "number")
  if(options$metricPit)       metricSummaryTable$addColumnInfo(name = "pit"     , title = "PIT"                     ,type = "number")
  if(options$metricMae)       metricSummaryTable$addColumnInfo(name = "mae"     , title = "MAE"                     ,type = "number")
  if(options$metricRmse)      metricSummaryTable$addColumnInfo(name = "rmse"    , title = "RMSE"                    ,type = "number")
  if(options$metricR2)        metricSummaryTable$addColumnInfo(name = "r2"      , title = gettextf("R%s", "\u00B2") ,type = "number")


  metricSummaryTable[["model"]]     <-  rownames(scoreSummary)
  metricSummaryTable[["crps"]]      <-  scoreSummary$"crps"
  metricSummaryTable[["dss"]]       <-  scoreSummary$"dss"
  metricSummaryTable[["log"]]       <-  scoreSummary$"log"
  metricSummaryTable[["coverage"]]  <-  scoreSummary$"coverage"
  metricSummaryTable[["bias"]]      <-  scoreSummary$"bias"
  metricSummaryTable[["pit"]]       <-  scoreSummary$"pit"
  metricSummaryTable[["mae"]]       <-  scoreSummary$"mae"
  metricSummaryTable[["rmse"]]      <-  scoreSummary$"rmse"
  metricSummaryTable[["r2"]]        <-  scoreSummary$"rsq"

  if(options$"checkPerformBma" && !is.null(bmaRes)){

    bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object

    scoreSum <- rowMeans(bmaRes$scores,na.rm = T)

    scoreTableList <- list()
    scoreTableList['model']   <- 'BMA'
    if(options$metricCrps)      scoreTableList["crps"]    <- scoreSum["crps"]
    if(options$metricDss)       scoreTableList["dss"]     <- scoreSum["dss"]
    if(options$metricLog)       scoreTableList["log"]     <- scoreSum["log"]
    if(options$metricCoverage)  scoreTableList["coverage"]<- scoreSum["coverage"]
    if(options$metricBias)      scoreTableList["bias"]    <- scoreSum["bias"]
    if(options$metricPit)       scoreTableList["pit"]     <- scoreSum["pit"]
    if(options$metricMae)       scoreTableList["mae"]     <- scoreSum["mae"]
    if(options$metricRmse)      scoreTableList["rmse"]    <- scoreSum["rmse"]
    if(options$metricR2)        scoreTableList["r2"]      <- scoreSum["rsq"]


    metricSummaryTable$addRows(scoreTableList)

  }

  jaspResults[["predanMainContainer"]][["cvContainer"]][["metricTable"]] <- metricSummaryTable


  return()
}



.predanPitPlots <- function(jaspResults,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  if(is.null(jaspResults[["predanMainContainer"]][["cvContainer"]][["pitPlots"]]) &
     length(options$"pitPlots") >0){

    cvRes <- jaspResults[["predanResults"]][["cvResultsState"]]$object

    pitValuesList <- lapply(cvRes, function(x) x$scoringArray[,"pit",])

    pitValues <- stack(lapply(pitValuesList,c))
    colnames(pitValues) <- c("pit_value","model")


    mods <- names(cvRes)
    modsFull <- lapply(cvRes, "[[","modelName")
    plotMods <- mods[which(modsFull %in% options$pitPlots)]


    pitValues <- subset(pitValues,model %in% plotMods)

    nBins <- 10
    width <- 1 / nBins
    plotQuantiles <- seq(0, 1, width)

    xBreaks <- pretty(plotQuantiles)


    nRow <- ceiling(length(unique(pitValues$model))/2 )
    nCol <- ifelse(length(unique(pitValues$model)) ==1,1,2)
    pitPlots <- createJaspPlot(title = "PIT binned density plot",dependencies = c("pitPlots"),width = 360*nCol,height = 360 *nRow )
    p <- ggplot2::ggplot(
      data = pitValues,
      ggplot2::aes(x = pit_value)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(width * density)),
                              breaks = plotQuantiles,
                              fill    = "grey",
                              col     = "black",
                              size    = .7
      ) +
      ggplot2::facet_wrap(
        facets = "model",
        ncol = nCol) +
      ggplot2::xlab("PIT") +
      ggplot2::ylab("Density") +
      ggplot2::scale_x_continuous(breaks = xBreaks)

    p <- p + ggplot2::geom_hline(yintercept = 0.1,linetype = "dashed")


    p <- jaspGraphs::themeJasp(p,
                               bty = list(type = "n", ldwX = .7, lwdY = 1)) +
      ggplot2::theme(plot.margin = ggplot2::margin(2))


    pitPlots$plotObject <- p
    jaspResults[["predanMainContainer"]][["cvContainer"]][["pitPlots"]] <- pitPlots

  }

  return()
}



.predanForecastVerificationResultPlot <- function(jaspResults,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  if(#is.null(jaspResults[["predanMainContainer"]][["cvContainer"]][["cvResPlot"]]) &&
     length(options$"modelsToPlot") >1){


    cvRes <- jaspResults[["predanResults"]][["cvResultsState"]]$object
    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object
    cvPlan <- jaspResults[["predanResults"]][["cvPlanState"]]$object

    predanResults <-jaspResults[["predanResults"]][["predanBounds"]][["object"]]
    upperLimit <- predanResults[["upperLimit"]]
    lowerLimit <- predanResults[["lowerLimit"]]
    plotLimit <- predanResults[["plotLimit"]]

    #maxSlices <- options$modelsToPlotSlices


    mods <- names(cvRes)
    modsFull <- sapply(cvRes, "[","modelName")
    plotMods <- c(mods[which(modsFull %in% options$"modelsToPlot")],"Data")
    slices <- head(dimnames(cvRes[[1]]$realMatrix)[[2]],options$modelsToPlotSlices)

    cvResPlot <- createJaspPlot(title = "Prediction Plots",width = 720,height = 180*length(slices))
    cvResPlot$dependOn(c("modelsToPlot","checkPerformBma","modelsToPlotSlices"))

    mods <- names(cvRes)
    modsFull <- sapply(cvRes, "[","modelName")



    ##TODO choice for equal or unequal t diff
    spread_equal <- T


    t_var <- ifelse(spread_equal,"tt",time)
    sumPred <- lapply(cvRes,function(x) x$predSummary)
    dataPlot <- lapply(cvPlan,
                            function(x) data.frame( tt = c(x$analysis,x$assessment),
                                                       type = rep(c("Analysis","Assessment"),
                                                                  c(length(x$analysis),length(x$assessment)))))


    realMatrix <- cvRes[[1]]$realMatrix
    dataPlot <- dplyr::bind_rows(.id = "slice",dataPlot)
    realData <- as.data.frame.table(realMatrix)

    dataPlot$value <- NA
    dataPlot$value[dataPlot$type == "Assessment"] <- realData$Freq
    dataPlot$value[dataPlot$type == "Analysis"] <- dataEng$y[dataPlot$tt[dataPlot$type == "Analysis"]]
    dataPlot$time <- dataEng$time[dataPlot$tt]


    predSummArray <-  sapply(cvRes,FUN =  function(x) x$predSummary,simplify = "array",USE.NAMES = T)

    dimnames(predSummArray)[3] <- list(dimnames(realMatrix)[[2]])


    pred <- cbind(as.data.frame.table(predSummArray[,1,,],responseName = "value"),
                  upr = as.data.frame.table(predSummArray[,2,,])$Freq,
                  lwr = as.data.frame.table(predSummArray[,3,,])$Freq)


    colnames(pred)[1:3] <- c("tt","slice","type")
    pred$tt <- as.numeric(dataPlot$tt[dataPlot$type == "Assessment"])
    pred$time <- dataPlot$time[dataPlot$type == "Assessment"]

    dataPlot$type <- "Data"
    dataPlot <- dplyr::bind_rows(dataPlot,pred)

    #BMA

    if(!is.null(jaspResults[["predanResults"]][["bmaResState"]]) && "BMA" %in% options$"modelsToPlot"){

      bma <- jaspResults[["predanResults"]][["bmaResState"]]$object

      bmaRes <- bma$res
      bmaDat <- sapply(X = 1:(length(bmaRes)-1), function(x) bmaRes[[x]]@predTest[,1,])

    bmaPred  <- as.data.frame.table(bmaDat)
    bmaSlices <- (unique(dataPlot$slice))
    if(all(is.na(bma$scores[,1])))
      bmaSlices <- bmaSlices[-1]

    ttBma <- unlist(lapply(cvPlan,function(x) tail(x$assessment,nrow(bmaPred)/length(bmaSlices))))

    bmaData <- subset(dataPlot,type == plotMods[1] & slice %in% bmaSlices &  tt %in% ttBma)
    bmaData$value <- bmaPred$Freq
    bmaData$slice <-  rep(bmaSlices,each = nrow(bmaPred)/length(bmaSlices))
    bmaData[,c("upr","lwr")] <- NA
    bmaData$type <- "BMA"
    #View(bmaData)
    dataPlot <- dplyr::bind_rows(dataPlot,bmaData)
    plotMods <- c(plotMods,"BMA")

    }
    #View(dataPlot)
    dataPlot <- subset(dataPlot,slice %in% slices & (type %in% plotMods | is.na(type)))
    xBreaks <- pretty(dataPlot[[t_var]])
    yBreaks <- pretty(dataPlot$value)

    #reorder so Data is first factor
    dataPlot$type <- factor(dataPlot$type,ordered = T,
                            levels = c("Data",unique(dataPlot$type)[!grepl("Data",unique(dataPlot$type))]))

    #order slices properly so plot shows correctl
    slicesLevels <- unique(dataPlot$slice)
    slicesLevels <- slicesLevels[order(nchar(slicesLevels))]
    dataPlot$slice <- factor(dataPlot$slice,levels = slicesLevels)

    #slicesInclude <- ifelse(options$resampleSliceStart == 'head',head(slicesLevels,maxSlices),tail(slicesLevels,maxSlices))
    #dataPlot <- dataPlot[dataPlot$slice %in% slicesInclude,]

    p <- ggplot2::ggplot(dataPlot,ggplot2::aes_string(t_var,"value",color = "type")) + ggplot2::geom_line() +
      ggplot2::labs(color = "Type") +
      ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                      plotLimit[[1]])) +
      ggplot2::facet_wrap(facets  = "slice",ncol = 1) +
      #ggplot2::coord_cartesian(ylim = range(yBreaks)) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
      ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid,
                     panel.background = ggplot2::element_rect(fill = "white"),
                     legend.position = "bottom",legend.title = ) +
      jaspGraphs::scale_JASPcolor_discrete("viridis") +
      ggplot2::ylab('Value') + ggplot2::xlab('Time') +
      ggplot2::geom_hline(na.rm = T,yintercept = upperLimit,linetype="dashed",color="darkred") +
      ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred")



    cvResPlot$plotObject <- p
    jaspResults[["predanMainContainer"]][["cvContainer"]][["cvResPlot"]] <- cvResPlot
  }

  return()
}

.ebmaHelper <- function(predSumArray,
                       realArray,
                       methodBMA = c("EM","gibbs"),
                       testMethod = c("next","in"),
                       inPercent = 0.3,retrain = T){

  testMethod <- match.arg(testMethod)
  if(testMethod == "in"){
    nTrain <- 1:round(nrow(realArray)*(1-inPercent),0)
    nTest <- tail(nTrain+1,1):nrow(realArray)
    nSlice <- 0
  } else{
    nTrain <- nTest <- 1:nrow(realArray)
    nSlice <- 1
  }

  #predBmaArray <-
  resList <- list()
  startProgressbar(dim(realArray)[2],'Running BMA')
  resList <- lapply(X = 1:dim(realArray)[2],FUN = function(i){

    iTest <- i + nSlice
    if(i != dim(realArray)[2] | testMethod == "in"){
      bmaData <- EBMAforecast::makeForecastData(.predCalibration = predSumArray[nTrain,1,i,],
                                                .outcomeCalibration  = realArray[nTrain,i,1],
                                                .predTest = predSumArray[nTest,1,iTest,],
                                                .outcomeTest = realArray[nTest,iTest,1],
                                                .modelNames = dimnames(realArray)[[3]])
    } else {
      bmaData <- EBMAforecast::makeForecastData(.predCalibration = predSumArray[nTrain,1,i,],
                                                .outcomeCalibration  = realArray[nTrain,i,1],
                                                .modelNames = dimnames(realArray)[[3]])
    }
    progressbarTick()
    bmaRes <- EBMAforecast::calibrateEnsemble(bmaData,model = "normal",method = methodBMA,tol	= 0.05,useModelParams =F)
  })

  dimsMetric <- ifelse(testMethod == "next", length(resList)-1,length(resList))

  if(methodBMA == "gibbs"){
    scores <- sapply(X = 1:dimsMetric,
                              function(x) colMeans(.scorePred(resList[[x]]@predTest[,1,],
                                                             resList[[x]]@outcomeTest,
                                                             SD = sqrt(resList[[x]]@variance))))
  } else{
    scores <- sapply(X = 1:dimsMetric, function(x) colMeans(.scorePred(resList[[x]]@predTest[,1,],
                                                                                          resList[[x]]@outcomeTest,
                                                                                          SD =sqrt(resList[[x]]@variance))))
  }

  if(testMethod == "next")
    scores <- cbind(NA,scores)

  weightMatrix <- sapply(resList,function(x) x@modelWeights)
  return(list(res = resList,scores = scores,weightMatrix = weightMatrix))
}

.predanBMAHelperResults <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]])) return()

  if(is.null(jaspResults[["predanResults"]][["bmaResState"]]) && options$checkPerformBma){

    bmaResState <- createJaspState()
    bmaResState$dependOn(c(.modelDependencies(),
                         .bmaResultsDependencies(),"modelsToPlot","checkPerformBma"))

    cvRes <- jaspResults[["predanResults"]][["cvResultsState"]]$object

    predSumArray <-   sapply(cvRes,FUN =  function(x) x$predSummary,simplify = "array")
    realArray <- sapply(cvRes, function(x) x$realMatrix,simplify = "array")


    bmaMethod <- switch (options$"bmaMethod",
       "bmaMethodEm" = "EM",
       "bmaMethodGibbs" = "gibbs"
    )

    bmaTestMethod <- switch (options$"bmaTestPeriod",
       "bmaTestNextSlice" = "next",
       "bmaSameSlice" = "in"
    )

    bmaRes <- .ebmaHelper(predSumArray = predSumArray,
                          realArray = realArray,
                          methodBMA = bmaMethod,
                          testMethod = bmaTestMethod,inPercent = options$bmaTestProp)

    bmaResState$object <- bmaRes

    #.predanMetricTable(jaspResults = jaspResults,options = options,ready = ready)

    jaspResults[["predanResults"]][["bmaResState"]] <- bmaResState
    jaspResults[["plottableModelsQml"]] <- createJaspQmlSource(sourceID="plottableModelsQml", value= c(options$selectedModels,"BMA"))

    #if("BMA" %in% options$"modelsToPlot")
    #  .predanForecastVerificationResultPlot(jaspResults,options,ready)

  }
  return()
}


.predanBMAWeightsTable <-function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["bmaResState"]])) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["bmaWeightsTable"]]) &&
     options$"bmaWeightsTable"){
    bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object

    bmaWeightsTable <- createJaspTable(title = "BMA - Model Weights")
    bmaWeightsTable$dependOn(c("bmaWeightsTable","bmaWeightsTablePerSlice","modelsToPlot","checkPerformBma"))
    weightMatrix <- bmaRes$weightMatrix
    bmaWeightsTable$addColumnInfo(name="model",  title="Model", type="string")
    if(options$"bmaWeightsTablePerSlice") {
      start <- ifelse(options$bmaTestPeriod == "bmaTestNextSlice",1,1)
      for (i in start:ncol(weightMatrix)) {
        bmaWeightsTable$addColumnInfo(name=paste0("slice",i),  title=paste0("Slice ",i), type="number")
      }
    } else{
      bmaWeightsTable$addColumnInfo(name="weights",  title= "Weights", type="number")
    }

    bmaWeightsTable[["model"]] <- rownames(weightMatrix)

    if(options$"bmaWeightsTablePerSlice") {

      start <- ifelse(options$bmaTestPeriod == "bmaTestNextSlice",1,1)

      for (i in start:ncol(weightMatrix)) {
        bmaWeightsTable[[paste0("slice",i)]] <- weightMatrix[,i]
      }

    } else{
      bmaWeightsTable[["weights"]] <- rowMeans(weightMatrix)
    }

    jaspResults[["predanMainContainer"]][["predanBMAContainer"]][["bmaWeightsTable"]] <- bmaWeightsTable

  }

  return()
}



## future predictions


#### helper functions
.makeEmptyFutureFrame <- function(dataEngFuture,dataEng,options){

  futureFrame <- dataEngFuture

  return(futureFrame)
}

# function computes credible interval per prediction model and computes BMA predictions
.adjustFuturePredictions <- function(predList,bmaRes = NULL,futureFrame,upperLimit,lowerLimit,options){



  predSummary <- lapply(X = predList,
                        function(x)
                          data.frame(median = apply(x$pred$dist,1,median,na.rm = T),
                                     lowerCI = apply(x$pred$dist,1,quantile,probs= 0.025,na.rm = T),
                                     higherCI= apply(x$pred$dist,1,quantile,probs= 0.975,na.rm = T),
                                     lowerLimitProb = apply(x$pred$dist, 1, quantInv,lowerLimit),
                                     upperLimitPrib = 1 - apply(x$pred$dist, 1, quantInv,upperLimit)

                          ))
  names(predSummary)
  predListAdjusted <- list()


  if(!is.null(bmaRes)){
    # select most recent bma result
    #bmaRes <- bmaRes$res[[length(bmaRes)]]
    bmaRes <- tail(bmaRes$res,1)

    #apply model weighst and optional bias adjustment to credible interval and mean prediction
    for(i in 1:5){
      d <- as.matrix(as.data.frame(sapply(predSummary,'[',i,simplify = "matrix")))
      colnames(d) <- bmaRes@modelNames
      predListAdjusted[[i]] <- EBMAforecast::EBMApredict(EBMAmodel = bmaRes,Predictions = d)@predTest[,,1]

    }

  } else{
    predListAdjusted <- lapply(1:5,function(i){
      d <- as.matrix(as.data.frame(sapply(predSummary,'[',i,simplify = "matrix")))
      colnames(d) <- names(predList)
      d
    })
  }
  #names(predListAdjusted) <- c("mean","lwr","upr")
  names(predListAdjusted) <- "x"

  predictionsCombined <- do.call(cbind,lapply(predListAdjusted,as.data.frame.table))[,c(1:3,6,9,12,15)]
  colnames(predictionsCombined) <- c("tt","model","mean","lwr","upr","lwrProb","uprProb")
  predictionsCombined$time <- futureFrame$time


  predictionsCombined$tt <- futureFrame$tt

  predictionsCombined$model <- as.character(predictionsCombined$model)
  predictionsCombined$model[predictionsCombined$model == "EBMA"] <- "BMA"

  predictionsCombined$pred <- 1

  return(predictionsCombined)
}



.predanFuturePredictionResults <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["cvResultsState"]]) || !(options$selectedFuturePredictionModel > 0) ) return()

  if(is.null(jaspResults[["predanResults"]][["futurePredState"]]) &&
             (options$"futurePredictionPoints" > 0 ||
              !is.null(jaspResults[["predanResults"]][["featureEngStateFuture"]]))){


    futurePredState <- createJaspState()
    futurePredState$dependOn(c(.modelDependencies(),
                               "selectedModels",
                               "futurePredPredictionHorizon",
                               "futurePredictionDays",
                               "futurePredictionPoints",
                               "futurePredTrainingPeriod",
                               "futurePredTrainingPoints"))

    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object
    dataEngFuture <- jaspResults[["predanResults"]][["featureEngStateFuture"]]$object

    #save maximum number of rows before training data for training data is reduced.
    nrRows <- nrow(dataEng)
    if(options$futurePredTrainingPeriod == "last") dataEng <- tail(dataEng,options$futurePredTrainingPoints)

    futureFrame <- .makeEmptyFutureFrame(dataEngFuture,dataEng = dataEng,options)



    modelList <- .createModelListHelper(dataEng,unlist(options$selectedModels))


    #error handling when covariates present but prediction points longer than actual
    #length(options$covariates) > 0 && length(options$factors) > 0
    if(options$futurePredictionPoints > nrow(futureFrame)){
      errorPlot <- createJaspPlot()
      errorPlot$setError(gettext("Cannot compute forecast. Larger forecast horizon requested than indicated by 'Include in Training' variable. Reduce forecast horizon or change training indicator"))
      jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["errorPlot"]] <- errorPlot

      return()
    } else{
      jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["errorPlot"]] <- NULL
    }

    futureFrame <- head(futureFrame,options$futurePredictionPoints)
    futureFrame$tt <- (nrRows+1):(nrRows+options$futurePredictionPoints)


    predList <- list()
    for (i in 1:length(modelList)) {
      predList[[i]] <- .predAnModelFit(trainData = dataEng,
                                       testData = futureFrame,
                                       predictFuture = T,
                                       method = modelList[[i]]$model,
                                       formula = modelList[[i]]$modelFormula,
                                       model_args =list())
    }
    names(predList) <- paste0(sapply(modelList,'[', "model"),1:length(modelList))


    futurePredState$object <- list(predList = predList,
                                   futureFrame = futureFrame)


    jaspResults[["predanResults"]][["futurePredState"]] <- futurePredState

  }


  return()

}

.predanFuturePredictionPlot <- function(jaspResults,dataset,options,ready){
  if(!ready || is.null(jaspResults[["predanResults"]][["futurePredState"]]) || !(options$selectedFuturePredictionModel > 0)) return()

  if(is.null(jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["futurePredPlot"]])
     && options$"checkFuturePredictionPlot"){

    futurePredPlot <- createJaspPlot(title = "Future prediction plot", height = 480, width = 720,position = 2)
    futurePredPlot$dependOn(c(.modelDependencies(),
                              .boundDependencies(),
                              "xAxisLimit",
                              "futurePredictionPoints",
                              "checkFuturePredictionPlot",
                              "futurePredSpreadPointsEqually",
                              "selectedFuturePredictionModel",
                              "selectedModels",
                              "futurePredPredictionHorizon",
                              "futurePredictionDays",
                              "futurePredictionPoints",
                              "futurePredTrainingPeriod",
                              "futurePredTrainingPoints",
                              "futurePredThreshold",
                              "futurePredReportingCheck"))


    predanResults <-jaspResults[["predanResults"]][["predanBounds"]][["object"]]
    upperLimit <- predanResults[["upperLimit"]]
    lowerLimit <- predanResults[["lowerLimit"]]
    plotLimit <- predanResults[["plotLimit"]]


    dataEng <- jaspResults[["predanResults"]][["featureEngState"]]$object
    futurePredState <- jaspResults[["predanResults"]][["futurePredState"]]$object
    bmaRes <- jaspResults[["predanResults"]][["bmaResState"]]$object

    predList <- futurePredState$predList
    futureFrame <- futurePredState$futureFrame

    predictionsCombined <- .adjustFuturePredictions(predList,bmaRes,futureFrame,upperLimit,lowerLimit,options)
    #jaspResults[["predanResults"]][["futurePredState"]]$object$predictionsCombined <- predictionsCombined

    dataOld <- cbind(dataEng[c('y', 'time')],upr= NA, lwr= NA,tt = NA,model = 'Actual',lwrProb = 0,uprProb = 0,pred = 0)
    dataOld$tt <- 1:nrow(dataEng)



    colnames(dataOld)[1] <- 'mean'
    dataOld <- tail(dataOld,500)
    predictionsCombined <- rbind(dataOld,predictionsCombined)

    modelList <- .createModelListHelper(dataEng,options$selectedModels)


    modelNames <-  paste0(sapply(modelList,'[', "model"),1:length(modelList))
    names(modelNames) <- names(modelList)

    selectedModPlot <- modelNames[names(modelNames) ==options$"selectedFuturePredictionModel"]
    if("BMA" %in% unique(predictionsCombined$model) & options$"selectedFuturePredictionModel" == "BMA")
      selectedModPlot <- "BMA"

    jaspResults[["predanResults"]][["futurePredState"]]$object$predictionsCombined <- predictionsCombined
    predictionsCombined <- subset(predictionsCombined, model %in% c(selectedModPlot,"Actual"))

    t_var <- ifelse(options$futurePredSpreadPointsEqually,"tt","time")





    xBreaks <- pretty(unique(predictionsCombined[[t_var]]))
    yBreaks <- jaspGraphs::getPrettyAxisBreaks(plotLimit)





    p <- ggplot2::ggplot(predictionsCombined,ggplot2::aes_string(t_var,"mean")) +
      ggplot2::geom_line(color = "black") +
      jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() + ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     legend.position = "bottom") +
      ggplot2::scale_y_continuous(name = "Y",breaks = yBreaks#,limits = range(yBreaks)
                                  ) +
      ggplot2::labs(color = "Type") +
      ggplot2::geom_hline(na.rm = T,yintercept = upperLimit,linetype="dashed",color="darkred") +
      ggplot2::geom_hline(yintercept = lowerLimit,linetype="dashed",color="darkred") +
      ggplot2::geom_vline(xintercept = max(dataOld[[t_var]]),linetype="dashed")

    if(options$xAxisLimit == "controlBounds")
      p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                        plotLimit[[1]]))
    else{
      plotLimit <- rev(ggplot2::layer_scales(p)$y$get_limits())
      p <- p + ggplot2::coord_cartesian(ylim=c(plotLimit[[2]],
                                               plotLimit[[1]]))
    }

    p <- p + ggplot2::geom_ribbon(ggplot2::aes_string(ymin="lwr",ymax="upr"),alpha=0.5,color = NA, fill = "blue") +
      ggplot2::scale_x_continuous(name = "Time",breaks = xBreaks,limits = range(xBreaks))

    outOfBoundMin <- predictionsCombined[[t_var]][min(which(predictionsCombined$lwrProb > options$futurePredThreshold |
                                                              predictionsCombined$uprProb > options$futurePredThreshold))]

    if(!is.na(outOfBoundMin) & options$futurePredReportingCheck)
      p <- p + ggplot2::geom_vline(xintercept = outOfBoundMin,linetype="dashed",color="darkred")

    futurePredPlot$plotObject <- p



    if(options$futurePredReportingCheck){


      # compute boundary breach
      warningIndicator <- any(predictionsCombined[predictionsCombined$pred==1, c("uprProb","lwrProb")] > options$futurePredThreshold)


      outBoundMax <- round(max(predictionsCombined[predictionsCombined$pred==1, c("uprProb","lwrProb")]),3)

      warningText <- ifelse(warningIndicator,
                            gettextf(paste0("Warning! The process is predicted to cross the out-of-control probability threshold for the first time at time point: ",outOfBoundMin)),
                            gettextf(paste0("No warning. The process is not predicted to cross the out-of-control probability threshold. The highest out-of-bound probability is: ",outBoundMax,"percent.")))


      jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["futurePredReport"]] <- createJaspReport(
        text =  warningText,
        report = warningIndicator,
        dependencies = c("futurePredThreshold","futurePredReportingCheck"),
        position = 1)

    }

    jaspResults[["predanMainContainer"]][["predanFuturePredContainer"]][["futurePredPlot"]] <- futurePredPlot






  }

  return()
}
.predanFuturePredictionTable <- function(jaspResults,dataset,options,ready){

}



