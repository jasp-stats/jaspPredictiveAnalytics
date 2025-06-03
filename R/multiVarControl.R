#
# Copyright (C) 2022 University of Amsterdam
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

multiVarControl <- function(jaspResults, dataset, options) {
  ready <- length(options[["variables"]]) > 0

  if(ready){
    dataset <- .mVarConReadData(dataset,options)
    #.mVarConCheckErrors(dataset,options)
  }
  .mVarContContainerHelper(jaspResults,ready,dataset,options)
  .mVarContSummaryTable(jaspResults,ready,dataset,options)
  .mVarContSummaryPlot(jaspResults,ready,dataset,options)

  .mVarContMultiBinomialHelper(jaspResults,ready,dataset,options)

  .mVarContMultiBinomialPlot(jaspResults,options,ready)
  .mVarContMultiBinomialPredictionPlot(jaspResults,options,ready)
  .mVarContMultiBinomialPredictionTables(jaspResults,options,ready)
}

.mVarConReadData <- function(dataset,options){
  #if(!is.null(dataset))
   # return(dataset)

  vars <- unlist(options$variables)
  dataset <- .readDataSetToEnd(columns.as.numeric = vars)

  dataset$time <- 1:nrow(dataset)

  dataset <- tail(dataset,options$previousDataPoints)
  return(dataset)
}

.mVarConCheckErrors <- function(dataset,options){
  if(!is.data.frame(dataset) || any(colnames(dataset) %in% c("X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24", "X25")))
    stop(gettext("Column input is wrong"))
  return()
}

.mVarContContainerHelper <- function(jaspResults,ready,dataset,options){
  #if(!ready) return()

  if(is.null(jaspResults[["mVarContMainContainer"]])){
    mVarContMainContainer <- createJaspContainer(position = 1)
    mVarContMainContainer$dependOn(c("variables","previousDataPoints"))
    jaspResults[["mVarContMainContainer"]] <- mVarContMainContainer
  }
 return()
}

##### Dependency functions

.multiVarModelDependencies <- function(){
  return(c("variables",
           "previousDataPoints",
           "multiBinWindow",
           "multiBinomDraws"
           )
  )
}


.multiVarPredictionDependencies <- function(){
  return(c("predictionHorizon"))
}








##### Helper functions

.computeBoundsHelper <- function(data,options){

    controlBounds <- data.frame( mean = c(66,8,2.5,13.15,13.15,13.15,13.15,7,7),
                               bound= c(.43,.46,.46,.16,.16,.18,.18,.33,.33))


    rownames(controlBounds) <- encodeColNames(c("X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24", "X25"))

    cols <- intersect(rownames(controlBounds),colnames(data))

    dataCoded <- as.data.frame(sapply(X=cols, function(x) {
      as.numeric(data[x] < controlBounds[x,"mean"] - controlBounds[x,"bound"] |
                   data[x] > controlBounds[x,"mean"] + controlBounds[x,"bound"] )
    },USE.NAMES = T))
    colnames(dataCoded) <- cols


  return(dataCoded)
}




# aggregate binomial data by a window
.aggregateBinomialData <- function(data,dataWindow){
  data$all <- rowSums(data)
  cols <- ncol(data) - 1 # subtract all column
  nS <- round(nrow(data)/dataWindow,0)
  x = 1:nrow(data)
  s <- split(x, sort(x%%nS))
  dataCombined <- as.data.frame(t(sapply(X = 1:nS,function(x) colSums(data[s[[x]],]))))
  dataCombined$u <- lengths(s)*cols
  return(dataCombined)
}


.mVarContSummaryTable <- function(jaspResults,ready,dataset,options){
  if(!is.null(jaspResults[["mVarContMainContainer"]][["overallSummaryTable"]])) return()


  overallSummaryTable <- createJaspTable(title = "Control Summary Table",position = 1)

  overallSummaryTable$dependOn(c("variables","overallControlSummaryTable","transposeOverallTable","orderTableByOutBound"))

  overallSummaryTable$addColumnInfo(name = "variable", title = "Variable", type= "string")
  overallSummaryTable$addColumnInfo(name = "nData", title = "Valid", type = "integer")
  overallSummaryTable$addColumnInfo(name = "missing", title = "Missing", type = "integer")
  overallSummaryTable$addColumnInfo(name = "outBoundNum", title = "Out-of-bound - Number", type = "integer")
  overallSummaryTable$addColumnInfo(name = "outBoundPerc", title = "Out-of-bound - Proportion", type = "number", format = "dp:3")

  if(options$transposeOverallTable)
    overallSummaryTable$transpose  <- T

  if(ready && options$overallControlSummaryTable)
    overallSummaryTable <- .mVarContSummaryTableFill(overallSummaryTable,dataset,options)

  jaspResults[["mVarContMainContainer"]][["overallSummaryTable"]] <- overallSummaryTable
  return()
}


.mVarContSummaryTableFill <- function(overallSummaryTable,dataset,options,ready){


  dataCoded <- .computeBoundsHelper(dataset,options)


  #View(dataCoded)
  dataRes <- data.frame()


  dataResAll <- data.frame(
    variable = "All",
    nData = sum(!is.na(dataCoded),na.rm = T),
    missing = sum(is.na(dataCoded),na.rm = T),
    outBoundNum = sum(dataCoded,na.rm = T),
    outBoundPerc = sum(dataCoded,na.rm = T)/sum(!is.na(dataCoded),na.rm = T)
  )


 # stop(gettext(paste0("Models didn't work. Instead of prediction matrix we got:",one_step_pred)))


  for(var in colnames(dataCoded)){
    dataRes <- rbind(dataRes,
      c(variable = var,
           nData = sum(!is.na(dataCoded[var]),na.rm = T),
           missing = sum(is.na(dataCoded[var]),na.rm = T),
           outBoundNum = sum(dataCoded[var],na.rm = T),
           outBoundPerc = sum(dataCoded[var],na.rm = T)/sum(!is.na(dataCoded[var]),na.rm = T)
      )
    )
  }
  colnames(dataRes) <- c("variable",
                         "nData",
                         "missing",
                         "outBoundNum",
                         "outBoundPerc")

  if(options$orderTableByOutBound)
    dataRes <- dataRes[order(dataRes$outBoundPerc,decreasing = T),]


  dataRes <- rbind(dataResAll,dataRes)

  dataRes[-1] <- apply(dataRes[-1],2,as.numeric)

  for(col in colnames(dataRes)){
    overallSummaryTable$addColumns( col = dataRes[col])
  }
  print("table created")

  return(overallSummaryTable)
}


.mVarContSummaryPlot <- function(jaspResults,ready,dataset,options){
  if(!is.null(jaspResults[["mVarContMainContainer"]][["overallSummaryPlotContainer"]]) || !ready) return()

  overallSummaryPlotContainer <- createJaspContainer(title = "Control Plots",position = 2)

  overallSummaryPlotContainer$dependOn(c("outBoundOverallPlotCheck",
                                         "outBoundOverallPlotMetricChoice",
                                         "outBoundOverallPlotLineType",
                                         "outBoundOverallPlotJitterCheck"))

  if(options$outBoundOverallPlotCheck)
    .mVarContSummaryPlotFill(overallSummaryPlotContainer,dataset,options)

  #if(options$summaryPlotIndividualVars)
  #  .mVarContSummaryPlotFillSingle(overallSummaryPlotContainer,dataset,options)

  jaspResults[["mVarContMainContainer"]][["overallSummaryPlotContainer"]] <- overallSummaryPlotContainer

  return()

}


.mVarContSummaryPlotFill <- function(overallSummaryPlotContainer,dataset,options){

  overallPlot <- createJaspPlot("Overall Control Plot", height = 480, width = 720,position = 2)


  dataCoded <- .computeBoundsHelper(dataset,options)
  #stop(gettext(dim(dataCoded)))
  dataCoded$all <- rowSums(dataCoded,na.rm = T)
  if(options$outBoundOverallPlotMetricChoice == "proportion"){
    dataCoded$all <- dataCoded$all/length(options$variables)
    yTitle <- "Out-of-bound Percent"
    yLim <- c(0,1)
  } else {
    yTitle <- "Out-of-bound Number"
    yLim <- c(0,length(options$variables))
  }
  dataCoded$time <- dataset$time

  xBreaks <- pretty(dataCoded$time)
  yBreaks <- pretty(yLim)


  p <- ggplot2::ggplot(data = dataCoded, ggplot2::aes(y = all,x = time)) + #ggplot2::geom_point() +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = c(min(dataCoded$time),max(dataCoded$time))) +
    ggplot2::ylab(yTitle) +
    ggplot2::scale_y_continuous("Errors",breaks = yBreaks,limits = yLim) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))

  if(options$outBoundOverallPlotLineType %in% c("line","both"))
    p <- p + ggplot2::geom_line(size=0.7)
  if(options$outBoundOverallPlotLineType %in% c("points","both"))
    p <- p + ggplot2::geom_point(size=0.5)

  if(options$outBoundOverallPlotJitterCheck)
    p <- p + ggplot2::geom_jitter(height = 0.5,width = 0,size=0.5)

  overallPlot$plotObject <- p

  overallSummaryPlotContainer[["overallPlot"]] <- overallPlot

  return()
}

.mVarContSummaryPlotFillSingle <- function(overallSummaryPlotContainer,dataset,options){
  #placeholder for individual plots
  return()
}


.mVarContMultiBinomialHelper <- function(jaspResults,ready,dataset,options){
  if(!ready) return()

  if(is.null(jaspResults[["binomialResults"]])){

    binomialResults <- createJaspState()

    binomialResults$dependOn(.multiVarModelDependencies())
    dataCoded <- .computeBoundsHelper(dataset,options)

    dataAggregated <- .aggregateBinomialData(dataCoded,options$multiBinWindow)


    binomialResults$object <- .multiVarBinResultsHelper(jaspResults,dataAggregated,options)
    jaspResults[["binomialResults"]] <- binomialResults
  }


  if(options$predictionHorizon > 0 && is.null(jaspResults[["binomialPredictions"]])){

    binomialPredictions <- createJaspState(dependencies =c(.multiVarPredictionDependencies(),.multiVarModelDependencies()))
    binomialPredictions$object <- .multiVarBinPredictionsHelper(jaspResults,options)
    jaspResults[["binomialPredictions"]] <- binomialPredictions
  }

  #jaspResults[["mVarContMainContainer"]][["multiVarBinomialContainer"]] <- multiVarBinomialContainer

  return()
}

.multiVarBinResultsHelper <-function(jaspResults,dataAggregated,options){

  mod <- bssm::bsm_ng(y=dataAggregated$all,
                sd_level = bssm::halfnormal(0.5,2),
                sd_slope = bssm::halfnormal(0.5,2),
                u=dataAggregated$u,
                distribution = "binomial")

  startProgressbar(1,label = "Running binomial state space model")
  sample <- bssm::run_mcmc(mod, iter = options$multiBinomDraws,mcmc_type = "approx")


  sampleSummary <- subset(as.data.frame(sample,variable = "states"),variable == "level")
  sampleSummary$value <- plogis(sampleSummary$value)
  binomialSummary <- do.call(data.frame,
                             aggregate( value ~ time,
                                        data = sampleSummary,
                                        FUN = function(x) c(mean = mean(x),
                                                            lowerCI = quantile(x,probs= 0.025),
                                                            higherCI= quantile(x,probs= 0.975),
                                                            number = mean(x)*max(dataAggregated$u),
                                                            lowerNumber = quantile(x,probs= 0.025)*max(dataAggregated$u),
                                                            higherNumber = quantile(x,probs= 0.975)*max(dataAggregated$u))
                             ))
  colnames(binomialSummary) <- c("time","mean","lowerCI","higherCI","number","lowerNumber","higherNumber")

  binomialSummary <- binomialSummary[1:nrow(binomialSummary)-1,]
  binomialSummary$actual <- dataAggregated$all/dataAggregated$u
  progressbarTick()

  return(list(model = mod, sample=sample, dat = dataAggregated,binomialSummary = binomialSummary))
}


.multiVarBinPredictionsHelper <- function(jaspResults,options){


  binomialResults <-  jaspResults[["binomialResults"]]$object
  mod <- binomialResults$mod
  sample <- binomialResults$sample
  dataAggregated <- binomialResults$dat

  modFuture <- mod
  modFuture$y <- rep(NA,options$predictionHorizon)

  futurePredictions <- predict(sample,modFuture, type = "mean",
                         nsim = options$multiBinomDraws/2,future = T)

  #sampleSummary <- subset(as.data.frame(sample,variable = "states"),variable == "level")

  #sampleSummary$value <- plogis(sampleSummary$value)
  futureSummary <- do.call(data.frame,
                             aggregate( value ~ time,
                                        data = futurePredictions,
                                        FUN = function(x) c(mean = mean(x),
                                                            lowerCI = quantile(x,probs= 0.025),
                                                            higherCI= quantile(x,probs= 0.975),
                                                            number = mean(x)*max(dataAggregated$u),
                                                            lowerNumber = quantile(x,probs= 0.025)*max(dataAggregated$u),
                                                            higherNumber = quantile(x,probs= 0.975)*max(dataAggregated$u))
                             ))

  colnames(futureSummary) <- c("time","mean","lowerCI","higherCI","number","lowerNumber","higherNumber")
  futureSummary$actual = NA

  futureSummary$time <- futureSummary$time + nrow(dataAggregated)

  return(list(futurePredictions= futurePredictions,futureSummary = futureSummary))

}




.mVarContMultiBinomialPlot <- function(jaspResults,options,ready){
  if(!ready || !options$multiBinaryCheckPlot)  return()
  titleSub <- ifelse(options$multiBinWindow > 1,paste(" - summary window:",options$multiBinWindow),"")
  multiBinomialPlot <- createJaspPlot(title =paste0("Estimated Multivariate Proportion",titleSub), height = 480, width = 720,
                                        dependencies = c("predictionTimePlot"),position = 4)


  binomialResults <-  jaspResults[["binomialResults"]]$object
  sample <- binomialResults$sample
  dataAggregated <- binomialResults$dat
  binomialSummary <- binomialResults$binomialSummary

  limitReached <- binomialSummary$mean > options$estimatedLimit

  jaspResults[["mVarContMainContainer"]][["estimationHtml"]] <- createJaspHtml(
    text = ifelse(any(limitReached) & options$estimatedLimit > 0,
                  gettextf('
                           <p style="color:tomato;"><b>This is a warning!</b></p>
                           Error proportion limit of %1$.2f is crossed for the first time at data point %2$i in the estimation period. At this point on average %3$.2f data points are estimates to be out of control with an lower limit of %4$.2f and an upper limit of %5$.2f </p>
                           ',options$estimatedLimit,
                           binomialSummary$time[which(limitReached)[1]],
                           binomialSummary$number[which(limitReached)[1]],
                           binomialSummary$lowerNumber[which(limitReached)[1]],
                           binomialSummary$higherNumber[which(limitReached)[1]]),
                  gettextf('<p ">No warning. The limit of %1$.2f is not crossed during the estimation period.</p>',options$estimatedLimit)
    ),position = 3
  )


  xBreaks <- pretty(binomialSummary$time)
  yBreaks <- pretty(c(0,1))
  p <- ggplot2::ggplot(binomialSummary,ggplot2::aes(x = time, y = mean)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(y=actual),size=0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lowerCI, ymax = higherCI),
                fill = "blue", alpha = 0.25) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = c(min(binomialSummary$time),max(binomialSummary$time))) +
    #ggplot2::ylab(yTitle) +
    ggplot2::scale_y_continuous("Proportion",breaks = yBreaks,limits = c(0,1)) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1))

  if(options$estimatedLimit > 0)
    p <- p + ggplot2::geom_hline(yintercept = options$estimatedLimit,linetype="dashed",color="darkred")

  multiBinomialPlot$plotObject <- p

  jaspResults[["mVarContMainContainer"]][["multiBinomialPlot"]] <- multiBinomialPlot

  return()
}

.mVarContMultiBinomialPredictionPlot <- function(jaspResults,options,ready){
  if( !ready || !options$predictionTimePlot || !is.null(jaspResults[["mVarContMainContainer"]][["multiBinomialPredictionPlot"]])) return()


  predictionPlot <- createJaspPlot(title ="Predicted Proportion", height = 480, width = 720,position = 6)
  predictionPlot$dependOn(.multiVarPredictionDependencies())
  binomialSummary <- jaspResults[["binomialResults"]]$object$binomialSummary
  futureSummary <- jaspResults[["binomialPredictions"]]$object$futureSummary

  #predictionPlot$setError(gettextf(paste0(colnames(binomialSummary),"lol",colnames(futureSummary))))
  combinedSummary <- rbind(binomialSummary,futureSummary)
  xBreaks <- pretty(combinedSummary$time)
  yBreaks <- pretty(c(0,1))

  predLimitReached <- futureSummary$mean > options$predictionLimit

  jaspResults[["mVarContMainContainer"]][["predictionHtml"]] <- createJaspHtml(
    text = ifelse(any(predLimitReached) & options$predictionLimit > 0,
                  gettextf('
                           <p style="color:tomato;"><b>This is a warning!</b></p>
                           Error proportion limit of %1$.2f is crossed for the first time in %2$i data points in the prediction period. At this point on average %3$.2f data points will be out of control with an lower limit of %4$.2f and an upper limit of %5$.2f </p>
                           ',options$predictionLimit,
                           which(predLimitReached)[1],
                           futureSummary$number[which(predLimitReached)[1]],
                           futureSummary$lowerNumber[which(predLimitReached)[1]],
                           futureSummary$higherNumber[which(predLimitReached)[1]]),
                  gettextf('<p ">No warning. The limit of %1$.2f is not crossed during the prediction period.</p>',options$predictionLimit)
                  ),position = 5
    )


  p <- ggplot2::ggplot(combinedSummary,ggplot2::aes(x = time, y = mean)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(y=actual),size=0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lowerCI, ymax = higherCI),
                         fill = "blue", alpha = 0.25) +
    jaspGraphs::themeJaspRaw() + jaspGraphs::geom_rangeframe() +
    ggplot2::theme(panel.grid = ggplot2::theme_bw()$panel.grid) +
    ggplot2::scale_x_continuous("Time",breaks = xBreaks,limits = c(min(combinedSummary$time),max(combinedSummary$time))) +
    #ggplot2::ylab(yTitle) +
    ggplot2::scale_y_continuous("Proportion",breaks = yBreaks,limits = c(0,1)) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 3, r = 12, b = 0, l = 1)) +
    ggplot2::geom_vline(xintercept = nrow(binomialSummary),linetype = "dashed")

  if(options$predictionLimit > 0)
    p <- p + ggplot2::geom_hline(yintercept = options$predictionLimit,linetype="dashed",color="darkred")

  predictionPlot$plotObject <- p

  jaspResults[["mVarContMainContainer"]][["multiBinomialPredictionPlot"]] <- predictionPlot
  return()

}


.mVarContMultiBinomialPredictionTables <- function(jaspResults,options,ready){
  if(!ready || !options$predictionTimeTable || !is.null(jaspResults[["mVarContMainContainer"]][["predictionTable"]])) return()

  predictionTable <- createJaspTable(title = "PredictionTable",
                                     dependencies = c(.multiVarModelDependencies(),
                                                      .multiVarPredictionDependencies()),position = 7)
  predictionTable$dependOn(c("predictionTableNumber","binTable"))

  if(options$binTable > 1) predictionTable$addColumnInfo(name = "bins", title = "Bin", type= "string")
  if(options$binTable == 1) predictionTable$addColumnInfo(name = "time", title = "Time bin", type = "integer")
  predictionTable$addColumnInfo(name = "proportion", title = "Proportion", type = "integer", format = "dp:3")

  predictionTable$addColumnInfo(name = "lower", title = "Lower", type = "number",overtitle = "Proportion Prediction Interval", format = "dp:2")
  predictionTable$addColumnInfo(name = "upper", title = "Upper", type = "number",overtitle = "Proportion Prediction Interval", format = "dp:2")

  if(options$predictionTableNumber){
    predictionTable$addColumnInfo(name = "number", title = "Number", type = "integer", format = "dp:2")
    predictionTable$addColumnInfo(name = "lowerNumber", title = "Lower", type = "integer",overtitle = "Number Prediction Interval", format = "dp:2")
    predictionTable$addColumnInfo(name = "upperNumber", title = "Upper", type = "integer",overtitle = "Number Prediction Interval", format = "dp:2")
    if(options$binTable > 1) predictionTable$addColumnInfo(name = "totalN", title = "Sample Size", type= "string")
  }
  .mVarContVarContMultiBinomialPredictionTablesFill(jaspResults,options,predictionTable)

  return()
}

.mVarContVarContMultiBinomialPredictionTablesFill <- function(jaspResults,options,predictionTable){
  futureSummary <- jaspResults[["binomialPredictions"]]$object$futureSummary

  if(options$binTable > 1){
    breaks <- seq(min(futureSummary$time)-1,max(futureSummary$time),options$binTable)
    labels <- paste(head(breaks, -1) +1, tail(breaks, -1), sep = '-')
    futureSummary$bins <- cut(futureSummary$time,breaks,labels)

    futureSummary <- as.data.frame(t(sapply(split(futureSummary,futureSummary$bins),function(x) c(bins = x$bins[1],
                                                                                                  time =x$time[1],
                                                                                                  mean = mean(x$mean),
                                                                                                  lowerCI = mean(x$lowerCI),
                                                                                                  higherCI = mean(x$higherCI),
                                                                                                  total = length(options$variables)*length(x$time)))))





    futureSummary$number  <- futureSummary$total*futureSummary$mean
    futureSummary$lowerNumber   <- futureSummary$total*futureSummary$lowerCI
    futureSummary$higherNumber    <- futureSummary$total*futureSummary$higherCI

    futureSummary$bins <- labels
  }

  if(options$binTable > 1) predictionTable[["bins"]] <- futureSummary$bins
  if(options$binTable == 1) predictionTable[["time"]] <- futureSummary$time
  predictionTable[["proportion"]] <- futureSummary$mean
  predictionTable[["lower"]] <- futureSummary$lowerCI
  predictionTable[["upper"]] <- futureSummary$higherCI

  if(options$predictionTableNumber){
    predictionTable[["number"]] <- futureSummary$number
    predictionTable[["lowerNumber"]] <- futureSummary$lowerNumber
    predictionTable[["upperNumber"]] <- futureSummary$higherNumber
    if(options$binTable>1) predictionTable[["totalN"]] <- futureSummary$total
  }
  jaspResults[["mVarContMainContainer"]][["predictionTable"]] <- predictionTable
  return()
}


