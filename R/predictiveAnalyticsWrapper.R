#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Predictive Analytics
#'
#' @param covariates, Covariates to be used in the prediction model. (optional)
#' @param dependent, Time series variable to be predicted. (needed)
#' @param time, Time variable that each corresponds to the time stamp of each observation. Can be in the following formats: ['YYYY-MM-DD', 'YYYY/MM/DD', 'YYYY-MM-DD HH:MM:SS', 'YYYY/MM/DD HH:MM:SS'] (needed)
#' @param trainingIndicator, Logical variable (only 0 or 1) indicating which cases should be used for training and verifying the models (= 1) and which cases should be predicted (= 0). This variable is necessary for making predictions when covariates and factors are supplied
predictiveAnalytics <- function(
          data = NULL,
          version = "1",
          acfLagsMax = 30,
          acfPartialCheck = FALSE,
          acfPlotCheck = FALSE,
          bmaMethod = "bmaMethodEm",
          bmaTestPeriod = "bmaTestNextSlice",
          bmaTestProp = 0.3,
          bmaWeightsTable = TRUE,
          bmaWeightsTablePerSlice = FALSE,
          checkFuturePredictionPlot = FALSE,
          checkPerformBma = FALSE,
          controlLineType = "line",
          controlPeriodCheck = FALSE,
          controlPeriodEnd = 0,
          controlPeriodStart = 0,
          controlPlotCheck = TRUE,
          controlPlotGrid = FALSE,
          controlPlotReportingCheck = FALSE,
          controlPlotReportingPercent = 0.05,
          controlPlotZoomCheck = FALSE,
          controlSpreadPointsEqually = TRUE,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          errorBoundMethodDrop = "stdDevBound",
          factors = list(types = list(), value = list()),
          featEngAutoTimeBased = FALSE,
          featEngImputeTS = FALSE,
          featEngLags = 0,
          futurePredPredictionType = "noFuturePrediction",
          futurePredReportingCheck = FALSE,
          futurePredSpreadPointsEqually = FALSE,
          futurePredThreshold = 0.95,
          futurePredTrainingPeriod = "last",
          futurePredTrainingPoints = 0,
          manualBoundErrorBound = 0,
          manualBoundMean = 0,
          manualBoundMethod = "manualBoundUniform",
          manualLowerBound = 0,
          manualUpperBound = 0,
          metricBias = TRUE,
          metricCoverage = TRUE,
          metricCrps = TRUE,
          metricDss = TRUE,
          metricLog = TRUE,
          metricMae = TRUE,
          metricPit = TRUE,
          metricR2 = TRUE,
          metricRmse = TRUE,
          modelsToPlot = list(types = list(), value = list()),
          modelsToPlotSlices = 5,
          outLierTableEnd = 0,
          outLierTableStart = 0,
          outlierHistogramCheck = FALSE,
          outlierTableCheck = FALSE,
          outlierTableFocusCheck = FALSE,
          outlierTableTransposeCheck = FALSE,
          periodicalPredictionNumber = 0,
          periodicalPredictionUnit = "days",
          pitPlots = list(types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          resampleCheckShowTrainingPlan = FALSE,
          resampleCumulativeCheck = FALSE,
          resampleForecastHorizon = 2,
          resampleInitialTraining = 2,
          resampleMaxSlice = 5,
          resamplePlanPlotEqualDistance = TRUE,
          resamplePlanPlotMaxPlots = 5,
          resampleSkip = 2,
          resampleSliceStart = "tail",
          selectedFuturePredictionModel = list(types = list(), value = ""),
          selectedModels = list(types = list(), value = list()),
          sigmaBound = 2,
          summaryStatsTableCheck = FALSE,
          time = list(types = list(), value = ""),
          trainingIndicator = list(types = list(), value = ""),
          trimmedMeanCheck = FALSE,
          trimmedMeanPercent = 0,
          xAxisLimit = "allData",
          zoomPeriodEnd = 0,
          zoomPeriodStart = 0) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("covariates", "dependent", "errorBoundMethodDrop", "factors", "modelsToPlot", "periodicalPredictionUnit", "pitPlots", "selectedFuturePredictionModel", "selectedModels", "time", "trainingIndicator")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspPredictiveAnalytics", "predictiveAnalytics", "predictiveAnalytics.qml", options, version, FALSE))
}