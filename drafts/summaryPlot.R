devtools::load_all()
library(ggplot2)
library(tidyr)
library(dplyr)



dec2bin = function(decimal_number, nattributes, basevector){
  dec = decimal_number
  profile = matrix(NA, nrow = 1, ncol = nattributes)
  for (i in nattributes:1){
    profile[1,i] =  dec %% basevector[i]
    dec = (dec - dec %% basevector[i])/basevector[i]
  }
  return(profile)
}


# list files in directory
directoryFiles = dir()
nCondition = 600
nReplicationsPerCondition = 100


# list only simulation results files
repFiles = directoryFiles[grep(pattern = "rep\\_", x = directoryFiles)]

# calculate measurement accuracy =================================================
resultsLong = NULL
arrayNumber = 1
for (arrayNumber in 1:(nCondition*nReplicationsPerCondition)){
  print(arrayNumber)

  fileName = repFiles[which(repFiles == paste0("rep_",arrayNumber,".RData"))]

  # grep only complete files
  if (!any(directoryFiles %in% fileName)) next

  # if (any(directoryFiles %in% fileName)){
  load(file = fileName)

  # get the number of calibration
  calibrationF = as.factor(estimatedParameters$calibration)
  calibration = as.numeric(levels(calibrationF))

  # extract nStudents
  nStudents = nrow(simDataList$simData)

  # extract runningData except simulated response
  running = runningData[(nStudents+1):nrow(runningData), ]

  # extract nNewStudents
  nNewStudents =  max(estimatedProfiles[,"student"])


  betaInterceptBias = NULL
  betaLambdaBias = NULL
  varInterceptBias = NULL
  varLambdaBias = NULL
  betaInterceptRMSE = NULL
  betaLambdaRMSE = NULL
  varInterceptRMSE = NULL
  varLambdaRMSE = NULL
  exposure = NULL
  exposureRateMean = NULL
  exposureRateMax = NULL
  unUsedItemN = NULL
  profileRecoveryRate = NULL
  attributeRecoveryRate = NULL
  testLengthMean = NULL
  testLengthSD = NULL
  testLengthMin = NULL
  testLengthMax = NULL
  unconvg = NULL
  for (nCalibration in 1:length(calibration)) {


    # extract assigned estimatedParameter data with nCalibration
    dat = estimatedParameters[which(estimatedParameters$calibration == nCalibration), ]

    # grep a parameter row
    betaInterceptRow = grep("^beta_intercept\\[", dat$parameter)
    betaLambdaRow = grep("^beta_lambda\\[", dat$parameter)
    varInterceptRow = grep("^var_intercept", dat$parameter)
    varLambdaRow = grep("^var_lambda", dat$parameter)



    # BIAS and RMSE of item parameters ============================================================

    # get deviation: eap - trueValues
    deviation = dat$eap- dat$trueValues

    # get absolute bias
    betaInterceptBias = mean(abs(deviation[betaInterceptRow]))
    betaLambdaBias = mean(abs(deviation[betaLambdaRow]))
    varInterceptBias = mean(abs(deviation[varInterceptRow]))
    varLambdaBias = mean(abs(deviation[varLambdaRow]))

    # get rmse
    betaInterceptRMSE = sqrt(mean(deviation[betaInterceptRow]^2))
    betaLambdaRMSE = sqrt(mean(deviation[betaLambdaRow]^2))
    varInterceptRMSE = sqrt(mean(deviation[varInterceptRow]^2))
    varLambdaRMSE = sqrt(mean(deviation[varLambdaRow]^2))



    # Item Exposure Rate ============================================================
    first = nNewStudents * (nCalibration-1) + 1
    last = nNewStudents * nCalibration

    # extract response data for the nCalibration
    response = running[first:last, ]

    # get exposure rate
    expRate = colSums(!is.na(response))/nNewStudents

    exposureRateMean = mean(expRate)
    unUsedItemN = length(which(expRate == 0))



    # Test Length ===================================================================
    testLength = rowSums(!is.na(response))
    testLengthMean = mean(testLength)
    testLengthSD = sd(testLength)
    testLengthMin = min(testLength)
    testLengthMax = max(testLength)



    # Recovery Rate =================================================================
    profiles = estimatedProfiles[which(estimatedProfiles$calibration == nCalibration), ]

    # 1. profile recovery rate
    nSame = length(which(profiles$profileMAP == profiles$trueProfile))
    profileRecoveryRate = nSame/nNewStudents

    # 2. attribute pattern recovery rate
    estiA = profiles[grep("attributeMAP", names(profiles))]
    trueA = profiles[grep("trueAttribute", names(profiles))]
    nAccurate = mapply(function(e, t) sum(e == t), estiA, trueA)
    attributeRecoveryRate = sum(nAccurate) / sum(lengths(estiA))



    # maxRhat ===================================================
    if(calibrationData[[nCalibration]]$maxRhat>1.1){
      unconvg = 1
    }else{
      unconvg = 0
    }



    result = data.frame(betaInterceptBias = betaInterceptBias, betaLambdaBias = betaLambdaBias,
                        varInterceptBias = varInterceptBias, varLambdaBias = varLambdaBias,
                        betaInterceptRMSE = betaInterceptRMSE, betaLambdaRMSE = betaLambdaRMSE,
                        varInterceptRMSE = varInterceptRMSE, varLambdaRMSE = varLambdaRMSE,
                        exposureRateMean = exposureRateMean, unUsedItemN = unUsedItemN,
                        testLengthMean = testLengthMean, testLengthSD = testLengthSD,
                        testLengthMin = testLengthMin, testLengthMax = testLengthMax,
                        profileRecoveryRate = profileRecoveryRate, attributeRecoveryRate = attributeRecoveryRate,
                        unconvg = unconvg, nCalibration = nCalibration, arrayNumber = arrayNumber)

    # add condition number
    result$conditionN = ceiling(arrayNumber/nReplicationsPerCondition)

    # remove value of unconverged calibration
    result[which(result$unconvg == 1), !colnames(result) %in% c("unconvg", "arrayNumber", "nCalibration", "conditionN")] = NA

    # combine the result with resultsLong
    resultsLong = rbind(resultsLong, result)


  } #// end of nCalibration
  # } #// end of if(completeFiles)
} #// end of arrayNumber



# create conditions list
conditions = list(
  nItemsInPool = c(40, 105),
  initialPilotSampleSize = c(30, 300, 2000),
  nNewStudents = c(30, 300),
  itemUpdateFunction = c(
    "Sample 1",
    "Sample 10",
    "Sample 8000",
    "EAP",
    "MAP"
  ),
  itemSummaryFunction = c(
    "Sample 1",
    "Sample 10",
    "Sample 8000",
    "EAP",
    "MAP"
  ),
  stopCriterion = c(.7, .9)
)

# number of conditions
nConditions = prod(unlist(lapply(X = conditions, FUN = length)))

conditionsMatrix = matrix(NA, nrow = nConditions, ncol = length(conditions))
colnames(conditionsMatrix) = names(conditions)

cond = 1
for (cond in 1:nConditions){
  conditionsMatrix[cond,] = dec2bin(
    decimal_number = cond - 1,
    nattributes = length(conditions),
    basevector = unlist(lapply(X = conditions, FUN = length))
  ) + 1
}

conditionsMatrix = as.data.frame(conditionsMatrix)
conditionsMatrix = data.frame(lapply(conditionsMatrix, as.factor))
conditionsMatrix$conditionN = 1:nrow(conditionsMatrix)



# Add condition-factor columns to resultsLong
resultsLongConds = merge(resultsLong, conditionsMatrix, by = "conditionN", all.x = TRUE)

# Select only converged calibration
resultsConvg = resultsLongConds[which(resultsLongConds$unconvg==0),]

# Convert conditions to factors
resultsConvg$nItemsInPool = as.factor(resultsConvg$nItemsInPool)
resultsConvg$initialPilotSampleSize = as.factor(resultsConvg$initialPilotSampleSize)
resultsConvg$nNewStudents = as.factor(resultsConvg$nNewStudents)
resultsConvg$itemSummaryFunction = as.factor(resultsConvg$itemSummaryFunction)
resultsConvg$itemUpdateFunction = as.factor(resultsConvg$itemUpdateFunction)
resultsConvg$stopCriterion = as.factor(resultsConvg$stopCriterion)

# Aggregate the data using the .data pronoun with string names
resultsAggregated = resultsConvg %>%
  group_by(nCalibration, nItemsInPool, initialPilotSampleSize, nNewStudents,
           itemSummaryFunction, itemUpdateFunction, stopCriterion) %>%
  summarise(
    betaInterceptBias.Mean = mean(betaInterceptBias, na.rm = TRUE),
    betaInterceptBias.SD = sd(betaInterceptBias, na.rm = TRUE),
    betaLambdaBias.Mean = mean(betaLambdaBias, na.rm = TRUE),
    betaLambdaBias.SD = sd(betaLambdaBias, na.rm = TRUE),
    varInterceptBias.Mean = mean(varInterceptBias, na.rm = TRUE),
    varInterceptBias.SD = sd(varInterceptBias, na.rm = TRUE),
    varLambdaBias.Mean = mean(varLambdaBias, na.rm = TRUE),
    varLambdaBias.SD = sd(varLambdaBias, na.rm = TRUE),
    betaInterceptRMSE.Mean = mean(betaInterceptRMSE, na.rm = TRUE),
    betaInterceptRMSE.SD = sd(betaInterceptRMSE, na.rm = TRUE),
    betaLambdaRMSE.Mean = mean(betaLambdaRMSE, na.rm = TRUE),
    betaLambdaRMSE.SD = sd(betaLambdaRMSE, na.rm = TRUE),
    varInterceptRMSE.Mean = mean(varInterceptRMSE, na.rm = TRUE),
    varInterceptRMSE.SD = sd(varInterceptRMSE, na.rm = TRUE),
    varLambdaRMSE.Mean = mean(varLambdaRMSE, na.rm = TRUE),
    varLambdaRMSE.SD = sd(varLambdaRMSE, na.rm = TRUE),
    exposureRate.Mean = mean(exposureRateMean, na.rm = TRUE),
    exposureRate.SD = sd(exposureRateMean, na.rm = TRUE),
    unUsedItem.Mean = mean(unUsedItemN, na.rm = TRUE),
    unUsedItem.SD = sd(unUsedItemN, na.rm = TRUE),
    testLength.Mean = mean(testLengthMean, na.rm = TRUE),
    testLength.SD = sd(testLengthMean, na.rm = TRUE),
    profileRecoveryRate.Mean = mean(profileRecoveryRate, na.rm = TRUE),
    profileRecoveryRate.SD = sd(profileRecoveryRate, na.rm = TRUE),
    attributeRecoveryRate.Mean = mean(attributeRecoveryRate, na.rm = TRUE),
    attributeRecoveryRate.SD = sd(attributeRecoveryRate, na.rm = TRUE),
    .groups = 'drop'
  )


# Change the value of condition
nItemsInPool_map = setNames(c(40, 105), c(1, 2))
initialPilotSampleSize_map = setNames(c(30, 300, 2000), c(1, 2, 3))
nNewStudents_map = setNames(c(30, 300), c(1, 2))
itemFunction_map = setNames(c("S1", "S10", "Full", "EAP", "MAP"), c(1, 2, 3, 4, 5))
stopCriterion_map = setNames(c(0.7, 0.9), c(1, 2))


resultsLabelled = resultsAggregated %>%
  mutate(
    nItemsInPool = nItemsInPool_map[as.character(nItemsInPool)],
    initialPilotSampleSize = initialPilotSampleSize_map[as.character(initialPilotSampleSize)],
    nNewStudents = nNewStudents_map[as.character(nNewStudents)],
    itemUpdateFunction = itemFunction_map[as.character(itemUpdateFunction)],
    itemSummaryFunction = itemFunction_map[as.character(itemSummaryFunction)],
    stopCriterion = stopCriterion_map[as.character(stopCriterion)]
  )

# Save the newly labelled data to a new CSV file
write.csv(resultsLabelled, "resultsLabelled.csv", row.names = FALSE)


# Get the current column names
original_names = names(resultsLabelled)

# First, replace "betaLambda" with "betaSlope"
temp_names = gsub("betaLambda", "betaSlope", original_names)

# Then, on the result, replace "varLambda" with "varSlope"
temp_names = gsub("varLambda", "errorVarianceSlope", temp_names)

# Then, on the result, replace "varLambda" with "varSlope"
new_names = gsub("varIntercept", "errorVarianceIntercept", temp_names)

# Assign the final, corrected names back to the dataframe
names(resultsLabelled) = new_names



# # Aggregate by three conditions and plot
# plotBy3Factors = function(df, y_mean, y_sd, cond1_str, cond2_str, cond3_str, y_label, color_label,
#                            sample, pool, stop) {
#
#   # Define the desired order for the facet levels
#   facet_level_order = c("S1", "S10", "Full", "EAP", "MAP")
#
#   # Convert columns to factors with the correct, custom level order
#   df = df %>%
#     dplyr::mutate(
#       !!cond1_str := factor(.data[[cond1_str]], levels = facet_level_order),
#       !!cond2_str := factor(.data[[cond2_str]], levels = facet_level_order),
#       !!cond3_str := factor(.data[[cond3_str]])
#     )
#
#   # Create the plot
#   plot_object = ggplot(df, aes(x = nCalibration, y = .data[[y_mean]], color = .data[[cond3_str]])) +
#     geom_ribbon(
#       aes(
#         ymin = .data[[y_mean]] - .data[[y_sd]],
#         ymax = .data[[y_mean]] + .data[[y_sd]],
#         fill = .data[[cond3_str]]
#       ),
#       alpha = 0.2,
#       color = NA
#     ) +
#     geom_line(linewidth = 0.5) +
#     geom_point(size = 0.8) +
#     facet_grid(
#       as.formula(paste(cond1_str, "~", cond2_str)),
#       labeller = labeller(
#         .rows = function(value) paste("Update: ", value),
#         .cols = function(value) paste("Summary: ", value)
#       )
#     ) +
#     labs(
#       title = paste("Sample:", sample, " Pool:", pool, " Stop:", stop),
#       x = '# of Calibration',
#       y = y_label,
#       color = color_label,
#       fill = color_label
#     ) +
#     scale_color_brewer(palette = "Set1") +
#     scale_fill_brewer(palette = "Set1") +
#     theme_bw(base_size = 12) +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       legend.position = "bottom",
#       panel.spacing = unit(1, "lines")
#     )
#
#   # Conditionally set y-axis limits for plots
#   if (grepl("betaInterceptBias|betaSlopeBias", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0, 0.5))
#   } else if (grepl("betaInterceptRMSE|betaSlopeRMSE", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0, 0.8))
#   } else if (grepl("errorVarianceInterceptBias", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0, 1))
#   } else if (grepl("errorVarianceInterceptRMSE", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0, 1))
#   } else if (grepl("exposureRate", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0, 0.8))
#   } else if (grepl("testLength", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0, 30))
#   } else if (grepl("profile", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0.3, 0.9))
#   } else if (grepl("attribute", y_mean, ignore.case = TRUE)) {
#     plot_object = plot_object + coord_cartesian(ylim = c(0.6, 1))
#   }
#
#   # Return the plot object
#   return(plot_object)
# }
#
#
# # 1. Define the base names of all y-variables you want to plot
# y_vars_to_plot = c(
#   "betaInterceptBias", "betaSlopeBias", "errorVarianceInterceptBias", "errorVarianceSlopeBias",
#   "betaInterceptRMSE", "betaSlopeRMSE", "errorVarianceInterceptRMSE", "errorVarianceSlopeRMSE",
#   "exposureRate", "unUsedItem","testLength",
#   "profileRecoveryRate", "attributeRecoveryRate"
# )
#
# # Outer loop for your simulation conditions
# for(sample in initialPilotSampleSize_map) {
#   for(pool in nItemsInPool_map) {
#     for(stop in stopCriterion_map) {
#
#       print(paste0("sample", sample, "_pool", pool, "_stop", stop))
#
#       # Filter the data once for the current set of conditions
#       plotData = resultsLabelled %>%
#         dplyr::filter(initialPilotSampleSize == sample,
#                       nItemsInPool == pool,
#                       stopCriterion == stop)
#
#       # 2. Inner loop to create a plot for each y-variable
#       for (y_base in y_vars_to_plot) {
#
#         # Dynamically create the column names for mean and SD
#         y_mean_col = paste0(y_base, ".Mean")
#         y_sd_col = paste0(y_base, ".SD")
#
#         # Dynamically create a nice label for the y-axis
#         y_axis_label = tools::toTitleCase(gsub("([a-z])([A-Z])", "\\1 \\2", y_base))
#
#         # Call the plotting function
#         current_plot = plotBy3Factors(
#           df = plotData,
#           y_mean = y_mean_col,
#           y_sd = y_sd_col,
#           cond1_str = "itemUpdateFunction",
#           cond2_str = "itemSummaryFunction",
#           cond3_str = "nNewStudents",
#           y_label = y_axis_label,
#           color_label = '# of New Students',
#           sample = sample,
#           pool = pool,
#           stop = stop
#         )
#
#         # Dynamically create the file name and save the plot
#         file_name_label = paste0("sample", sample, "_pool", pool, "_stop", stop)
#         file_path = file.path(paste0("plot_", y_base, "_", file_name_label, ".pdf"))
#         ggsave(file_path, plot = current_plot, width = 11, height = 8.5)
#
#       }
#     }
#   }
# }



#-----------------------------------------------------------
# New plot: Use when summary and update has the same value
#------------------------------------------------------------

plotBy3FactorsNew = function(df, y_mean, cond1_str, cond2_str, cond3_str, y_label, color_label,
                              newStudents, stop) {

  # Define the desired order for the levels
  custom_level_order = c("S1", "S10", "Full", "EAP", "MAP")

  # Convert columns to factors, applying the custom order to cond3_str
  df = df %>%
    dplyr::mutate(
      !!cond1_str := factor(.data[[cond1_str]]),
      !!cond2_str := factor(.data[[cond2_str]]),
      !!cond3_str := factor(.data[[cond3_str]], levels = custom_level_order)
    )

  # Create the plot
  plot_object = ggplot(df, aes(x = nCalibration, y = .data[[y_mean]], color = .data[[cond3_str]])) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.8) +
    facet_grid(
      as.formula(paste(cond1_str, "~", cond2_str)),
      labeller = labeller(
        .rows = function(value) paste("Pool: ", value),
        .cols = function(value) paste("Sample Size: ", value)
      )
    ) +
    labs(
      title = paste("New Students:", newStudents, " Stop:", stop),
      x = '# of Calibration',
      y = y_label,
      color = color_label,
      fill = color_label
    ) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.spacing = unit(1, "lines")
    )

  # Conditionally set y-axis limits for plots
  if (grepl("betaInterceptBias|betaSlopeBias", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0, 0.5))
  } else if (grepl("betaInterceptRMSE|betaSlopeRMSE", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0, 0.8))
  } else if (grepl("errorVarianceInterceptBias", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0, 1))
  } else if (grepl("errorVarianceInterceptRMSE", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0, 1))
  } else if (grepl("exposureRate", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0, 0.8))
  } else if (grepl("testLength", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0, 30))
  } else if (grepl("profile", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0.3, 0.9))
  } else if (grepl("attribute", y_mean, ignore.case = TRUE)) {
    plot_object = plot_object + coord_cartesian(ylim = c(0.6, 1))
  }

  # Return the plot object
  return(plot_object)
}


# Remain when Summary and Update have the same value
resultsFiltered = resultsLabelled %>%
  filter(itemSummaryFunction == itemUpdateFunction)

# save plots
y_vars_to_plot = c("betaInterceptBias","betaSlopeBias","errorVarianceInterceptBias","errorVarianceSlopeBias",
                   "betaInterceptRMSE", "betaSlopeRMSE", "errorVarianceInterceptRMSE", "errorVarianceSlopeRMSE",
                   "profileRecoveryRate","attributeRecoveryRate","testLength","exposureRate")

nNewStudents = nNewStudents_map[1]
stop = stopCriterion_map[1]
y_base = y_vars_to_plot[1]
# Outer loop for your simulation conditions
for(newStudents in nNewStudents_map) {
  for(stop in stopCriterion_map) {

    print(paste0("newStudents", newStudents, "_stop", stop))

    # Filter the data once for the current set of conditions
    plotData = resultsFiltered %>%
      dplyr::filter(nNewStudents == newStudents,
                    stopCriterion == stop)

    # 2. Inner loop to create a plot for each y-variable
    for (y_base in y_vars_to_plot) {
      print(paste0(y_base))
      # Dynamically create the column names for mean and SD
      y_mean_col = paste0(y_base, ".Mean")

      # Dynamically create a nice label for the y-axis
      y_axis_label = tools::toTitleCase(gsub("([a-z])([A-Z])", "\\1 \\2", y_base))


      current_plot = plotBy3FactorsNew(
        df = plotData,
        y_mean = y_mean_col,
        cond1_str = "nItemsInPool",
        cond2_str = "initialPilotSampleSize",
        cond3_str = "itemSummaryFunction",
        y_label = y_axis_label,
        color_label = 'Summary/Update Method',
        newStudents = newStudents,
        stop = stop
      )


      # Dynamically create the file name and save the plot
      file_name_label = paste0("NewStudents", newStudents, "_stop", stop)
      file_path = file.path(paste0("plot2_", y_base, "_", file_name_label, ".png"))
      ggsave(file_path, plot = current_plot, width = 11, height = 8.5)

    }
  }
}

