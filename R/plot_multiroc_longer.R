#' Plot Multiple ROC Curves from Long-Format Data
#'
#' This function generates multiple Receiver Operating Characteristic (ROC) curves from long-format data and plots them in a single graph. Each predictor (identified by the predictor_col) is used to compute an individual ROC curve, and the curves are displayed with distinct colors. Optionally, the AUC values can be shown in the legend.
#'
#' @param data A data frame in long format that contains at least three columns: a binary response variable, a predictor identifier, and the corresponding numeric values.
#' @param response_col A character string specifying the name of the column in \code{data} containing the binary response variable (e.g., 0/1 or "Control"/"Case").
#' @param predictor_col A character string specifying the name of the column in \code{data} that identifies the predictor (or model). This column's values will be used as the legend labels.
#' @param value_col A character string specifying the name of the column in \code{data} containing the numeric predictor values.
#' @param auc_show Logical. If \code{TRUE}, the AUC value for each predictor is displayed in the legend. Default is \code{FALSE}.
#' @param legend_pos A character string indicating the position of the legend. Can be \code{"right"} (default) or \code{"inside"} (e.g., bottom-right corner).
#' @param digits The number of decimal places to round the AUC values. Default is \code{3}.
#' @param line_size The thickness of the ROC curve lines. Default is \code{0.72}.
#' @param plot_theme A ggplot2 theme object used to customize the appearance of the plot. Default is \code{ggthemes::theme_clean()}.
#' @param palette A character string specifying the color palette to use for the ROC curves. Options are \code{"jama"} (default) or \code{"nejm"}. If \code{NULL}, the default ggplot2 colors are used.
#' @param custom_colors A character vector of custom colors for the ROC curves. If provided, this will override the \code{palette} parameter.
#' @param smooth Logical. If \code{TRUE}, the ROC curves are smoothed. Default is \code{FALSE}.
#'
#' @returns A \code{ggplot} object displaying multiple ROC curves in one plot. The plot includes:
#' \itemize{
#'   \item ROC curves for each predictor (model), distinguished by unique colors.
#'   \item A diagonal reference line representing random performance.
#'   \item A legend that can optionally display the AUC values.
#'   \item Customizable appearance via the provided theme and color parameters.
#' }
#'
#' @details
#' This function is designed for long-format data where each row corresponds to an observation for a particular predictor. The function groups the data by the predictor identifier (specified by \code{predictor_col}) and computes the ROC curve and AUC for each group using the \code{pROC} package. The resulting ROC curves are then combined into a single plot using \code{ggplot2}. Users can control whether AUC values are displayed in the legend and customize the plot using various parameters.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("example_data_long")
#' table(example_data_long$ID,example_data_long$Group)
#'
#' # 1.Plot ROC curves for all predictors in a single graph
#' plot_multiroc_longer(example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value")
#'
#' # 2.Plot ROC curves for all predictors in a single graph with AUC values displayed
#' plot_multiroc_longer(example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value",
#'    auc_show = TRUE)
#'
#' # 3.Use custom colors and place the legend inside the plot
#' plot_multiroc_longer(example_data_long,
#'    response_col = "Group",
#'    predictor_col = "ID",
#'    value_col = "Value",
#'    auc_show = TRUE,
#'    legend_pos = "inside")

plot_multiroc_longer <- function(data, response_col = NULL, predictor_col = NULL, value_col = NULL, auc_show = FALSE,
                                 legend_pos = "right", digits = 3, line_size = 0.72, plot_theme = ggthemes::theme_clean(),
                                 palette = "jama", custom_colors = NULL, smooth = FALSE) {

  roc_list <- data %>%
    dplyr::group_by(!!sym(predictor_col)) %>%
    dplyr::summarize(
      roc_obj = list(
        pROC::roc(
          response = !!sym(response_col),
          predictor = !!sym(value_col),
          levels = c("0","1"),
          direction = "<",
          smooth = smooth
        )
      )
    ) %>%
    dplyr::mutate(auc_val = sapply(roc_obj, pROC::auc))

  # 2) 生成命名的 ROC 列表
  roc_obj_list <- setNames(roc_list$roc_obj, roc_list[[predictor_col]])

  # 3) 如果需要显示 AUC，就生成带 AUC 的标签
  max_model_length <- max(nchar(roc_list[[predictor_col]]))
  if (auc_show) {
    model_labels <- sprintf(
      paste0("%-", max_model_length + 0, "s (AUC=%.2f)"),
      roc_list[[predictor_col]], roc_list$auc_val
    )
  } else {
    model_labels <- roc_list[[predictor_col]]
  }
  names(model_labels) <- roc_list[[predictor_col]]

  # 4) 颜色方案
  if (!is.null(custom_colors)) {
    color_vector <- rep(custom_colors, length.out = nrow(roc_list))
  } else if (palette == "jama") {
    color_vector <- c("#0072B2", "#E69F00", "#009E73", "#F0E442",
                      "#D55E00", "#CC79A7", "#56B4E9", "#999999",
                      "#C00000", "#FF8000", "#00B050", "#7030A0")
  } else if (palette == "nejm") {
    color_vector <- c("#3E4A89", "#B71C1C", "#1B5E20", "#F57F17",
                      "#4E342E", "#0D47A1", "#F44336", "#43A047",
                      "#8E24AA", "#3949AB", "#00ACC1", "#FF7043")
  } else {
    color_vector <- NULL
  }

  # 5) 调用 ggroc()，
  p <- pROC::ggroc(
    roc_obj_list,
    size = line_size,
    legacy.axes = TRUE  # 和 multiROC 一样
  ) +
    ggplot2::annotate("segment", x = 0, xend = 1, y = 0, yend = 1,
                      col = "darkgrey", linetype = "dashed", linewidth = 0.4) +
    plot_theme +
    theme(
      legend.position = if (legend_pos == "inside") c(0.95, 0.02) else "right",
      legend.justification = if (legend_pos == "inside") c(1, 0) else NULL,
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.8), colour = "black"),
      legend.text = ggplot2::element_text(family = "mono"),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0.5, size = 14, face = "plain"),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(colour = NA)
    ) +
    ggplot2::labs(title = "ROC Curves", x = "1 - Specificity", y = "Sensitivity", color = "Predictors") +
    ggplot2::coord_fixed(ratio = 1.04)

  # 6) 应用颜色和标签
  if (!is.null(color_vector)) {
    p <- p + scale_color_manual(
      values = color_vector,
      labels = model_labels,
      breaks = roc_list[[predictor_col]]
    )
  } else {
    p <- p + scale_color_discrete(
      labels = model_labels,
      breaks = roc_list[[predictor_col]]
    )
  }

  return(p)
}
