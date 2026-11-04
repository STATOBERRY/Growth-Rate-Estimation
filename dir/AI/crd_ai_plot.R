# =============================================================================
# crd_ai_plot.R
# RAISINS - AI Chat Plot Engine
# Standalone file: source this in global.R before crd_server.R
#
# Provides:
#   crd_ai_make_plot(results, plot_type, opts)  → ggplot object
#   crd_ai_plot_to_base64(gg, width, height)    → base64 PNG string
#   crd_ai_default_opts()                       → named list of all options
#   crd_ai_detect_plot(msg)                     → plot_type string or NULL
#   crd_ai_detect_edit(msg)                     → TRUE/FALSE
#   crd_ai_edit_prompt(plot_type)               → GPT system prompt string
#   crd_ai_apply_opts(opts, json_list)          → updated opts list
#
# ─────────────────────────────────────────────────────────────────────────────
# INPUT: crdp_results_store() - the reactiveVal already in crd_server.R
#   Relevant fields used here:
#     $raw_df        : data.frame with Treatment + response columns
#     $trans_df      : transformed version (same structure)
#     $crdp_char     : character (variable) names
#     $crdp_groups   : letter grouping matrix [treatments x chars]
#     $crdp_trt_means: treatment means matrix
#     $t_levels      : treatment level names (sorted)
#     $resp_cols     : response column names
#     $trt_col       : treatment column name
# =============================================================================

#### Changed by Claude AI (ppg)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(RColorBrewer)
  library(base64enc)
})

# ─────────────────────────────────────────────────────────────────────────────
# 1. KEYWORD DETECTION
# ─────────────────────────────────────────────────────────────────────────────
.PLOT_KEYWORDS <- list(
  boxplot   = c("box", "boxplot", "box plot"),
  violin    = c("violin", "violin plot"),
  bar       = c("bar", "bar chart", "bar plot", "bar graph", "barchart"),
  line      = c("line", "line graph", "line plot", "mean line", "dot plot", "mean dot"),
  linecon   = c("connected", "linecon", "connected line", "line con"),
  raincloud = c("raincloud", "rain cloud", "rain"),
  dist      = c("dist", "distribution", "density", "ridge"),
  summary   = c("summary", "box and bar", "box bar", "box + bar")  #### 
)

#### 
# Generic keywords that mean "give me a plot" without specifying type
.GENERIC_PLOT_KW <- c("plot", "chart", "graph", "individual plot",
                      "single plot", "show plot", "give me plot",
                      "generate plot", "create plot", "make plot")
#### 

.EDIT_KEYWORDS <- c(
  "change","make","set","use","switch","update","turn",
  "color","colour","theme","alpha","transparent","opacity",
  "size","font","angle","rotate","legend","title","bold",
  "palette","bigger","smaller","darker","lighter",
  "show","hide","remove","add","letters","grid","background",
  "individual","only","single","just","filter","all variable","all char",
  "notch","jitter","scatter","points","quantile","trim","dodge","mean point"
)

#### 
crd_ai_detect_plot <- function(msg) {
  m <- tolower(trimws(msg))
  # First check specific plot type keywords
  for (key in names(.PLOT_KEYWORDS))
    for (kw in .PLOT_KEYWORDS[[key]])
      if (grepl(kw, m, fixed = TRUE)) return(key)
  # Then check generic "plot"/"chart"/"graph" → returns "generic"
  for (kw in .GENERIC_PLOT_KW)
    if (grepl(kw, m, fixed = TRUE)) return("generic")
  NULL
}
#### 

crd_ai_detect_edit <- function(msg) {
  m <- tolower(msg)
  any(sapply(.EDIT_KEYWORDS, function(k) grepl(paste0("\\b", k, "\\b"), m, perl = TRUE)))
}

#### 
# Extract variable/character names from user message (case-insensitive match)
crd_ai_extract_chars <- function(msg, results) {
  if (is.null(results)) return(NULL)
  resp_cols <- results$resp_cols %||% results$crdp_char
  if (is.null(resp_cols) || length(resp_cols) == 0) return(NULL)
  m <- tolower(trimws(msg))
  matched <- resp_cols[sapply(resp_cols, function(v) grepl(tolower(v), m, fixed = TRUE))]
  if (length(matched) > 0) matched else NULL
}
#### 

# ─────────────────────────────────────────────────────────────────────────────
# 2. DEFAULT OPTIONS  (every knob the user can ask to change)
# ─────────────────────────────────────────────────────────────────────────────
crd_ai_default_opts <- function() {
  list(
    # ── Palette & layout ─────────────────────────────────────────────────────
    palette          = "Set2",       # RColorBrewer palette name
    ncol             = 2L,           # facet columns
    treatment_order  = "alpha",      # "alpha" or "data"
    filter_chars     = NULL,         # subset of response variables to show
    # ── Labels ───────────────────────────────────────────────────────────────
    title            = "",
    x_label          = "Treatment",
    y_label          = "Value",
    legend_title     = "Treatment",
    # ── Significance letters ─────────────────────────────────────────────────
    show_letters     = TRUE,
    letter_size      = 3.5,
    letter_color     = "#2d1c3b",
    # ── Theme ────────────────────────────────────────────────────────────────
    theme_name       = "minimal",    # minimal|bw|classic|gray|light|void
    # ── Title text ───────────────────────────────────────────────────────────
    title_size       = 14,  title_color = "#000000", title_bold = FALSE, title_hjust = 0.5,
    # ── Axis titles ──────────────────────────────────────────────────────────
    x_title_size     = 12,  x_title_color = "#000000", x_title_bold = FALSE,
    y_title_size     = 12,  y_title_color = "#000000", y_title_bold = FALSE,
    # ── Axis text ────────────────────────────────────────────────────────────
    x_text_angle     = 35L, x_text_size = 9L, x_text_color = "#000000",
    x_text_bold      = FALSE, x_text_hjust = 1.0, x_text_vjust = 0.5, x_text_hide = FALSE,
    y_text_size      = 9L,  y_text_color = "#000000", y_text_bold = FALSE,
    # ── Axis ticks & lines ───────────────────────────────────────────────────
    x_tick_color     = "#000000", x_tick_size = 0.5, x_tick_hide = FALSE,
    y_tick_color     = "#000000", y_tick_size = 0.5,
    x_axis_line_color= "#000000", x_axis_line_size = 0.5,
    y_axis_line_color= "#000000", y_axis_line_size = 0.5,
    # ── Legend ───────────────────────────────────────────────────────────────
    legend_position  = "none",
    legend_title_size= 11,  legend_title_color = "#000000", legend_title_bold = FALSE,
    legend_text_size = 9,   legend_text_color  = "#000000",
    legend_bg_color  = "#ffffff", legend_border_color = "#cccccc", legend_border_size = 0.5,
    # ── Strip (facet label) ───────────────────────────────────────────────────
    strip_text_size  = 9L,  strip_text_color = "#734d94", strip_text_bold = TRUE,
    strip_bg_color   = "#f3eefe", strip_border_color = "#c8b4e8",
    # ── Grid ─────────────────────────────────────────────────────────────────
    show_major_grid  = TRUE,  major_grid_color = "#e5e5e5", major_grid_size = 0.4, major_grid_linetype = "solid",
    show_minor_grid  = FALSE, minor_grid_color = "#f0f0f0", minor_grid_size = 0.25, minor_grid_linetype = "solid",
    # ── Panel & plot background ───────────────────────────────────────────────
    panel_bg_color   = "#ffffff", plot_bg_color = "#ffffff",
    # ── Y axis expand ────────────────────────────────────────────────────────
    y_expand_lower   = 0.05, y_expand_upper = 0.18,
    # ── Boxplot specific ─────────────────────────────────────────────────────
    box_alpha        = 0.85, box_linewidth = 0.5, box_linetype = "solid",
    outlier_size     = 1.5,  outlier_color = "#000000", outlier_alpha = 0.7,
    show_mean_point  = FALSE, mean_point_size = 2, mean_point_color = "#e63946", mean_point_shape = 18L,
    # ── Violin specific ───────────────────────────────────────────────────────
    violin_alpha     = 0.75, violin_trim = FALSE, violin_scale = "area", inset_box_width = 0.12,
    # ── Bar specific ─────────────────────────────────────────────────────────
    bar_alpha        = 0.85, bar_border_color = "#555555", bar_border_size = 0.4, bar_width = 0.7,
    errorbar_width   = 0.18, errorbar_size = 0.5, errorbar_color = "#333333",
    # ── Line / dot specific ───────────────────────────────────────────────────
    point_size       = 3.5,  point_shape = 16L, point_alpha = 1,
    errorbar_h_height= 0.25, line_width = 0.9, line_color = "#555555"
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. APPLY GPT JSON EDITS TO OPTS
# ─────────────────────────────────────────────────────────────────────────────
####  - safe value conversion
crd_ai_apply_opts <- function(opts, json_list) {
  for (nm in names(json_list)) {
    val <- json_list[[nm]]
    if (is.null(val) || (is.character(val) && nchar(trimws(val)) == 0)) next
    if (nm %in% names(opts)) {
      existing <- opts[[nm]]
      new_val <- tryCatch({
        if (is.logical(existing))  isTRUE(as.logical(val))
        else if (is.integer(existing)) .safe_int(val, existing)
        else if (is.numeric(existing)) .safe_num(val, existing)
        else .safe_str(val, existing)
      }, error = function(e) existing)
      # Only apply if result is valid (not NA, not empty)
      if (!is.null(new_val) && length(new_val) == 1 && !is.na(new_val))
        opts[[nm]] <- new_val
    }
  }
  opts
}
#### 

# ─────────────────────────────────────────────────────────────────────────────
# 4. BUILD PLOT DATA FROM crdp_results_store()
#    Adapts the stored result format to the long + summary format needed
# ─────────────────────────────────────────────────────────────────────────────
.make_ai_plot_data <- function(results, opts) {
  raw_df    <- results$raw_df
  groups    <- results$crdp_groups    # matrix or NULL
  t_levels  <- results$t_levels %||% gtools::mixedsort(unique(as.character(raw_df$Treatment)))
  resp_cols <- results$resp_cols %||% results$crdp_char
  
  # ── Long format ────────────────────────────────────────────────────────────
  data_long <- raw_df |>
    tidyr::pivot_longer(cols = dplyr::all_of(resp_cols),
                        names_to = "character", values_to = "value")
  data_long$character <- factor(data_long$character, levels = resp_cols)
  
  # ── Treatment ordering ──────────────────────────────────────────────────────
  ord <- opts$treatment_order %||% "alpha"
  lvls <- switch(ord,
                 "data"      = t_levels,   # original order from results (server-assigned)
                 "mean_desc" = {
                   # order by grand mean across all characters, high → low
                   mn <- tapply(data_long$value, as.character(data_long$Treatment), mean, na.rm = TRUE)
                   names(sort(mn, decreasing = TRUE))
                 },
                 "mean_asc"  = {
                   mn <- tapply(data_long$value, as.character(data_long$Treatment), mean, na.rm = TRUE)
                   names(sort(mn, decreasing = FALSE))
                 },
                 # default "alpha"
                 gtools::mixedsort(unique(as.character(data_long$Treatment)))
  )
  
  data_long$Treatment <- factor(data_long$Treatment, levels = lvls)
  
  # ── Summary ─────────────────────────────────────────────────────────────────
  df_summary <- data_long |>
    dplyr::group_by(character, Treatment) |>
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd_value   = sd(value,   na.rm = TRUE),
      .groups    = "drop"
    )
  
  # ── Letter grouping ──────────────────────────────────────────────────────────
  unique_chars <- unique(df_summary$character)
  df_summary <- df_summary |>
    dplyr::arrange(factor(character, levels = unique_chars), Treatment)
  
  if (!is.null(groups) && (is.matrix(groups) || is.data.frame(groups)) &&
      nrow(groups) > 0 && ncol(groups) > 0) {
    group_vector <- as.vector(as.matrix(groups))
    if (length(group_vector) == nrow(df_summary)) {
      df_summary$group <- group_vector
    } else {
      df_summary$group <- ""
    }
  } else {
    df_summary$group <- ""
  }
  
  df_summary$Treatment <- factor(df_summary$Treatment, levels = lvls)
  df_summary$character <- factor(df_summary$character, levels = resp_cols)
  
  # ── Filter characters ────────────────────────────────────────────────────────
  if (!is.null(opts$filter_chars) && length(opts$filter_chars) > 0) {
    data_long  <- data_long[data_long$character %in% opts$filter_chars, ]
    df_summary <- df_summary[df_summary$character %in% opts$filter_chars, ]
  }
  
  list(long = data_long, summary = df_summary)
}

.make_palette <- function(n, pal = "Set2") {
  tryCatch(
    colorRampPalette(RColorBrewer::brewer.pal(min(8, max(3, n)), pal))(n),
    error = function(e) rainbow(n)
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. SHARED THEME APPLICATION
# ─────────────────────────────────────────────────────────────────────────────
####  - guard against empty/vector values
.safe_str <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) default
  else as.character(x)[1]
}
.safe_int <- function(x, default = 2L) {
  v <- suppressWarnings(as.integer(x))
  if (is.null(v) || length(v) == 0 || is.na(v[1])) default else v[1]
}
.safe_num <- function(x, default = 0.5) {
  v <- suppressWarnings(as.numeric(x))
  if (is.null(v) || length(v) == 0 || is.na(v[1])) default else v[1]
}
.safe_lgl <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) default else isTRUE(x)
}
#### 

.get_theme <- function(name) {
  name <- .safe_str(name, "minimal")
  switch(name,
         "bw"      = theme_bw(),
         "normal"  = theme_bw(),
         "classic" = theme_classic(),
         "gray"    = theme_gray(),
         "light"   = theme_light(),
         "void"    = theme_void(),
         "dark"    = theme_dark(),
         "economist" = if (requireNamespace("ggthemes", quietly = TRUE)) ggthemes::theme_economist() else theme_minimal(),
         "tufte"     = if (requireNamespace("ggthemes", quietly = TRUE)) ggthemes::theme_tufte()     else theme_minimal(),
         "stata"     = if (requireNamespace("ggthemes", quietly = TRUE)) ggthemes::theme_stata()     else theme_minimal(),
         "wsj"       = if (requireNamespace("ggthemes", quietly = TRUE)) ggthemes::theme_wsj()       else theme_minimal(),
         "calc"      = if (requireNamespace("ggthemes", quietly = TRUE)) ggthemes::theme_calc()      else theme_minimal(),
         "hc"        = if (requireNamespace("ggthemes", quietly = TRUE)) ggthemes::theme_hc()        else theme_minimal(),
         theme_minimal()
  )
}

.apply_opts <- function(p, opts, is_horizontal = FALSE) {
  ####  - sanitize all opt values
  o <- opts
  o$theme_name      <- .safe_str(o$theme_name, "minimal")
  o$plot_bg_color   <- .safe_str(o$plot_bg_color, "#ffffff")
  o$panel_bg_color  <- .safe_str(o$panel_bg_color, "#ffffff")
  o$title_size      <- .safe_num(o$title_size, 14)
  o$title_color     <- .safe_str(o$title_color, "#000000")
  o$title_hjust     <- .safe_num(o$title_hjust, 0.5)
  o$x_title_size    <- .safe_num(o$x_title_size, 12)
  o$y_title_size    <- .safe_num(o$y_title_size, 12)
  o$x_text_angle    <- .safe_num(o$x_text_angle, 35)
  o$x_text_size     <- .safe_num(o$x_text_size, 9)
  o$y_text_size     <- .safe_num(o$y_text_size, 9)
  o$strip_text_size <- .safe_num(o$strip_text_size, 9)
  o$legend_position <- .safe_str(o$legend_position, "none")
  o$ncol            <- .safe_int(o$ncol, 2L)
  #### 
  face_fn <- function(b) if (isTRUE(b)) "bold" else "plain"
  
  p <- p +
    .get_theme(o$theme_name) +
    theme(
      plot.background   = element_rect(fill = o$plot_bg_color,  colour = NA),
      panel.background  = element_rect(fill = o$panel_bg_color, colour = NA),
      panel.grid.major  = if (isTRUE(o$show_major_grid))
        element_line(colour = o$major_grid_color, linewidth = o$major_grid_size,
                     linetype = o$major_grid_linetype) else element_blank(),
      panel.grid.minor  = if (isTRUE(o$show_minor_grid))
        element_line(colour = o$minor_grid_color, linewidth = o$minor_grid_size,
                     linetype = o$minor_grid_linetype) else element_blank(),
      plot.title        = element_text(size = o$title_size, colour = o$title_color,
                                       face = face_fn(o$title_bold), hjust = o$title_hjust),
      axis.title.x      = element_text(size = o$x_title_size, colour = o$x_title_color,
                                       face = face_fn(o$x_title_bold),
                                       margin = margin(t = 12)),
      axis.title.y      = element_text(size = o$y_title_size, colour = o$y_title_color,
                                       face = face_fn(o$y_title_bold)),
      axis.ticks.x      = if (isTRUE(o$x_tick_hide)) element_blank()
      else element_line(colour = o$x_tick_color, linewidth = o$x_tick_size),
      axis.ticks.y      = element_line(colour = o$y_tick_color, linewidth = o$y_tick_size),
      axis.line.x       = element_line(colour = o$x_axis_line_color, linewidth = o$x_axis_line_size),
      axis.line.y       = element_line(colour = o$y_axis_line_color, linewidth = o$y_axis_line_size),
      strip.text        = element_text(size = o$strip_text_size, colour = o$strip_text_color,
                                       face = face_fn(o$strip_text_bold)),
      strip.background  = element_rect(fill = o$strip_bg_color, colour = o$strip_border_color),
      legend.position   = o$legend_position,
      legend.title      = element_text(size = o$legend_title_size, colour = o$legend_title_color,
                                       face = face_fn(o$legend_title_bold)),
      legend.text       = element_text(size = o$legend_text_size, colour = o$legend_text_color),
      legend.background = element_rect(fill = o$legend_bg_color, colour = o$legend_border_color,
                                       linewidth = o$legend_border_size)
    )
  
  x_text <- if (isTRUE(o$x_text_hide)) element_blank() else
    element_text(angle = o$x_text_angle, size = o$x_text_size, colour = o$x_text_color,
                 face = face_fn(o$x_text_bold), hjust = o$x_text_hjust, vjust = o$x_text_vjust)
  y_text <- element_text(size = o$y_text_size, colour = o$y_text_color,
                         face = face_fn(o$y_text_bold))
  
  if (is_horizontal)
    p + theme(axis.text.x = y_text, axis.text.y = x_text)
  else
    p + theme(axis.text.x = x_text, axis.text.y = y_text)
}

# ─────────────────────────────────────────────────────────────────────────────
# 6. LETTER LABELS
# ─────────────────────────────────────────────────────────────────────────────
.add_letters <- function(p, pd, opts, is_bar = FALSE, is_horiz = FALSE) {
  if (!isTRUE(opts$show_letters)) return(p)
  sig <- pd$summary |> dplyr::filter(nchar(trimws(.data$group)) > 0)
  if (nrow(sig) == 0) return(p)
  
  if (is_horiz) {
    sig$xpos <- sig$mean_value + sig$sd_value
    p <- p + geom_text(data = sig,
                       aes(x = xpos, y = Treatment, label = group),
                       hjust = -0.3, size = opts$letter_size,
                       colour = opts$letter_color, fontface = "bold",
                       inherit.aes = FALSE)
  } else if (is_bar) {
    sig$ypos <- sig$mean_value + sig$sd_value * 1.2
    p <- p + geom_text(data = sig,
                       aes(x = Treatment, y = ypos, label = group),
                       vjust = -0.2, size = opts$letter_size,
                       colour = opts$letter_color, fontface = "bold",
                       inherit.aes = FALSE)
  } else {
    sig$ypos <- sig$mean_value + sig$sd_value * 1.4
    p <- p + geom_text(data = sig,
                       aes(x = Treatment, y = ypos, label = group),
                       vjust = -0.2, size = opts$letter_size,
                       colour = opts$letter_color, fontface = "bold",
                       inherit.aes = FALSE)
  }
  p
}

# ─────────────────────────────────────────────────────────────────────────────
# 7. INDIVIDUAL PLOT FUNCTIONS
# ─────────────────────────────────────────────────────────────────────────────

.plot_boxplot <- function(results, opts) {
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$long$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Box Plot"
  
  p <- ggplot(pd$long, aes(x = Treatment, y = value, fill = Treatment)) +
    geom_boxplot(alpha       = opts$box_alpha,
                 linewidth   = opts$box_linewidth,
                 linetype    = opts$box_linetype,
                 outlier.size   = opts$outlier_size,
                 outlier.colour = opts$outlier_color,
                 outlier.alpha  = opts$outlier_alpha,
                 position    = position_dodge2(preserve = "total")) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(opts$y_expand_lower, opts$y_expand_upper))) +
    facet_wrap(~character, scales = "free_y", ncol = opts$ncol) +
    labs(title = title, x = opts$x_label, y = opts$y_label, fill = opts$legend_title)
  
  if (isTRUE(opts$show_mean_point))
    p <- p + stat_summary(fun = mean, geom = "point",
                          size = opts$mean_point_size,
                          colour = opts$mean_point_color,
                          shape = opts$mean_point_shape)
  
  p <- .add_letters(p, pd, opts)
  .apply_opts(p, opts)
}

.plot_violin <- function(results, opts) {
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$long$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Violin Plot"
  
  p <- ggplot(pd$long, aes(x = Treatment, y = value, fill = Treatment)) +
    geom_violin(trim = opts$violin_trim, scale = opts$violin_scale,
                alpha = opts$violin_alpha, colour = "grey30") +
    geom_boxplot(width = opts$inset_box_width, fill = "white",
                 colour = "grey30", alpha = 0.6,
                 outlier.size = opts$outlier_size) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(opts$y_expand_lower, opts$y_expand_upper))) +
    facet_wrap(~character, scales = "free_y", ncol = opts$ncol) +
    labs(title = title, x = opts$x_label, y = opts$y_label, fill = opts$legend_title)
  
  p <- .add_letters(p, pd, opts)
  .apply_opts(p, opts)
}

.plot_bar <- function(results, opts) {
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$summary$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Bar Chart (Mean \u00b1 SD)"
  
  p <- ggplot(pd$summary, aes(x = Treatment, y = mean_value, fill = Treatment)) +
    geom_col(alpha     = opts$bar_alpha,
             colour    = opts$bar_border_color,
             linewidth = opts$bar_border_size,
             width     = opts$bar_width) +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = opts$errorbar_width, linewidth = opts$errorbar_size,
                  colour = opts$errorbar_color) +
    scale_fill_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(opts$y_expand_lower, opts$y_expand_upper))) +
    facet_wrap(~character, scales = "free_y", ncol = opts$ncol) +
    labs(title = title, x = opts$x_label, y = opts$y_label, fill = opts$legend_title)
  
  p <- .add_letters(p, pd, opts, is_bar = TRUE)
  .apply_opts(p, opts)
}

.plot_line <- function(results, opts) {
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$summary$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Mean Value Plot"
  
  p <- ggplot(pd$summary, aes(x = mean_value, y = Treatment, colour = Treatment)) +
    geom_point(size = opts$point_size, shape = opts$point_shape, alpha = opts$point_alpha) +
    geom_errorbarh(aes(xmin = mean_value - sd_value, xmax = mean_value + sd_value),
                   height = opts$errorbar_h_height, linewidth = opts$errorbar_size) +
    scale_colour_manual(values = colors) +
    scale_x_continuous(expand = expansion(mult = c(opts$y_expand_lower, opts$y_expand_upper + 0.1))) +
    facet_wrap(~character, scales = "free_x", ncol = opts$ncol) +
    labs(title = title, x = "Mean", y = opts$x_label, colour = opts$legend_title)
  
  p <- .add_letters(p, pd, opts, is_horiz = TRUE)
  .apply_opts(p, opts, is_horizontal = TRUE)
}

.plot_linecon <- function(results, opts) {
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$summary$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Connected Mean Plot"
  
  p <- ggplot(pd$summary, aes(x = Treatment, y = mean_value,
                              group = 1, colour = Treatment)) +
    geom_line(colour = opts$line_color, linewidth = opts$line_width) +
    geom_point(size = opts$point_size, shape = opts$point_shape, alpha = opts$point_alpha) +
    geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                  width = opts$errorbar_width, colour = opts$line_color,
                  linewidth = opts$errorbar_size) +
    scale_colour_manual(values = colors) +
    scale_y_continuous(expand = expansion(mult = c(opts$y_expand_lower, opts$y_expand_upper))) +
    facet_wrap(~character, scales = "free_y", ncol = opts$ncol) +
    labs(title = title, x = opts$x_label, y = opts$y_label, colour = opts$legend_title)
  
  p <- .add_letters(p, pd, opts)
  .apply_opts(p, opts)
}

.plot_raincloud <- function(results, opts) {
  if (!requireNamespace("ggdist",  quietly = TRUE)) stop("Install ggdist for raincloud plots.")
  if (!requireNamespace("ggforce", quietly = TRUE)) stop("Install ggforce for raincloud plots.")
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$long$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Raincloud Plot"
  
  p <- ggplot(pd$long, aes(x = Treatment, y = value, fill = Treatment, colour = Treatment)) +
    ggdist::stat_halfeye(adjust = 0.6, width = 0.5, .width = 0,
                         justification = -0.25, point_colour = NA,
                         alpha = opts$violin_alpha) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.6,
                 fill = "white", colour = "grey30") +
    ggforce::geom_sina(size = 1.2, alpha = 0.4) +
    scale_fill_manual(values = colors) +
    scale_colour_manual(values = colors) +
    facet_wrap(~character, scales = "free_y", ncol = opts$ncol) +
    labs(title = title, x = opts$x_label, y = opts$y_label)
  
  .apply_opts(p, opts)
}

.plot_dist <- function(results, opts) {
  if (!requireNamespace("ggridges", quietly = TRUE)) stop("Install ggridges for distribution plots.")
  pd     <- .make_ai_plot_data(results, opts)
  n_trt  <- length(unique(pd$long$Treatment))
  colors <- .make_palette(n_trt, opts$palette)
  title  <- if (nchar(opts$title) > 0) opts$title else "CRD \u2014 Distribution (Density Ridges)"
  
  p <- ggplot(pd$long, aes(x = value, y = Treatment, fill = Treatment)) +
    ggridges::geom_density_ridges(alpha = opts$violin_alpha, colour = "grey30",
                                  scale = 0.9, rel_min_height = 0.01) +
    scale_fill_manual(values = colors) +
    facet_wrap(~character, scales = "free_x", ncol = opts$ncol) +
    labs(title = title, x = "Value", y = opts$x_label)
  
  .apply_opts(p, opts, is_horizontal = TRUE)
}

#### 
.plot_summary <- function(results, opts) {
  p1 <- .plot_boxplot(results, opts)
  p2 <- .plot_bar(results, opts)
  if (requireNamespace("patchwork", quietly = TRUE))
    patchwork::wrap_plots(p1, p2, ncol = 1)
  else p1
}
#### 

# ─────────────────────────────────────────────────────────────────────────────
# 8. MAIN ENTRY POINT
# ─────────────────────────────────────────────────────────────────────────────
crd_ai_make_plot <- function(results, plot_type, opts = NULL) {
  if (is.null(opts)) opts <- crd_ai_default_opts()
  if (is.null(results)) stop("No analysis results available.")
  
  ####  - sanitize critical opts
  plot_type    <- .safe_str(plot_type, "boxplot")
  opts$ncol    <- .safe_int(opts$ncol, 2L)
  opts$palette <- .safe_str(opts$palette, "Set2")
  opts$theme_name <- .safe_str(opts$theme_name, "minimal")
  #### 
  
  # Ensure raw_df exists - fall back to trans_data stored in results
  if (is.null(results$raw_df) && !is.null(results$trans_data)) {
    results$raw_df <- results$trans_data
  }
  if (is.null(results$raw_df)) stop("No data found in results.")
  
  # Ensure resp_cols and t_levels
  if (is.null(results$resp_cols))  results$resp_cols  <- results$crdp_char
  if (is.null(results$t_levels))   results$t_levels   <-
    gtools::mixedsort(unique(as.character(results$raw_df$Treatment)))
  
  fn <- switch(plot_type,
               boxplot   = .plot_boxplot,
               violin    = .plot_violin,
               bar       = .plot_bar,
               line      = .plot_line,
               linecon   = .plot_linecon,
               raincloud = .plot_raincloud,
               dist      = .plot_dist,
               summary   = .plot_summary,  #### 
               stop("Unknown plot type: ", plot_type)
  )
  
  tryCatch(fn(results, opts), error = function(e) {
    base::message("crd_ai_make_plot [", plot_type, "]: ", conditionMessage(e))
    NULL
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# 9. RENDER TO BASE64
# ─────────────────────────────────────────────────────────────────────────────
crd_ai_plot_to_base64 <- function(gg, width = 800, height = 520, res = 130) {
  if (is.null(gg)) return(NULL)
  tmp <- tempfile(fileext = ".png")
  on.exit({ try(grDevices::dev.off(), silent = TRUE); unlink(tmp) }, add = TRUE)
  tryCatch({
    grDevices::png(tmp, width = width, height = height, res = res)
    print(gg)
    grDevices::dev.off()
    raw <- readBin(tmp, "raw", file.info(tmp)$size)
    paste0("data:image/png;base64,", base64enc::base64encode(raw))
  }, error = function(e) {
    base::message("crd_ai_plot_to_base64: ", conditionMessage(e))
    NULL
  })
}

# ─────────────────────────────────────────────────────────────────────────────
# 10. GPT EDIT PROMPT  (~300 tokens, temp = 0, returns raw JSON only)
# ─────────────────────────────────────────────────────────────────────────────
crd_ai_edit_prompt <- function(plot_type) {
  # Common opts always available
  common <- paste0(
    '  "palette": one of Set1 Set2 Set3 Pastel1 Pastel2 Dark2 Accent Paired Blues Reds Greens Spectral RdYlGn RdBu\n',
    '  "theme_name": one of minimal bw classic gray light void dark\n',
    '  "ncol": integer 1–6  (facet columns)\n',
    '  "title": any text string\n',
    '  "x_label": any text\n',
    '  "y_label": any text\n',
    '  "show_letters": true or false\n',
    '  "letter_size": number 2–8\n',
    '  "letter_color": R color name or hex\n',
    '  "legend_position": right left top bottom none\n',
    '  "x_text_angle": number 0–90\n',
    '  "x_text_size": number 6–20\n',
    '  "show_major_grid": true or false\n',
    '  "strip_bg_color": R color name or hex\n',
    '  "strip_text_color": R color name or hex\n',
    '  "plot_bg_color": R color name or hex\n',
    '  "panel_bg_color": R color name or hex\n'
  )
  
  specific <- switch(plot_type,
                     boxplot  = paste0(
                       '  "box_alpha": number 0–1\n',
                       '  "box_linewidth": number 0.1–3\n',
                       '  "box_linetype": solid dashed dotted\n',
                       '  "show_mean_point": true or false\n',
                       '  "mean_point_color": R color name or hex\n',
                       '  "outlier_size": number 0.5–5\n'
                     ),
                     violin = paste0(
                       '  "violin_alpha": number 0–1\n',
                       '  "violin_trim": true or false\n',
                       '  "violin_scale": area count width\n',
                       '  "inset_box_width": number 0.05–0.3\n'
                     ),
                     bar = paste0(
                       '  "bar_alpha": number 0–1\n',
                       '  "bar_width": number 0.2–1\n',
                       '  "bar_border_color": R color name or hex\n',
                       '  "errorbar_color": R color name or hex\n',
                       '  "errorbar_width": number 0.05–0.5\n'
                     ),
                     line = paste0(
                       '  "point_size": number 1–8\n',
                       '  "point_shape": integer 1–25\n',
                       '  "errorbar_h_height": number 0.05–0.5\n'
                     ),
                     linecon = paste0(
                       '  "line_width": number 0.3–3\n',
                       '  "line_color": R color name or hex\n',
                       '  "point_size": number 1–8\n',
                       '  "point_shape": integer 1–25\n'
                     ),
                     raincloud = '  "violin_alpha": number 0–1\n',
                     dist      = '  "violin_alpha": number 0–1\n',
                     ""
  )
  
  paste0(
    "You are a plot customisation assistant for RAISINS CRD.\n",
    "The active plot is: '", plot_type, "'.\n\n",
    "Respond with ONLY a raw JSON object - no explanation, no markdown, no backticks.\n",
    "Map the user's request to one or more of these option keys:\n\n",
    common, specific,
    "\nExample: {\"palette\": \"Dark2\", \"box_alpha\": 0.6}\n",
    "If nothing matches, respond with exactly: {}\n"
  )
}

#### end change


# =============================================================================
# PLOT UI FOR CHAT  (self-contained, like chat_csv_generator.R)
# =============================================================================
# PUBLIC FUNCTIONS (called by chat_ui.R and chat_server.R):
#   crd_ai_plot_css()                                -> CSS string
#   crd_ai_build_toolbar(active_type, opts, ns_id)   -> tagList (dropdowns + buttons)
#   crd_ai_build_plot_card(m, is_active, toolbar, ns_fn) -> div (full card)
#   CRD_AI_PLOT_META                                 -> list of plot type labels/icons
#   CRD_AI_PLOT_NAMES                                -> named vector for captions
# =============================================================================

#### 

# ---------------------------------------------------------------------------
# 11. PLOT TYPE METADATA
# ---------------------------------------------------------------------------
CRD_AI_PLOT_META <- list(
  boxplot   = list(label = "Boxplot",             icon = "fa-chart-bar"),
  violin    = list(label = "Violin",              icon = "fa-wine-glass"),
  bar       = list(label = "Bar Chart",           icon = "fa-chart-column"),
  line      = list(label = "Mean Value (dot)",    icon = "fa-circle"),
  linecon   = list(label = "Connected Mean",      icon = "fa-chart-line"),
  summary   = list(label = "Summary (Box + Bar)", icon = "fa-layer-group"),
  raincloud = list(label = "Raincloud",           icon = "fa-cloud-rain"),
  dist      = list(label = "Distribution Ridges", icon = "fa-water")
)

# ---------------------------------------------------------------------------
# 12. PLOT NAMES (for captions)
# ---------------------------------------------------------------------------
CRD_AI_PLOT_NAMES <- c(
  boxplot   = "Box Plot",
  violin    = "Violin Plot",
  bar       = "Bar Chart",
  line      = "Mean Value Plot",
  linecon   = "Connected Mean Plot",
  summary   = "Summary Plot",
  raincloud = "Raincloud Plot",
  dist      = "Distribution Ridges"
)

# ---------------------------------------------------------------------------
# 13. CSS (include in chat_ui via tags$style(HTML(crd_ai_plot_css())))
# ---------------------------------------------------------------------------
crd_ai_plot_css <- function() {
  base_css <- "
.rch-plot-card {
  margin-top:8px; border-radius:12px; overflow:hidden;
  border:1px solid var(--border); background:white;
  box-shadow:0 2px 12px rgba(86,4,102,0.08);
}
.rch-plot-header {
  display:flex; align-items:center; gap:8px; padding:10px 14px;
  background:#faf0fc; border-bottom:1px solid var(--border);
  font-size:12.5px; color:var(--txt); font-weight:600; flex-wrap:wrap;
}
.rch-plot-header .fa { font-size:14px; color:var(--accent); }
.rch-ctrl-row {
  display:flex; align-items:center; gap:8px; padding:8px 14px;
  background:#fdf8fe; border-bottom:1px solid #f0e4f5; flex-wrap:wrap;
}
.rch-ctrl-label {
  font-size:10.5px; font-weight:700; color:var(--txt3);
  text-transform:uppercase; letter-spacing:.06em;
}
.rch-ctrl-sel {
  font-size:12px; padding:3px 8px; border-radius:6px;
  border:1px solid var(--border); background:white;
  color:var(--txt); font-family:var(--font); cursor:pointer;
  outline:none; min-width:70px;
}
.rch-ctrl-sel:focus { border-color:#9e5aab; box-shadow:0 0 0 2px rgba(86,4,102,0.1); }
.rch-ctrl-check {
  display:flex; align-items:center; gap:4px;
  font-size:12px; color:var(--txt); cursor:pointer;
}
.rch-ctrl-check input { accent-color:var(--accent); cursor:pointer; }
.rch-plt-btns {
  display:flex; flex-wrap:wrap; gap:6px; padding:8px 14px;
  border-bottom:1px solid #f0e4f5;
}
.rch-plt-btn {
  font-size:11.5px; padding:5px 12px; border-radius:16px;
  border:1px solid var(--border); background:white;
  color:var(--txt2); cursor:pointer; font-family:var(--font);
  font-weight:500; transition:all .15s; display:flex;
  align-items:center; gap:5px; white-space:nowrap;
}
.rch-plt-btn:hover { border-color:var(--accent); color:var(--txt); background:#faf0fc; }
.rch-plt-btn.active {
  background:var(--accent); color:white; border-color:var(--accent);
  font-weight:600; box-shadow:0 1px 4px rgba(86,4,102,0.2);
}
.rch-plot-body { padding:0; background:white; }
.rch-plot-footer {
  display:flex; gap:8px; padding:10px 14px;
  border-top:1px solid #f0e4f5; background:#fdf8fe;
}
.rch-dl-btn {
  font-size:12px; padding:6px 16px; border-radius:20px;
  border:none; cursor:pointer; font-family:var(--font);
  font-weight:600; display:flex; align-items:center; gap:5px;
  transition:all .15s;
}
.rch-dl-btn.primary {
  background:linear-gradient(135deg,#560466,#2d1a30); color:white;
}
.rch-dl-btn.primary:hover { box-shadow:0 2px 8px rgba(86,4,102,0.3); transform:scale(1.02); }
.rch-dl-btn.secondary {
  background:white; color:var(--accent); border:1px solid var(--border);
}
.rch-dl-btn.secondary:hover { border-color:var(--accent); background:#faf0fc; }
.rch-plot-caption {
  font-size:12px; color:var(--txt3); padding:0 14px 10px;
  font-style:italic; background:white;
}
"
#### 
paste0(base_css, crd_ai_plot_settings_css())
#### 
}

# ---------------------------------------------------------------------------
# 14. TOOLBAR BUILDER (palette, theme, cols, letters, plot type buttons)
# ---------------------------------------------------------------------------
####  -- matches app.R exact two-event JS pattern
crd_ai_build_toolbar <- function(active_type, opts, ns_id, settings_open = FALSE) {
  
  # Two-event pattern (same as app.R): update stages value, apply flushes it
  fire_update <- function(fld, js_val, vtype = "S") {
    paste0(
      "Shiny.setInputValue('", ns_id, "plt_ctrl_update',",
      "{field:'", fld, "',value:", js_val, ",vtype:'", vtype, "'},",
      "{priority:'event'});"
    )
  }
  fire_apply <- paste0(
    "Shiny.setInputValue('", ns_id, "plt_ctrl_apply',",
    "Math.random(),{priority:'event'});"
  )
  
  # Palette
  pal_sel <- tags$select(
    class = "rch-ctrl-sel",
    onchange = paste0(fire_update("palette", "this.value", "S"), fire_apply),
    tags$option(value="Set2", if(identical(opts$palette,"Set2")) "selected" else NULL, "Set2 (default)"),
    tags$option(value="Set1", if(identical(opts$palette,"Set1")) "selected" else NULL, "Set1"),
    tags$option(value="Set3", if(identical(opts$palette,"Set3")) "selected" else NULL, "Set3"),
    tags$option(value="Paired", if(identical(opts$palette,"Paired")) "selected" else NULL, "Paired"),
    tags$option(value="Dark2", if(identical(opts$palette,"Dark2")) "selected" else NULL, "Dark2"),
    tags$option(value="Pastel1", if(identical(opts$palette,"Pastel1")) "selected" else NULL, "Pastel"),
    tags$option(value="Blues", if(identical(opts$palette,"Blues")) "selected" else NULL, "Blues"),
    tags$option(value="Greens", if(identical(opts$palette,"Greens")) "selected" else NULL, "Greens"),
    tags$option(value="Purples", if(identical(opts$palette,"Purples")) "selected" else NULL, "Purples"),
    tags$option(value="Oranges", if(identical(opts$palette,"Oranges")) "selected" else NULL, "Oranges"),
    tags$option(value="Spectral", if(identical(opts$palette,"Spectral")) "selected" else NULL, "Spectral")
  )
  
  # Theme
  theme_sel <- tags$select(
    class = "rch-ctrl-sel",
    onchange = paste0(fire_update("theme_name", "this.value", "S"), fire_apply),
    tags$option(value="minimal", if(identical(opts$theme_name,"minimal")) "selected" else NULL, "Minimal"),
    tags$option(value="classic", if(identical(opts$theme_name,"classic")) "selected" else NULL, "Classic"),
    tags$option(value="bw", if(identical(opts$theme_name,"bw")) "selected" else NULL, "B&W"),
    tags$option(value="light", if(identical(opts$theme_name,"light")) "selected" else NULL, "Light"),
    tags$option(value="gray", if(identical(opts$theme_name,"gray")) "selected" else NULL, "Gray"),
    tags$option(value="void", if(identical(opts$theme_name,"void")) "selected" else NULL, "Void"),
    tags$option(value="dark", if(identical(opts$theme_name,"dark")) "selected" else NULL, "Dark"),
    tags$option(value="economist", if(identical(opts$theme_name,"economist")) "selected" else NULL, "Economist"),
    tags$option(value="tufte", if(identical(opts$theme_name,"tufte")) "selected" else NULL, "Tufte")
  )
  
  # Ncol
  ncol_val <- .safe_int(opts$ncol, 2L)
  ncol_sel <- tags$select(
    class = "rch-ctrl-sel", style = "max-width:70px;",
    onchange = paste0(fire_update("ncol", "parseInt(this.value)", "I"), fire_apply),
    tags$option(value="1", if(ncol_val==1) "selected" else NULL, "1 col"),
    tags$option(value="2", if(ncol_val==2) "selected" else NULL, "2 cols"),
    tags$option(value="3", if(ncol_val==3) "selected" else NULL, "3 cols"),
    tags$option(value="4", if(ncol_val==4) "selected" else NULL, "4 cols")
  )
  
  # Letters
  letters_chk <- tags$label(
    class = "rch-ctrl-check",
    tags$input(type = "checkbox",
               if (isTRUE(opts$show_letters)) "checked" else NULL,
               onchange = paste0(fire_update("show_letters", "this.checked", "B"), fire_apply)),
     " Letters"
  )
  
  # Plot type buttons
  plot_btns <- div(class = "rch-plt-btns",
                   lapply(names(CRD_AI_PLOT_META), function(key) {
                     meta <- CRD_AI_PLOT_META[[key]]
                     cls  <- if (identical(key, active_type)) "rch-plt-btn active" else "rch-plt-btn"
                     js   <- paste0(fire_update("plot_type", paste0("'", key, "'"), "S"), fire_apply)
                     tags$button(class = cls, onclick = js,
                                 tags$i(class = paste("fas", meta$icon), style = "font-size:10px;"),
                                 meta$label)
                   })
  )
  
  tagList(
    div(class = "rch-ctrl-row",
        span(class = "rch-ctrl-label", "PALETTE"), pal_sel,
        span(class = "rch-ctrl-label", "THEME"),   theme_sel,
        span(class = "rch-ctrl-label", "COLS"),    ncol_sel,
        letters_chk,
        ####  - Plot Settings toggle
        tags$button(
          ### current changes by claude - apply 'open' class when panel is open
          class = paste("rch-settings-btn", if (isTRUE(settings_open)) "open" else ""),
          id = paste0(ns_id, "settings_toggle"),
          onclick = paste0(
            "var p=document.getElementById('", ns_id, "settings_panel');",
            "if(p){p.classList.toggle('visible');this.classList.toggle('open');}",
            ### current changes by claude - notify Shiny so panel stays open on re-render
            "Shiny.setInputValue('", ns_id, "plt_settings_toggle',Math.random(),{priority:'event'});"),
          tags$i(class = "fas fa-sliders"), " Plot Settings"
        )
        #### 
    ),
    plot_btns
  )
}
#### 

# ---------------------------------------------------------------------------
# 15. FULL PLOT CARD BUILDER (header + toolbar + plotOutput + caption + download)
# ---------------------------------------------------------------------------
#### 
crd_ai_build_plot_card <- function(m, is_active, toolbar, ns_fn, settings_panel = NULL) {
  div(class = "rch-plot-card",
      div(class = "rch-plot-header", icon("chart-area"), " PLOTS"),
      if (!is.null(toolbar)) toolbar else NULL,
      # Settings panel (collapsible, hidden by default)
      if (is_active && !is.null(settings_panel)) settings_panel else NULL,
      div(class = "rch-plot-body",
          plotOutput(ns_fn(m$plot_id), width = "100%",
                     height = m$plot_h %||% "450px")),
      if (!is.null(m$caption) && nchar(m$caption) > 0)
        div(class = "rch-plot-caption", m$caption) else NULL,
      if (!is.null(m$dl_id))
        div(class = "rch-plot-footer",
            downloadButton(ns_fn(m$dl_id),
                           label = tagList(" PNG"),
                           class = "rch-dl-btn primary")) else NULL
  )
}
#### 


# =============================================================================
# 16. PLOT SETTINGS PANEL (Advanced controls - matches app.R exactly)
# =============================================================================
#### 

crd_ai_plot_settings_css <- function() {
  "
.rch-adv-panel { display:none; padding:12px 14px; background:#fdf8fe; border-bottom:1px solid #f0e4f5; }
.rch-adv-panel.visible { display:block; }
.rch-adv-tabs { display:flex; gap:4px; margin-bottom:10px; flex-wrap:wrap; }
.rch-adv-tab {
  font-size:11px; padding:5px 12px; border-radius:14px; border:1px solid var(--border);
  background:white; color:var(--txt2); cursor:pointer; font-weight:500; font-family:var(--font);
}
.rch-adv-tab.active { background:var(--accent); color:white; border-color:var(--accent); }
.rch-adv-pane { display:none; }
.rch-adv-pane.active { display:block; }
.rch-adv-grid { display:grid; grid-template-columns:1fr 1fr; gap:8px 14px; }
.rch-adv-item { font-size:11.5px; }
.rch-adv-item label { display:block; font-weight:600; color:var(--txt2); margin-bottom:2px; font-size:10.5px; }
.rch-adv-item input[type=range] { width:100%; accent-color:var(--accent); }
.rch-adv-item input[type=color] { width:36px; height:24px; border:1px solid var(--border); border-radius:4px; cursor:pointer; }
.rch-adv-item input[type=text] {
  width:100%; font-size:12px; border:1.5px solid #d4c4ee; border-radius:7px; padding:3px 6px;
  background:#fff; font-family:var(--font);
}
.rch-adv-item select {
  width:100%; font-size:12px; border:1.5px solid #d4c4ee; border-radius:7px; padding:3px 6px;
  background:#fff; font-family:var(--font);
}
.rch-adv-color { display:flex; align-items:center; gap:6px; }
.rch-adv-color label { margin-bottom:0; }
.rch-adv-check { display:flex; align-items:center; gap:4px; cursor:pointer; font-size:11.5px; }
.rch-adv-check input { accent-color:var(--accent); cursor:pointer; }
.rch-adv-footer { display:flex; gap:8px; justify-content:flex-end; margin-top:10px; padding-top:8px; border-top:1px solid #f0e4f5; }
.rch-adv-reset {
  font-size:11.5px; padding:5px 14px; border-radius:14px; border:1px solid var(--border);
  background:white; color:var(--txt2); cursor:pointer; font-family:var(--font);
}
.rch-adv-apply {
  font-size:11.5px;
  padding:5px 14px;
  border-radius:14px;
  border:none;
  background:#28a745;   /* green */
  color:white;
  cursor:pointer;
  font-weight:600;
  font-family:var(--font);
}

.rch-adv-apply:hover {
  background: #218838;  /* darker green */
  box-shadow: 0 2px 6px rgba(40,167,69,0.3);
}
.rch-settings-btn:hover { border-color:var(--accent); color:var(--txt); background:#faf0fc; }
.rch-settings-btn.open { background:var(--accent); color:white; border-color:var(--accent); }
.rch-geom-pane { display:none; }
.rch-geom-pane.active { display:block; }
"
}


crd_ai_build_plot_settings <- function(ns_id, active_type, is_open = FALSE) {
  ts <- paste0("rch_", as.integer(Sys.time()))
  adv_id <- paste0(ns_id, "settings_panel")   ### current changes by claude - ID must match toolbar button
  
  # JS helpers - same two-event pattern as toolbar
  fire <- function(fld, js_val, vtype = "S") {
    paste0("Shiny.setInputValue('", ns_id, "plt_ctrl_update',",
           "{field:'", fld, "',value:", js_val, ",vtype:'", vtype, "'},",
           "{priority:'event'});")
  }
  fire_apply <- paste0("Shiny.setInputValue('", ns_id, "plt_ctrl_apply',Math.random(),{priority:'event'});")
  
  # Helper builders (same as app.R)
  adv_sel <- function(fld, lbl, choices_vec, def, vtype="S") {
    tags$div(class="rch-adv-item",
             tags$label(lbl),
             tags$select(
               onchange=paste0("var v=this.value;", fire(fld, "v", vtype), fire_apply),
               lapply(seq_along(choices_vec), function(i) {
                 v <- choices_vec[[i]]; nm <- names(choices_vec)[i]
                 if (v == def) tags$option(value=v, selected=NA, nm)
                 else          tags$option(value=v, nm)
               })
             )
    )
  }
  
  adv_sld <- function(fld, lbl, mn, mx, step, def, vtype="N") {
    lid <- paste0("rch_", fld, "_", ts)
    tags$div(class="rch-adv-item",
             tags$label(HTML(paste0(lbl, ' <span id="', lid, '" style="color:#734d94">', def, '</span>'))),
             tags$input(type="range", min=mn, max=mx, step=step, value=def,
                        oninput=paste0(
                          'document.getElementById("', lid, '").textContent=this.value;',
                          'var v=', if(vtype=="I") "parseInt(this.value)" else "parseFloat(this.value)", ';',
                          fire(fld, "v", vtype), fire_apply
                        )
             )
    )
  }
  
  adv_cpk <- function(fld, lbl, def="#000000") {
    tags$div(class="rch-adv-item rch-adv-color",
             tags$label(lbl),
             tags$input(type="color", value=def,
                        oninput=paste0("var v=this.value;", fire(fld, "v", "S"), fire_apply))
    )
  }
  
  adv_chk <- function(fld, lbl, def=FALSE) {
    args <- list(type="checkbox",
                 onchange=paste0("var v=this.checked;", fire(fld, "v", "B"), fire_apply))
    if (def) args$checked <- NA
    tags$div(class="rch-adv-item",
             tags$label(class="rch-adv-check", do.call(tags$input, args), paste0(" ", lbl)))
  }
  
  ### current changes by claude - stage on keystroke, apply only on blur or Enter (prevents re-render per letter)
  adv_txt <- function(fld, lbl, ph="") {
    tags$div(class="rch-adv-item",
             tags$label(lbl),
             tags$input(type="text", placeholder=ph,
                        oninput=paste0("var v=this.value;", fire(fld, "v", "S")),
                        onblur=fire_apply,
                        onkeydown=paste0("if(event.key==='Enter'){", fire_apply, "}"))
    )
  }
  
  ## claude change - deferred helpers: stage value on change, apply only via Apply button
  adv_sld_defer <- function(fld, lbl, mn, mx, step, def, vtype="N") {
    lid <- paste0("rch_", fld, "_", ts)
    tags$div(class="rch-adv-item",
             tags$label(HTML(paste0(lbl, ' <span id="', lid, '" style="color:#734d94">', def, '</span>'))),
             tags$input(type="range", min=mn, max=mx, step=step, value=def,
                        oninput=paste0(
                          'document.getElementById("', lid, '").textContent=this.value;',
                          'var v=', if(vtype=="I") "parseInt(this.value)" else "parseFloat(this.value)", ';',
                          fire(fld, "v", vtype)
                        )
             )
    )
  }
  
  adv_cpk_defer <- function(fld, lbl, def="#000000") {
    tags$div(class="rch-adv-item rch-adv-color",
             tags$label(lbl),
             tags$input(type="color", value=def,
                        oninput=paste0("var v=this.value;", fire(fld, "v", "S")))
    )
  }
  
  adv_sel_defer <- function(fld, lbl, choices_vec, def, vtype="S") {
    tags$div(class="rch-adv-item",
             tags$label(lbl),
             tags$select(
               onchange=paste0("var v=this.value;", fire(fld, "v", vtype), fire_apply),
               lapply(seq_along(choices_vec), function(i) {
                 v <- choices_vec[[i]]; nm <- names(choices_vec)[i]
                 if (v == def) tags$option(value=v, selected=NA, nm)
                 else          tags$option(value=v, nm)
               })
             )
    )
  }
  
  # ── Tab: General ───────────────────────────────────────────────────────
  tab_general <- div(class="rch-adv-pane active", id=paste0("rtab_gen_",ts),
                     div(class="rch-adv-grid",
                         adv_txt("title", "Plot title", "Auto"),
                         adv_txt("x_label", "X label", "Treatment"),
                         adv_txt("y_label", "Y label", "Value"),
                         adv_sld_defer("letter_size", "Letter size", 1.5, 6, 0.5, 3.5),
                         adv_cpk_defer("letter_color", "Letter colour", "#2d1c3b"),
                         adv_sld_defer("y_expand_upper", "Top space", 0.05, 0.6, 0.05, 0.15),
                         adv_sel_defer("treatment_order", "Treatment order",
                                       c("Alphabetical"="alpha", "Data order"="data", "Mean (high-low)"="mean_desc", "Mean (low-high)"="mean_asc"), "alpha")
                     )
  )
  
  # ── Tab: Axes & Text ───────────────────────────────────────────────────
  tab_axes <- div(class="rch-adv-pane", id=paste0("rtab_ax_",ts),
                  div(class="rch-adv-grid",
                      adv_sld("x_text_angle", "X label angle", 0, 90, 5, 35, "I"),
                      adv_sld("x_text_size", "X text size", 5, 16, 1, 8, "I"),
                      adv_cpk("x_text_color", "X text colour", "#000000"),
                      adv_chk("x_text_bold", "X text bold"),
                      adv_chk("x_text_hide", "Hide X labels"),
                      adv_sld("y_text_size", "Y text size", 5, 16, 1, 9, "I"),
                      adv_cpk("y_text_color", "Y text colour", "#000000"),
                      adv_sld("x_title_size", "X title size", 8, 20, 1, 12, "I"),
                      adv_cpk("x_title_color", "X title colour", "#000000"),
                      adv_chk("x_title_bold", "X title bold"),
                      adv_sld("y_title_size", "Y title size", 8, 20, 1, 12, "I"),
                      adv_cpk("y_title_color", "Y title colour", "#000000"),
                      adv_sld("title_size", "Plot title size", 8, 24, 1, 14, "I"),
                      adv_cpk("title_color", "Title colour", "#000000"),
                      adv_chk("title_bold", "Title bold"),
                      adv_sld("title_hjust", "Title align", 0, 1, 0.1, 0.5)
                  )
  )
  
  # ── Tab: Theme ─────────────────────────────────────────────────────────
  tab_theme <- div(class="rch-adv-pane", id=paste0("rtab_th_",ts),
                   div(class="rch-adv-grid",
                       adv_sel("legend_position", "Legend",
                               c("None"="none","Right"="right","Left"="left","Bottom"="bottom","Top"="top"), "none"),
                       adv_cpk("panel_bg_color", "Panel background", "#ffffff"),
                       adv_cpk("plot_bg_color", "Plot background", "#ffffff"),
                       adv_chk("show_major_grid", "Major gridlines", TRUE),
                       adv_cpk("major_grid_color", "Grid colour", "#e5e5e5"),
                       adv_sld("major_grid_size", "Grid width", 0.1, 1.5, 0.1, 0.4),
                       adv_chk("show_minor_grid", "Minor gridlines", FALSE),
                       adv_sld("strip_text_size", "Facet text size", 6, 16, 1, 9, "I"),
                       adv_cpk("strip_text_color", "Facet text colour", "#734d94"),
                       adv_cpk("strip_bg_color", "Facet background", "#f3eefe")
                   )
  )
  
  # ── Tab: Geom Options ──────────────────────────────────────────────────
  gsid <- paste0("rch_geom_", ts)
  ghdr <- function(ic, lbl) tags$div(class="rch-adv-item", style="grid-column:1/-1;margin-top:4px;",
                                     tags$label(style="color:#734d94;font-weight:700;font-size:11px;", paste0(ic, " ", lbl)))
  
  gp_box <- div(class="rch-geom-pane active", id=paste0(gsid,"_boxplot"),
                div(class="rch-adv-grid",
                    ghdr("\U0001f4e6","Boxplot"),
                    adv_sld("box_alpha","Fill opacity",0.1,1,0.05,0.85),
                    adv_sld("box_linewidth","Border width",0.1,2,0.1,0.5),
                    adv_sel("box_linetype","Border type",c("Solid"="solid","Dashed"="dashed","Dotted"="dotted"),"solid"),
                    adv_sld("outlier_size","Outlier size",0.5,5,0.5,1.5),
                    adv_cpk("outlier_color","Outlier colour","#000000"),
                    adv_sld("outlier_alpha","Outlier opacity",0.1,1,0.1,0.7),
                    adv_chk("show_mean_point","Show mean point"),
                    adv_sld("mean_point_size","Mean pt size",0.5,6,0.5,2),
                    adv_cpk("mean_point_color","Mean pt colour","#ff4444")
                ))
  
  gp_violin <- div(class="rch-geom-pane", id=paste0(gsid,"_violin"),
                   div(class="rch-adv-grid",
                       ghdr("\U0001f3bb","Violin"),
                       adv_sld("violin_alpha","Fill opacity",0.1,1,0.05,0.75),
                       adv_chk("violin_trim","Trim tails"),
                       adv_sel("violin_scale","Scale",c("Area"="area","Count"="count","Width"="width"),"area"),
                       adv_sld("inset_box_width","Inset box width",0.02,0.3,0.02,0.12),
                       adv_sld("outlier_size","Outlier size",0.5,5,0.5,1)
                   ))
  
  gp_bar <- div(class="rch-geom-pane", id=paste0(gsid,"_bar"),
                div(class="rch-adv-grid",
                    ghdr("\U0001f4ca","Bar Chart"),
                    adv_sld("bar_alpha","Fill opacity",0.1,1,0.05,0.85),
                    adv_sld("bar_width","Bar width",0.1,1,0.05,0.7),
                    adv_cpk("bar_border_color","Border colour","#555555"),
                    adv_sld("bar_border_size","Border width",0.1,2,0.1,0.5),
                    ghdr("\u2194","Error Bars"),
                    adv_sld("errorbar_width","Cap width",0.02,0.5,0.02,0.2),
                    adv_sld("errorbar_size","Line width",0.1,2,0.1,0.5),
                    adv_cpk("errorbar_color","Colour","#444444")
                ))
  
  gp_line <- div(class="rch-geom-pane", id=paste0(gsid,"_line"),
                 div(class="rch-adv-grid",
                     ghdr("\u25CF","Mean Value Dot"),
                     adv_sld("point_size","Point size",0.5,8,0.5,3.5),
                     adv_sel("point_shape","Point shape",c("Circle"="16","Triangle"="17","Square"="15","Diamond"="18"),"16","I"),
                     adv_sld("point_alpha","Opacity",0.1,1,0.1,1),
                     ghdr("\u2194","Error Bars"),
                     adv_sld("errorbar_h_height","Height",0.05,0.5,0.05,0.25),
                     adv_sld("errorbar_size","Width",0.1,2,0.1,0.5)
                 ))
  
  gp_linecon <- div(class="rch-geom-pane", id=paste0(gsid,"_linecon"),
                    div(class="rch-adv-grid",
                        ghdr("\U0001f4c8","Connected Mean"),
                        adv_sld("line_width","Line width",0.2,3,0.1,0.8),
                        adv_cpk("line_color","Line colour","#666666"),
                        adv_sld("point_size","Point size",0.5,8,0.5,3.5),
                        adv_sel("point_shape","Point shape",c("Circle"="16","Triangle"="17","Square"="15","Diamond"="18"),"16","I"),
                        adv_sld("point_alpha","Opacity",0.1,1,0.1,1),
                        ghdr("\u2194","Error Bars"),
                        adv_sld("errorbar_width","Cap width",0.02,0.5,0.02,0.2),
                        adv_sld("errorbar_size","Width",0.1,2,0.1,0.5)
                    ))
  
  gp_rain <- div(class="rch-geom-pane", id=paste0(gsid,"_raincloud"),
                 div(class="rch-adv-grid",
                     ghdr("\U0001f327","Raincloud / Distribution"),
                     adv_sld("violin_alpha","Fill opacity",0.1,1,0.05,0.75),
                     adv_sld("point_size","Jitter size",0.2,4,0.1,1.2)
                 ))
  
  gp_dist <- div(class="rch-geom-pane", id=paste0(gsid,"_dist"),
                 div(class="rch-adv-grid",
                     ghdr("\U0001f4ca","Density Ridges"),
                     adv_sld("violin_alpha","Ridge opacity",0.1,1,0.05,0.75)
                 ))
  
  gp_summary <- div(class="rch-geom-pane", id=paste0(gsid,"_summary"),
                    div(class="rch-adv-grid",
                        ghdr("\U0001f4e6","Boxplot part"),
                        adv_sld("box_alpha","Box opacity",0.1,1,0.05,0.85),
                        adv_sld("outlier_size","Outlier size",0.5,5,0.5,1.5),
                        ghdr("\U0001f4ca","Bar part"),
                        adv_sld("bar_alpha","Bar opacity",0.1,1,0.05,0.85),
                        adv_sld("bar_width","Bar width",0.1,1,0.05,0.7),
                        adv_sld("errorbar_width","Error bar cap",0.02,0.5,0.02,0.2),
                        adv_cpk("errorbar_color","Error bar colour","#444444")
                    ))
  
  # Geom pane switcher JS
  geom_js <- tags$script(HTML(paste0(
    "(function(){",
    "  var subId='", gsid, "';",
    "  window.rchSwitchGeom=function(key){",
    "    var ps=document.querySelectorAll('[id^=\"'+subId+'_\"]');",
    "    ps.forEach(function(p){p.classList.remove('active');});",
    "    var t=document.getElementById(subId+'_'+key);",
    "    if(t)t.classList.add('active');",
    "  };",
    "}());"
  )))
  
  # Tab bar JS
  tab_js <- function(pane_id) paste0(
    "document.querySelectorAll('#",adv_id," .rch-adv-tab').forEach(t=>t.classList.remove('active'));",
    "document.querySelectorAll('#",adv_id," .rch-adv-pane').forEach(p=>p.classList.remove('active'));",
    "this.classList.add('active');",
    "document.getElementById('",pane_id,"_",ts,"').classList.add('active');"
  )
  
  tab_geom <- div(class="rch-adv-pane", id=paste0("rtab_gm_",ts),
                  geom_js,
                  tags$p(style="font-size:11px;color:#9b7fc0;margin-bottom:8px;font-style:italic;",
                         "\U0001f4a1 Options update per plot type. Click a different type above to see its settings."),
                  gp_box, gp_violin, gp_bar, gp_line, gp_linecon, gp_rain, gp_dist, gp_summary
  )
  
  # ── Assemble panel ─────────────────────────────────────────────────────
  ### current changes by claude - apply 'visible' class when panel should stay open
  panel_class <- if (isTRUE(is_open)) "rch-adv-panel visible" else "rch-adv-panel"
  div(class=panel_class, id=adv_id,
      div(class="rch-adv-tabs",
          tags$button(class="rch-adv-tab active", onclick=tab_js("rtab_gen"), "General"),
          tags$button(class="rch-adv-tab",        onclick=tab_js("rtab_ax"),  "Axes & Text"),
          tags$button(class="rch-adv-tab",        onclick=tab_js("rtab_th"),  "Theme"),
          tags$button(class="rch-adv-tab",        onclick=tab_js("rtab_gm"),  "Geom Options")
      ),
      tab_general, tab_axes, tab_theme, tab_geom,
      div(class="rch-adv-footer",
          tags$button(class="rch-adv-reset",
                      onclick=paste0(fire("reset","null","S"), fire_apply),
                      tags$i(class="fas fa-rotate-left"), " Reset"),
          tags$button(class="rch-adv-apply",
                      onclick=fire_apply,
                      tags$i(class="fas fa-check"), " Apply")
      )
  )
}
#### 