## ============================================================================
##  RAISINS Growth Rate Estimation — trend-plot workspace (as in RAISINS RBD)
##  Colour patterns, palette styling, hide-legend and the DPI limit come from
##  greyskull (raisins_ws_*). The workspace frame / dialogs and the label
##  editor use the CRD workspace layout.
## ============================================================================

## ---- CRD workspace frame and dialogs --------------------------------------
raisins_plot_dialog_button <- function(id, label, glyph, tab=NULL) {
  tags$button(type="button",class="btn btn-default raisins-plot-tool",
    `data-toggle`="modal",`data-target`=paste0("#",id),
    `data-plot-tab`=tab,icon(glyph),tags$span(label))
}
raisins_plot_dialog_ui <- function(id,title,...) {
  div(id=id,class="modal fade raisins-plot-dialog",tabindex="-1",role="dialog",
    `aria-labelledby`=paste0(id,"-title"),`data-backdrop`="true",`data-keyboard`="true",
    div(class="modal-dialog modal-lg",role="document",
      div(class="modal-content",
        div(class="modal-header",tabindex="0",title="Drag this title bar to move; arrow keys also move the dialog",
          tags$button(type="button",class="close",`data-dismiss`="modal",`aria-label`="Close",tags$span(`aria-hidden`="true","\u00d7")),
          tags$h4(id=paste0(id,"-title"),class="modal-title",title),
          tags$small(class="raisins-plot-drag-hint","Click and drag title bar to move · Drag edges or corners to resize")),
        div(class="modal-body",...),
        div(class="modal-footer",tags$button(type="button",class="btn btn-primary",`data-dismiss`="modal","Done"))),
      lapply(c('n','e','s','w','ne','nw','se','sw'),function(edge)
        tags$span(class=paste('raisins-dialog-resize',paste0('raisins-resize-',edge)),
          `data-resize-dir`=edge,`aria-hidden`='true',title='Drag to resize dialog'))))
}
raisins_plot_workspace_ui <- function(title,toolbar,selector,plot,download,dialogs) {
  tagList(htmltools::singleton(tagList(
    tags$style(HTML("
      .raisins-plot-workspace{border:1px solid #dce3ed;border-radius:12px;background:#fff;margin:16px 0;box-shadow:0 3px 14px rgba(39,58,89,.06);overflow:visible;}
      .raisins-plot-workspace .raisins-plot-heading{display:flex;align-items:center;justify-content:space-between;gap:14px;flex-wrap:wrap;padding:18px 20px;border-bottom:1px solid #e6eaf0;background:#f7f9fc;border-radius:12px 12px 0 0;}
      .raisins-plot-workspace .raisins-plot-heading h3{margin:0;font-size:22px;color:#394b66;font-weight:600;}
      .raisins-plot-workspace .raisins-plot-toolbar{display:flex;flex-wrap:wrap;gap:8px;}
      .raisins-plot-workspace .raisins-plot-tool{display:inline-flex;align-items:center;gap:7px;min-height:38px;border-color:#cdd6e2;border-radius:6px;background:#fff;color:#35465f;}
      .raisins-plot-workspace .raisins-plot-selector{padding:14px 20px 0;max-width:380px;}
      .raisins-plot-workspace .raisins-plot-canvas{padding:10px 18px 18px;overflow-x:auto;}
      .raisins-plot-workspace .raisins-plot-download{padding:16px 20px;background:#f7f9fc;border-top:1px solid #e6eaf0;border-radius:0 0 12px 12px;}
      .raisins-plot-workspace .raisins-plot-export-settings{display:flex;gap:16px;flex-wrap:wrap;}
      .raisins-plot-workspace .raisins-plot-export-settings .form-group{width:145px;margin-bottom:12px;}
      .raisins-plot-workspace .raisins-plot-export-actions{display:flex;align-items:center;justify-content:space-between;gap:14px;flex-wrap:wrap;}
      .raisins-plot-workspace .raisins-plot-export-actions .form-group{margin-bottom:0;}
      .raisins-plot-dialog .modal-dialog{width:560px;max-width:calc(100% - 24px);margin:24px auto;}
      .raisins-plot-dialog .modal-body{max-height:calc(100vh - 175px);overflow-y:auto;padding:14px 18px;font-size:14px;}
      .raisins-plot-dialog .modal-header,.raisins-plot-dialog .modal-footer{padding:12px 18px;}
      .raisins-plot-dialog .modal-body .form-group{margin-bottom:10px;}
      .raisins-plot-dialog .modal-header{cursor:move;touch-action:none;user-select:none;}
      .raisins-plot-dialog .raisins-plot-drag-hint{display:block;margin-top:4px;font-size:11px;color:#617085;}
      .raisins-plot-dialog .modal-header .close{cursor:pointer;touch-action:auto;}
      .raisins-plot-dialog .modal-dialog{pointer-events:auto;overflow:visible;transition:none;}
      .raisins-plot-dialog .modal-content{display:flex;flex-direction:column;max-height:calc(100vh - 24px);}
      .raisins-plot-dialog .modal-header,.raisins-plot-dialog .modal-footer{flex-shrink:0;}
      .raisins-plot-dialog .modal-body{min-height:0;flex:1 1 auto;}
      .raisins-plot-dialog .modal-body .shiny-input-container{max-width:100%;}
      .raisins-plot-dialog .raisins-dialog-sized .modal-content{height:100%;}
      .raisins-plot-dialog .raisins-dialog-sized .modal-body{max-height:none;}
      .raisins-dialog-resize{position:absolute;z-index:10;display:block;pointer-events:auto;touch-action:none;user-select:none;}
      .raisins-resize-n,.raisins-resize-s{left:10px;right:10px;height:12px;cursor:ns-resize;}
      .raisins-resize-n{top:0;}.raisins-resize-s{bottom:0;}
      .raisins-resize-e,.raisins-resize-w{top:10px;bottom:10px;width:12px;cursor:ew-resize;}
      .raisins-resize-e{right:0;}.raisins-resize-w{left:0;}
      .raisins-resize-ne,.raisins-resize-nw,.raisins-resize-se,.raisins-resize-sw{width:20px;height:20px;}
      .raisins-resize-ne{top:0;right:0;cursor:nesw-resize;}.raisins-resize-sw{bottom:0;left:0;cursor:nesw-resize;}
      .raisins-resize-nw{top:0;left:0;cursor:nwse-resize;}.raisins-resize-se{bottom:0;right:0;cursor:nwse-resize;}
      .raisins-resize-se:after{content:'';position:absolute;right:4px;bottom:4px;width:10px;height:10px;border-right:2px solid #7e929e;border-bottom:2px solid #7e929e;}
      .modal-backdrop.raisins-plot-backdrop.in{opacity:.12;}
      .raisins-plot-toolbar .raisins-hide-legend .shiny-input-container{width:auto;margin:0;}
      .raisins-plot-toolbar .raisins-hide-legend .checkbox{margin:0;padding:8px 2px;}
      .raisins-plot-toolbar .raisins-hide-legend label{font-size:13px;color:#35465f;white-space:nowrap;}
      .raisins-plot-dialog .modal-body label{font-size:14px;}
      .raisins-plot-dialog .modal-body h4{font-size:16px;}
      .raisins-plot-dialog .modal-body .form-control{font-size:14px;}
      .raisins-plot-dialog .nav-tabs{margin-bottom:16px;}
      .raisins-plot-dialog .raisins-label-list{padding:0 4px;}
      .raisins-plot-dialog .raisins-label-list .control-label{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0;}
      .raisins-plot-dialog .raisins-label-original{padding-top:8px;overflow-wrap:anywhere;}
      .raisins-plot-workspace .raisins-plot-heading{background:#fff;padding:16px 20px;}
      .raisins-plot-workspace .raisins-plot-heading h3{font-size:20px;letter-spacing:-.2px;}
      .raisins-plot-workspace .raisins-plot-tool{font-size:13px;padding:8px 11px;transition:background-color .15s,border-color .15s;}
      .raisins-plot-workspace .raisins-plot-tool:hover,.raisins-plot-workspace .raisins-plot-tool:focus{background:#edf7f3;border-color:#568e7f;color:#15594b;}
      .raisins-plot-dialog .modal-content{border:1px solid #dce3e7;border-radius:10px;box-shadow:0 16px 48px rgba(29,46,61,.18);overflow:hidden;}
      .raisins-plot-dialog .modal-header{background:#f8fafb;border-bottom:1px solid #e5eaee;}
      .raisins-plot-dialog .modal-title{font-size:17px;font-weight:600;color:#263b4d;}
      .raisins-plot-dialog .modal-footer{background:#fafcfc;border-top:1px solid #e5eaee;}
      .raisins-plot-dialog .modal-footer .btn-primary{background:#087f5b;border-color:#087f5b;border-radius:6px;padding:7px 22px;}
      .raisins-plot-dialog .modal-body p,.raisins-plot-dialog .help-block{color:#5c6d79;font-size:13px;line-height:1.5;}
      .raisins-plot-dialog .form-control{border-color:#d6dfe5;border-radius:5px;box-shadow:none;}
      .raisins-plot-dialog .form-control:focus{border-color:#388873;box-shadow:0 0 0 2px rgba(56,136,115,.12);}
      .raisins-plot-dialog .raisins-style-picker .shiny-options-group{display:grid;grid-template-columns:1fr 1fr;gap:10px;}
      .raisins-plot-dialog .raisins-style-picker .radio{margin:0;}
      .raisins-plot-dialog .raisins-style-picker .radio:first-child{grid-column:1/-1;}
      .raisins-plot-dialog .raisins-style-picker .radio label{display:flex;align-items:center;min-height:92px;width:100%;padding:12px;border:1px solid #dce3e8;border-radius:8px;background:#fff;transition:border-color .15s,background-color .15s;}
      .raisins-plot-dialog .raisins-style-picker .radio label:has(input:checked){border-color:#087f5b;background:#eff8f4;box-shadow:0 0 0 1px #087f5b;}
      .raisins-plot-dialog .raisins-style-picker .radio label:focus-within{outline:2px solid #388873;outline-offset:2px;}
      .raisins-plot-dialog .raisins-style-picker input[type=radio]{position:static;float:none;flex-shrink:0;margin:0 10px 0 0;accent-color:#087f5b;}
      .raisins-plot-dialog .raisins-style-card{display:flex;align-items:center;gap:10px;}
      .raisins-plot-dialog .raisins-style-preview{width:82px;height:44px;flex-shrink:0;}
      .raisins-plot-dialog .raisins-style-copy strong{display:block;font-size:13px;color:#263b4d;margin-bottom:3px;}
      .raisins-plot-dialog .raisins-style-copy>span{display:block;font-size:11px;color:#617280;line-height:1.4;}
      .raisins-plot-dialog .raisins-label-list .row{margin:0 0 6px;padding:8px 0 0;border:1px solid #e5eaee;border-radius:6px;background:#fafcfc;}
      .raisins-plot-dialog .raisins-label-original{font-size:13px;color:#536674;}
      .raisins-plot-dialog .raisins-label-column-head{display:grid;grid-template-columns:5fr 7fr;padding:0 18px 8px;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.6px;color:#637685;}
      .raisins-plot-workspace .raisins-export-heading{font-size:14px;font-weight:600;color:#35495a;margin-bottom:12px;}
      .raisins-plot-workspace .raisins-plot-export-settings label{font-size:12px;color:#536674;}
      .raisins-plot-workspace .raisins-plot-export-settings .form-control{border:1px solid #d3dce3;border-radius:5px;box-shadow:none;}
      .raisins-plot-workspace .raisins-plot-export-actions .download-btn{background:#087f5b;border:1px solid #087f5b;border-radius:6px;box-shadow:none;padding:10px 18px;font-size:14px;color:#fff;}
      .raisins-plot-workspace .raisins-plot-export-actions .download-btn:hover{background:#06694a;}
      @media(max-width:520px){.raisins-plot-dialog .raisins-style-picker .shiny-options-group{grid-template-columns:1fr;}.raisins-plot-dialog .raisins-style-picker .radio label{min-height:76px;}.raisins-plot-dialog .raisins-style-preview{width:92px;}.raisins-plot-dialog .raisins-label-column-head{display:none;}}
      @media(max-width:600px){.raisins-plot-workspace .raisins-plot-heading,.raisins-plot-workspace .raisins-plot-download{padding:14px;}.raisins-plot-workspace .raisins-plot-toolbar{width:100%;}.raisins-plot-workspace .raisins-plot-tool{flex:1 1 44%;justify-content:center;}.raisins-plot-dialog .modal-body{max-height:65vh;}.raisins-plot-workspace .raisins-plot-export-settings .form-group{flex:1 1 90px;width:auto;}.raisins-plot-workspace .raisins-plot-export-actions .download-btn{width:100%;text-align:center;}}
    ")),
    tags$script(src=paste0("raisins-plot-dialog.js?v=",unname(tools::md5sum("www/raisins-plot-dialog.js")))),
    tags$script(HTML("$(document).on('show.bs.modal.raisinsPlotTools shown.bs.modal.raisinsPlotTools','.raisins-plot-dialog',function(e){this.scrollTop=0;$(this).find('.modal-body').scrollTop(0);if(e.type==='shown'){var heading=$(this).find('.modal-title').attr('tabindex','-1')[0];if(heading){try{heading.focus({preventScroll:true});}catch(err){heading.focus();}this.scrollTop=0;$(this).find('.modal-body').scrollTop(0);}}});"))
  )),div(class="raisins-plot-workspace",
    div(class="raisins-plot-heading",h3(title),div(class="raisins-plot-toolbar",toolbar)),
    if (!is.null(selector)) div(class="raisins-plot-selector",selector),
    div(class="raisins-plot-canvas",plot),
    if (!is.null(download)) div(class="raisins-plot-download",download),dialogs))
}

## ---- label editor (RBD's rcbd_box_labels_server) ----------------------------
gre_labels_server <- function(id, originals, dataset_key) {
  shiny::moduleServer(id, function(input, output, session) {
    fields <- shiny::reactive({
      values <- as.character(originals())
      key <- digest::digest(list(dataset_key(), values), algo="xxhash64")
      list(values=values, ids=paste0("label_",key,"_",seq_along(values)))
    })
    output$fields <- shiny::renderUI({
      f <- fields()
      shiny::tagList(lapply(seq_along(f$values),function(i) shiny::fluidRow(
        shiny::column(5, shiny::tags$div(class="raisins-label-original", f$values[i])),
        shiny::column(7, shiny::textInput(session$ns(f$ids[i]), label=paste("Plot label for",f$values[i]),
          value=f$values[i], width="100%")))))
    })
    shiny::observeEvent(input$reset, {
      f <- fields()
      for(i in seq_along(f$values)) shiny::updateTextInput(session,f$ids[i],value=f$values[i])
    })
    shiny::reactive({
      f <- fields()
      labels <- vapply(seq_along(f$values),function(i) {
        value <- input[[f$ids[i]]]
        if(is.null(value)||!nzchar(trimws(value))) f$values[i] else value
      },character(1))
      changed <- labels != f$values
      stats::setNames(labels[changed],f$values[changed])
    })
  })
}

## ---- trend-plot workspace UI ------------------------------------------------
gre_trend_workspace_ui <- function() {
  id <- "gre_ws_trend"
  raisins_plot_workspace_ui("Fitted Trend Plot",
    toolbar = tagList(
      raisins_plot_dialog_button(paste0(id, "_settings"), "Plot settings", "cog"),
      raisins_plot_dialog_button(paste0(id, "_labels"), "Change labels", "pen"),
      raisins_plot_dialog_button(paste0(id, "_styles"), "Colours", "palette"),
      raisins_plot_dialog_button(paste0(id, "_theme"), "Change theme", "adjust"),
      raisins_ws_legend_ui(paste0(id, "_hide_legend"))),
    selector = NULL,
    plot = shinycssloaders::withSpinner(plotOutput("gre_trend_plot", height = "460px")),
    download = uiOutput("gre_trend_plot_dl_ui"),
    dialogs = tagList(
      raisins_plot_dialog_ui(paste0(id, "_settings"), "Fitted Trend Plot settings",
        checkboxInput("gre_ws_trend_points", "Show observed values as points", TRUE),
        sliderInput("gre_ws_trend_point_size", "Point size", min = 0.5, max = 6, value = 2, step = 0.5),
        sliderInput("gre_ws_trend_line_width", "Trend line width", min = 0.25, max = 3, value = 1, step = 0.25),
        sliderInput("gre_ws_trend_text_size", "Text size", min = 8, max = 24, value = 13),
        selectInput("gre_ws_trend_font", "Font", c("Default" = "", "Cambria", "Arial", "Times New Roman",
                                                     "Helvetica", "Georgia", "Verdana"), selectize = FALSE)),
      raisins_plot_dialog_ui(paste0(id, "_labels"), "Change labels",
        tags$p("Changes affect this figure only; data and statistical results stay unchanged."),
        textInput("gre_ws_trend_title_input", "Plot title", value = "", placeholder = "Use the original plot label"),
        textInput("gre_ws_trend_x", "X-axis title", value = "", placeholder = "Use the original plot label"),
        textInput("gre_ws_trend_y", "Y-axis title", value = "", placeholder = "Use the original plot label"),
        tags$hr(), tags$h4("Series labels"),
        div(class = "raisins-label-column-head", tags$span("Original name"), tags$span("Display label")),
        div(class = "raisins-label-list", uiOutput(NS(paste0(id, "_edit"), "fields"))),
        actionButton(NS(paste0(id, "_edit"), "reset"), "Reset series labels", icon = icon("undo"),
                     class = "btn-default btn-sm")),
      raisins_plot_dialog_ui(paste0(id, "_styles"), "Colours", raisins_ws_colours_ui(paste0(id, "_preset"))),
      raisins_plot_dialog_ui(paste0(id, "_theme"), "Change theme",
        selectInput(paste0(id, "_theme_value"), "Theme",
                    c("Use my settings" = "original", "Minimal" = "minimal",
                      "Classic" = "classic", "Black and white" = "bw"),
                    selected = "original", selectize = FALSE))))
}

## Export controls, drawn once a trend has been fitted.
gre_trend_export_ui <- function(input) {
  tagList(
    div(class = "raisins-export-heading", "Download figure"),
    div(class = "raisins-plot-export-settings",
      sliderInput("gre_trend_width", "Width (inches)", value = isolate(input$gre_trend_width) %||% 8,
                  min = 1, max = 20, step = 0.5),
      sliderInput("gre_trend_height", "Height (inches)", value = isolate(input$gre_trend_height) %||% 5,
                  min = 1, max = 20, step = 0.5),
      raisins_ws_limit_dpi(sliderInput("gre_trend_dpi", "DPI (max 600)", value = isolate(input$gre_trend_dpi) %||% 300,
                                       min = 72, max = 600, step = 50))),
    div(class = "raisins-plot-export-actions",
      radioButtons("gre_trend_format", "Format:", c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
                   selected = isolate(input$gre_trend_format) %||% "png", inline = TRUE),
      downloadButton("gre_trend_plot_dl", "Download Plot", class = "download-btn")))
}

## ---- trend-plot server helpers ------------------------------------------------
## Builds the fitted-trend ggplot with every workspace choice applied.
gre_trend_plot_build <- function(r, input, labels = character()) {
  dd <- do.call(rbind, lapply(names(r$trend_fits), function(s) {
    f <- r$trend_fits[[s]]$fitted; f$Series <- s; f }))
  dd$Series <- factor(dd$Series, levels = names(r$trend_fits))
  base <- input$gre_ws_trend_text_size %||% 13
  font <- input$gre_ws_trend_font %||% ""
  p <- ggplot2::ggplot(dd, ggplot2::aes(year, colour = Series))
  if (!isFALSE(input$gre_ws_trend_points))
    p <- p + ggplot2::geom_point(ggplot2::aes(y = y), size = input$gre_ws_trend_point_size %||% 2)
  p <- p + ggplot2::geom_line(ggplot2::aes(y = fit), linewidth = input$gre_ws_trend_line_width %||% 1) +
    ggplot2::labs(x = r$year_col, y = "Value", title = "Observed values with best-fit trend")
  # Series display labels (from the label editor) before the colour pattern.
  if (length(labels))
    p <- p + ggplot2::scale_colour_discrete(labels = function(x) {
      i <- match(x, names(labels)); x[!is.na(i)] <- unname(labels[i[!is.na(i)]]); x })
  p <- raisins_ws_style(p, input$gre_ws_trend_preset %||% "custom")
  theme <- switch(input$gre_ws_trend_theme_value %||% "original",
                  classic = ggplot2::theme_classic(base_size = base, base_family = font),
                  bw      = ggplot2::theme_bw(base_size = base, base_family = font),
                  ggplot2::theme_minimal(base_size = base, base_family = font))
  p <- p + theme
  overrides <- list()
  ids <-c(title = "gre_ws_trend_title_input", x = "gre_ws_trend_x", y = "gre_ws_trend_y")
  for (nm in names(ids)) {
    v <- input[[ids[[nm]]]]
    if (length(v) == 1L && nzchar(v)) overrides[[nm]] <- v
  }
  if (length(overrides)) p <- p + do.call(ggplot2::labs, overrides)
  if (isTRUE(input$gre_ws_trend_hide_legend)) p <- p + ggplot2::theme(legend.position = "none")
  p
}

## Checked export dimensions (same limits as RBD's rcbd_ws_dimensions).
gre_trend_dimensions <- function(input) {
  width  <- input$gre_trend_width %||% 8
  height <- input$gre_trend_height %||% 5
  dpi    <- input$gre_trend_dpi %||% 300
  format <- input$gre_trend_format %||% "png"
  tryCatch({
    valid <- function(value, lo, hi) is.numeric(value) && length(value) == 1L && is.finite(value) && value >= lo && value <= hi
    if (!valid(width, 1, 40) || !valid(height, 1, 40)) stop("Choose finite export dimensions between 1 and 40 inches.", call. = FALSE)
    if (!valid(dpi, 72, 600)) stop("Choose DPI between 72 and 600.", call. = FALSE)
    if (!format %in% c("png", "jpeg", "tiff", "pdf", "svg")) stop("Choose a supported figure format.", call. = FALSE)
    if (format %in% c("png", "jpeg", "tiff") && width * height * dpi^2 > 4e7)
      stop("This raster image is too large. Reduce dimensions or DPI to stay below 40 million pixels.", call. = FALSE)
    list(width = width, height = height, dpi = dpi, format = format)
  }, error = function(e) { shiny::showNotification(conditionMessage(e), type = "error", duration = 8); stop(e) })
}

gre_open_device <- function(file, format, d) {
  switch(format,
    png  = grDevices::png(file, width = d$width, height = d$height, units = "in", res = d$dpi, bg = "white"),
    jpeg = grDevices::jpeg(file, width = d$width, height = d$height, units = "in", res = d$dpi, bg = "white"),
    tiff = grDevices::tiff(file, width = d$width, height = d$height, units = "in", res = d$dpi, bg = "white", compression = "lzw"),
    pdf  = grDevices::cairo_pdf(file, width = d$width, height = d$height),
    svg  = svglite::svglite(file, width = d$width, height = d$height))
  grDevices::dev.cur()
}
gre_interactive_workspace_ui <- function(title, plot, selector = NULL) {
  raisins_plot_workspace_ui(title, toolbar = NULL, selector = selector,
                            plot = plot, download = NULL, dialogs = NULL)
}

gre_publication_ui <- function() {
  raisins_plot_workspace_ui("Publication Figures",
    toolbar = tagList(
      raisins_plot_dialog_button("gre_pub_settings", "Plot settings", "cog"),
      raisins_plot_dialog_button("gre_pub_labels", "Change labels", "pen"),
      raisins_plot_dialog_button("gre_pub_colours", "Colours", "palette"),
      raisins_plot_dialog_button("gre_pub_theme", "Change theme", "adjust")),
    selector = tagList(
      selectInput("gre_pub_kind", "Figure",
        c("Observed values and fitted trend" = "trend",
          "Compound growth with confidence intervals" = "growth",
          "Variability and trend-adjusted instability" = "instability",
          "Area-yield contributions" = "decomposition")),
      uiOutput("gre_pub_series_ui")),
    plot = shinycssloaders::withSpinner(plotOutput("gre_pub_plot", height = "480px")),
    download = uiOutput("gre_pub_download_ui"),
    dialogs = tagList(
      raisins_plot_dialog_ui("gre_pub_settings", "Publication figure settings",
        sliderInput("gre_pub_point_size", "Point size", 0.5, 6, 2.5, step = 0.5),
        sliderInput("gre_pub_line_width", "Line width", 0.25, 3, 0.65, step = 0.05),
        sliderInput("gre_pub_text_size", "Text size", 8, 24, 10),
        selectInput("gre_pub_font", "Font", c("Sans serif" = "sans", "Serif" = "serif",
                    "Monospace" = "mono"), selectize = FALSE)),
      raisins_plot_dialog_ui("gre_pub_labels", "Change labels",
        tags$p("Changes affect this figure only."),
        textInput("gre_pub_title", "Plot title", placeholder = "Use the original plot label"),
        textInput("gre_pub_x", "X-axis title", placeholder = "Use the original plot label"),
        textInput("gre_pub_y", "Y-axis title", placeholder = "Use the original plot label")),
      raisins_plot_dialog_ui("gre_pub_colours", "Colours", raisins_ws_colours_ui("gre_pub_palette")),
      raisins_plot_dialog_ui("gre_pub_theme", "Change theme",
        selectInput("gre_pub_theme_value", "Theme",
          c("Use my settings" = "original", "Minimal" = "minimal", "Classic" = "classic",
            "Black and white" = "bw"), selectize = FALSE))))
}

gre_publication_export_ui <- function(input) {
  tagList(
    div(class = "raisins-export-heading", "Download figure"),
    div(class = "raisins-plot-export-settings",
      sliderInput("gre_pub_width", "Width (inches)", 1, 20,
                  isolate(input$gre_pub_width) %||% 7, step = 0.5),
      sliderInput("gre_pub_height", "Height (inches)", 1, 20,
                  isolate(input$gre_pub_height) %||% 4.5, step = 0.5),
      raisins_ws_limit_dpi(sliderInput("gre_pub_dpi", "DPI (max 600)", 72, 600,
                           isolate(input$gre_pub_dpi) %||% 600, step = 50))),
    div(class = "raisins-plot-export-actions",
      radioButtons("gre_pub_format", "Format:",
        c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
        selected = isolate(input$gre_pub_format) %||% "pdf", inline = TRUE),
      downloadButton("gre_pub_download", "Download Plot", class = "download-btn")))
}

gre_publication_plot <- function(r, kind, series, input) {
  dark <- "#263746"
  accent <- "#0072B2"
  contrast <- "#B34A00"
  point_size <- input$gre_pub_point_size %||% 2.5
  line_width <- input$gre_pub_line_width %||% 0.65
  if (kind == "trend") {
    fit <- r$trend_fits[[series]]
    shiny::validate(shiny::need(fit, "Run Trend fitting on an eligible series to build this figure."))
    p <- ggplot2::ggplot(fit$fitted, ggplot2::aes(year)) +
      ggplot2::geom_line(ggplot2::aes(y = fit), colour = accent, linewidth = line_width) +
      ggplot2::geom_point(ggplot2::aes(y = y), shape = 21, fill = "white", colour = dark, size = point_size) +
      ggplot2::scale_x_continuous(breaks = function(limits) {
        breaks <- pretty(limits)
        breaks[breaks == round(breaks)]
      }) +
      ggplot2::labs(x = "Year", y = series,
        caption = paste("Open points: observations. Line:", tolower(fit$best), "fit selected by AICc."))
  } else if (kind == "growth") {
    d <- r$group_comparison
    if (is.null(d)) {
      d <- r$growth
      shiny::validate(shiny::need(d, "Run Growth rates to build this figure."))
      d$Group <- d$Series
      d$Scope <- "Group"
    } else {
      d <- d[d$Series == series, , drop = FALSE]
    }
    d <- d[is.finite(d$`CGR (%/yr)`), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d) > 0, "No compound growth estimates are available."))
    d$Group <- factor(d$Group, levels = rev(d$Group))
    ci <- d[is.finite(d$`CGR CI low`) & is.finite(d$`CGR CI high`), , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(`CGR (%/yr)`, Group)) +
      ggplot2::geom_vline(xintercept = 0, colour = "#777777", linetype = "dashed", linewidth = 0.35) +
      ggplot2::geom_segment(data = ci, ggplot2::aes(x = `CGR CI low`, xend = `CGR CI high`, yend = Group),
                            colour = dark, linewidth = line_width) +
      ggplot2::geom_point(ggplot2::aes(shape = Scope), colour = accent, size = point_size) +
      ggplot2::scale_shape_manual(values = c(Combined = 18, Group = 16), guide = "none") +
      ggplot2::labs(x = "Compound growth (% per year)", y = NULL,
        caption = paste0("Bars: ", 100 * (1 - r$alpha),
          "% model confidence intervals where available.",
          if (!is.null(r$group_comparison)) "\nDiamond: combined series. Circles: individual groups."))
  } else if (kind == "instability") {
    d <- r$instab
    shiny::validate(shiny::need(d, "Run Instability to build this figure."))
    d <- d[is.finite(d$`CV (%)`) & is.finite(d$`CDVI (%)`), , drop = FALSE]
    shiny::validate(shiny::need(nrow(d) > 0, "No valid instability estimates are available."))
    p <- ggplot2::ggplot(d, ggplot2::aes(y = Series)) +
      ggplot2::geom_segment(ggplot2::aes(x = `CV (%)`, xend = `CDVI (%)`, yend = Series),
                            colour = "#888888", linewidth = line_width) +
      ggplot2::geom_point(ggplot2::aes(x = `CV (%)`), shape = 21, colour = contrast, fill = "white", size = point_size) +
      ggplot2::geom_point(ggplot2::aes(x = `CDVI (%)`), shape = 18, colour = accent, size = point_size) +
      ggplot2::labs(x = "Variability (%)", y = NULL,
        caption = "Open circles: CV. Diamonds: Cuddy-Della Valle Index (CDVI).")
  } else {
    d <- r$decomp
    shiny::validate(shiny::need(d, r$decomp_msg %||% "Run Area-Yield decomposition to build this figure."))
    d$Effect <- factor(d$Effect, levels = rev(d$Effect))
    d$Direction <- ifelse(d$Contribution < 0, "Decrease", "Increase")
    p <- ggplot2::ggplot(d, ggplot2::aes(Contribution, Effect, fill = Direction)) +
      ggplot2::geom_col(width = 0.55, colour = dark, linewidth = 0.25) +
      ggplot2::geom_vline(xintercept = 0, colour = dark, linewidth = 0.35) +
      ggplot2::scale_fill_manual(values = c(Increase = accent, Decrease = contrast), guide = "none") +
      ggplot2::labs(x = "Contribution (production units)", y = NULL,
        caption = "Area, yield and interaction contributions sum to the total change.")
  }
  labels <- list(title = if (kind == "growth" && nzchar(series)) series else NULL)
  for (field in c("title", "x", "y")) {
    label <- trimws(input[[paste0("gre_pub_", field)]] %||% "")
    if (nzchar(label)) labels[[field]] <- label
  }
  p <- p + do.call(ggplot2::labs, labels)
  p <- raisins_ws_style(p, input$gre_pub_palette %||% "custom")
  base <- input$gre_pub_text_size %||% 10
  font <- input$gre_pub_font %||% "sans"
  theme <- switch(input$gre_pub_theme_value %||% "original",
    minimal = ggplot2::theme_minimal(base_size = base, base_family = font),
    bw = ggplot2::theme_bw(base_size = base, base_family = font),
    ggplot2::theme_classic(base_size = base, base_family = font))
  p + theme + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.2)),
    plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.8),
                                         margin = ggplot2::margin(t = 8)),
    axis.text = ggplot2::element_text(colour = dark),
    plot.margin = ggplot2::margin(10, 12, 9, 10)
  )
}
gre_index_years <- function(r) {
  valid <- Reduce("&", lapply(r$raw[r$series], function(x) is.finite(x) & x > 0))
  r$raw[[r$year_col]][valid]
}

gre_explore_plot <- function(r, input) {
  kind <- input$gre_explore_kind %||% "indexed"
  year <- r$raw[[r$year_col]]
  series <- r$series
  check <- function(test, message) shiny::validate(shiny::need(test, message))
  if (kind == "scatter") {
    check(length(series) >= 2L, "Select at least two series in Analysis Results.")
    x <- input$gre_explore_x %||% series[1]
    y <- input$gre_explore_y %||% series[2]
    if (!x %in% series) x <- series[1]
    if (!y %in% series) y <- series[2]
    check(x != y, "Choose two different series.")
    d <- data.frame(Year = year, X = r$raw[[x]], Y = r$raw[[y]])
    d <- d[is.finite(d$X) & is.finite(d$Y), , drop = FALSE]
    check(nrow(d) >= 2L, "At least two years with both values are needed.")
    return(gre_chart(d, ggplot2::aes(X, Y), "Paired observations by year", x, y) +
      ggplot2::geom_point(ggplot2::aes(colour = Year), size = 3.2) +
      ggplot2::scale_colour_viridis_c(end = 0.86) +
      ggplot2::labs(caption = "Each point is one year. Shared time trends can produce apparent associations."))
  }

  if (kind == "indexed") {
    years <- gre_index_years(r)
    check(length(years) > 0L, "Choose series with positive values in at least one common year.")
    base <- suppressWarnings(as.numeric(input$gre_explore_base %||% years[1]))
    if (!base %in% years) base <- years[1]
    d <- do.call(rbind, lapply(series, function(name) {
      y <- r$raw[[name]]
      segment <- cumsum(c(TRUE, diff(year) != 1 | !is.finite(head(y, -1))))
      data.frame(Year = year, Series = name, Index = 100 * (y / y[year == base]),
                 Segment = segment)
    }))
    d <- d[is.finite(d$Index), , drop = FALSE]
    return(gre_chart(d, ggplot2::aes(Year, Index, colour = Series),
                     "Indexed trajectories", "Year", paste0("Index (", base, " = 100)")) +
      ggplot2::geom_hline(yintercept = 100, colour = "#8795a0", linetype = 2, linewidth = 0.4) +
      ggplot2::geom_line(ggplot2::aes(group = interaction(Series, Segment)), linewidth = 0.8) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_colour_viridis_d(end = 0.85) +
      ggplot2::labs(caption = "All series use the same base year. Line gaps indicate missing values or years."))
  }

  d <- do.call(rbind, lapply(series, function(name) {
    y <- r$raw[[name]]
    previous <- c(NA_real_, head(y, -1))
    eligible <- c(FALSE, diff(year) == 1) & is.finite(previous) & is.finite(y) &
      previous > 0 & y >= 0
    change <- rep(NA_real_, length(y))
    change[eligible] <- 100 * ((y[eligible] - previous[eligible]) / previous[eligible])
    change[!is.finite(change)] <- NA_real_
    data.frame(Year = year, Series = name, Change = change)
  }))
  check(any(is.finite(d$Change)), "No eligible adjacent-year changes are available.")
  extent <- max(abs(d$Change), na.rm = TRUE)
  if (extent == 0) extent <- 1
  d$Series <- factor(d$Series, levels = rev(series))
  gre_chart(d, ggplot2::aes(Year, Series, fill = Change),
            "Annual percentage change", "Year", NULL) +
    ggplot2::geom_tile(width = 0.96, height = 0.8, colour = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradient2(low = "#AC451D", mid = "#FFFFFF", high = "#0072B2",
                                  midpoint = 0, limits = c(-extent, extent), na.value = "#D5DDE4",
                                  name = "Change (%)") +
    ggplot2::labs(caption = "Grey: change unavailable. A valid pair needs adjacent calendar years and a positive starting value.")
}
