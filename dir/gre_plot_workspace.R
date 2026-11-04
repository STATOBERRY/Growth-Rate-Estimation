## ============================================================================
##  RAISINS Growth Rate Estimation — trend-plot workspace (as in RAISINS RBD)
##  Colour patterns, palette styling, hide-legend and the DPI limit come from
##  greyskull (raisins_ws_*). The workspace frame / dialogs and the label
##  editor are RBD's app-local helpers, copied unchanged in look.
## ============================================================================

## ---- workspace frame + dialogs (from RBD dir/rcbd_ui.R) ---------------------
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
          tags$button(type="button",class="close",`data-dismiss`="modal",`aria-label`="Close",tags$span(`aria-hidden`="true","×")),
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
      .raisins-plot-workspace .raisins-plot-heading{display:flex;align-items:center;justify-content:space-between;gap:14px;flex-wrap:wrap;padding:16px 20px;border-bottom:1px solid #e6eaf0;background:#fff;border-radius:12px 12px 0 0;}
      .raisins-plot-workspace .raisins-plot-heading h3{margin:0;font-size:20px;letter-spacing:-.2px;color:#394b66;font-weight:600;}
      .raisins-plot-workspace .raisins-plot-toolbar{display:flex;flex-wrap:wrap;gap:8px;}
      .raisins-plot-workspace .raisins-plot-tool{display:inline-flex;align-items:center;gap:7px;min-height:38px;border-color:#cdd6e2;border-radius:6px;background:#fff;color:#35465f;font-size:13px;padding:8px 11px;transition:background-color .15s,border-color .15s;}
      .raisins-plot-workspace .raisins-plot-tool:hover,.raisins-plot-workspace .raisins-plot-tool:focus{background:#edf7f3;border-color:#568e7f;color:#15594b;}
      .raisins-plot-workspace .raisins-plot-selector{padding:14px 20px 0;max-width:380px;}
      .raisins-plot-workspace .raisins-plot-canvas{padding:10px 18px 18px;overflow-x:auto;}
      .raisins-plot-workspace .raisins-plot-download{padding:16px 20px;background:#f7f9fc;border-top:1px solid #e6eaf0;border-radius:0 0 12px 12px;}
      .raisins-plot-workspace .raisins-plot-export-settings{display:flex;gap:16px;flex-wrap:wrap;}
      .raisins-plot-workspace .raisins-plot-export-settings .form-group{width:145px;margin-bottom:12px;}
      .raisins-plot-workspace .raisins-plot-export-actions{display:flex;align-items:center;justify-content:space-between;gap:14px;flex-wrap:wrap;}
      .raisins-plot-workspace .raisins-plot-export-actions .form-group{margin-bottom:0;}
      .raisins-plot-workspace .raisins-export-heading{font-size:14px;font-weight:600;color:#35495a;margin-bottom:12px;}
      .raisins-plot-workspace .raisins-plot-export-settings label{font-size:12px;color:#536674;}
      .raisins-plot-workspace .raisins-plot-export-actions .download-btn{background:#087f5b;border:1px solid #087f5b;border-radius:6px;box-shadow:none;padding:10px 18px;font-size:14px;color:#fff;}
      .raisins-plot-workspace .raisins-plot-export-actions .download-btn:hover{background:#06694a;}
      .raisins-plot-dialog .modal-dialog{width:560px;max-width:calc(100% - 24px);margin:24px auto;pointer-events:auto;overflow:visible;transition:none;}
      .raisins-plot-dialog .modal-content{display:flex;flex-direction:column;max-height:calc(100vh - 24px);border:1px solid #dce3e7;border-radius:10px;box-shadow:0 16px 48px rgba(29,46,61,.18);overflow:hidden;}
      .raisins-plot-dialog .modal-header,.raisins-plot-dialog .modal-footer{padding:12px 18px;flex-shrink:0;}
      .raisins-plot-dialog .modal-header{cursor:move;touch-action:none;user-select:none;background:#f8fafb;border-bottom:1px solid #e5eaee;}
      .raisins-plot-dialog .modal-header .close{cursor:pointer;touch-action:auto;}
      .raisins-plot-dialog .raisins-plot-drag-hint{display:block;margin-top:4px;font-size:11px;color:#617085;}
      .raisins-plot-dialog .modal-title{font-size:17px;font-weight:600;color:#263b4d;}
      .raisins-plot-dialog .modal-body{max-height:calc(100vh - 175px);overflow-y:auto;padding:14px 18px;font-size:14px;min-height:0;flex:1 1 auto;}
      .raisins-plot-dialog .modal-body .form-group{margin-bottom:10px;}
      .raisins-plot-dialog .modal-body .shiny-input-container{max-width:100%;}
      .raisins-plot-dialog .modal-body p,.raisins-plot-dialog .help-block{color:#5c6d79;font-size:13px;line-height:1.5;}
      .raisins-plot-dialog .modal-footer{background:#fafcfc;border-top:1px solid #e5eaee;}
      .raisins-plot-dialog .modal-footer .btn-primary{background:#087f5b;border-color:#087f5b;border-radius:6px;padding:7px 22px;}
      .raisins-plot-dialog .form-control{border-color:#d6dfe5;border-radius:5px;box-shadow:none;}
      .raisins-plot-dialog .form-control:focus{border-color:#388873;box-shadow:0 0 0 2px rgba(56,136,115,.12);}
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
      .raisins-plot-dialog .raisins-label-list{padding:0 4px;}
      .raisins-plot-dialog .raisins-label-list .control-label{position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0;}
      .raisins-plot-dialog .raisins-label-list .row{margin:0 0 6px;padding:8px 0 0;border:1px solid #e5eaee;border-radius:6px;background:#fafcfc;}
      .raisins-plot-dialog .raisins-label-original{padding-top:8px;overflow-wrap:anywhere;font-size:13px;color:#536674;}
      .raisins-plot-dialog .raisins-label-column-head{display:grid;grid-template-columns:5fr 7fr;padding:0 18px 8px;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.6px;color:#637685;}
      @media(max-width:520px){.raisins-plot-dialog .raisins-label-column-head{display:none;}}
      @media(max-width:600px){.raisins-plot-workspace .raisins-plot-heading,.raisins-plot-workspace .raisins-plot-download{padding:14px;}.raisins-plot-workspace .raisins-plot-toolbar{width:100%;}.raisins-plot-workspace .raisins-plot-tool{flex:1 1 44%;justify-content:center;}.raisins-plot-dialog .modal-body{max-height:65vh;}.raisins-plot-workspace .raisins-plot-export-settings .form-group{flex:1 1 90px;width:auto;}.raisins-plot-workspace .raisins-plot-export-actions .download-btn{width:100%;text-align:center;}}
    ")),
    tags$script(src=paste0("raisins-plot-dialog.js?v=",unname(tools::md5sum("www/raisins-plot-dialog.js")))),
    tags$script(HTML("$(document).on('show.bs.modal.raisinsPlotTools shown.bs.modal.raisinsPlotTools','.raisins-plot-dialog',function(e){this.scrollTop=0;$(this).find('.modal-body').scrollTop(0);if(e.type==='shown'){var heading=$(this).find('.modal-title').attr('tabindex','-1')[0];if(heading){try{heading.focus({preventScroll:true});}catch(err){heading.focus();}this.scrollTop=0;$(this).find('.modal-body').scrollTop(0);}}});"))
  )),div(class="raisins-plot-workspace",
    div(class="raisins-plot-heading",h3(title),div(class="raisins-plot-toolbar",toolbar)),
    div(class="raisins-plot-selector",selector),div(class="raisins-plot-canvas",plot),
    div(class="raisins-plot-download",download),dialogs))
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
