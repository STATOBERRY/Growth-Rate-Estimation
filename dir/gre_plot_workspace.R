## ============================================================================
##  RAISINS Growth Rate Estimation and Trend Analysis - trend-plot workspace (as in RAISINS RBD)
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
          tags$small(class="raisins-plot-drag-hint","Click and drag title bar to move \u00b7 Drag edges or corners to resize")),
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
      .raisins-plot-workspace .raisins-plot-export-settings .form-group{flex:1 1 190px;max-width:260px;margin-bottom:12px;}
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

## ---- shared plot workspace ------------------------------------------------
## All six picture buttons use this: one builder per button returns a ggplot, and
## the same object is drawn on screen, shown interactively, downloaded and put in
## the report. Inputs are named gre_ws_<key>_<field>.
gre_ws_id <- function(key, field) paste0("gre_ws_", key, "_", field)

gre_ws_defaults <- function(key) {
  pub <- startsWith(key, "fig_")
  list(point = 2.5, line = 0.9, text = if (pub) 11 else 13,
       theme = if (pub) "classic" else "minimal",
       width = if (pub) 7 else 8, height = if (pub) 4.5 else 5,
       dpi = if (pub) 600 else 300, format = "png",
       screen = 480)
}

gre_ws_toggle <- function(id, label, value = FALSE, title = NULL) {
  div(class = "raisins-hide-legend", title = title, checkboxInput(id, label, value))
}

## The Plot settings dialog: chart-specific options first, then the common ones.
gre_ws_settings_ui <- function(key, extra = NULL) {
  id <- function(field) gre_ws_id(key, field)
  dv <- gre_ws_defaults(key)
  half <- function(a, b) fluidRow(column(6, a), column(6, b))
  tagList(
    extra,
    if (!is.null(extra)) tags$hr(),
    tags$h4("Points and lines"),
    half(sliderInput(id("point"), "Point size", 0.5, 8, dv$point, step = 0.5, ticks = FALSE),
         sliderInput(id("line"), "Line width", 0.25, 3, dv$line, step = 0.05, ticks = FALSE)),
    half(selectInput(id("shape"), "Point shape",
           c("Filled circle" = "16", "Open circle" = "21", "Square" = "15",
             "Triangle" = "17", "Diamond" = "18"), selectize = FALSE),
         selectInput(id("linetype"), "Line type",
           c("Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted",
             "Long dash" = "longdash"), selectize = FALSE)),
    tags$hr(), tags$h4("Text"),
    half(sliderInput(id("text"), "Text size", 8, 24, dv$text, ticks = FALSE),
         selectInput(id("font"), "Font",
           c("Sans serif" = "sans", "Serif" = "serif", "Monospace" = "mono"), selectize = FALSE)),
    half(checkboxInput(id("show_title"), "Show plot title", TRUE),
         checkboxInput(id("show_note"), "Show the note under the plot", TRUE)),
    tags$hr(), tags$h4("Layout"),
    half(selectInput(id("legend"), "Legend position",
           c("Bottom" = "bottom", "Right" = "right", "Top" = "top", "Left" = "left"), selectize = FALSE),
         selectInput(id("grid"), "Gridlines",
           c("Major only" = "major", "Major and minor" = "both", "None" = "none"), selectize = FALSE)),
    half(selectInput(id("angle"), "Horizontal-axis label angle",
           c("Level" = "0", "45 degrees" = "45", "90 degrees" = "90"), selectize = FALSE),
         numericInput(id("year_step"), "Years between axis labels (0 = automatic)", 0, min = 0, max = 50, step = 1)),
    half(numericInput(id("value_min"), "Value axis minimum (blank = automatic)", NA),
         numericInput(id("value_max"), "Value axis maximum (blank = automatic)", NA)),
    sliderInput(id("screen"), "Plot height on screen (pixels)", 300, 900, dv$screen, step = 20, ticks = FALSE))
}

gre_ws_ui <- function(key, title, selector = NULL, extra = NULL, series_labels = TRUE) {
  id <- function(field) gre_ws_id(key, field)
  dv <- gre_ws_defaults(key)
  raisins_plot_workspace_ui(title,
    toolbar = tagList(
      raisins_plot_dialog_button(id("d_settings"), "Plot settings", "cog"),
      raisins_plot_dialog_button(id("d_labels"), "Change labels", "pen"),
      raisins_plot_dialog_button(id("d_colours"), "Colours", "palette"),
      raisins_plot_dialog_button(id("d_theme"), "Change theme", "adjust"),
      raisins_ws_legend_ui(id("hide_legend")),
      gre_ws_toggle(id("interactive"), "Interactive view", FALSE,
                    "Zoom and read values by hovering. Downloads always use the standard figure.")),
    selector = selector,
    plot = tagList(
      conditionalPanel(sprintf("!input['%s']", id("interactive")),
        shinycssloaders::withSpinner(plotOutput(id("plot"), height = "auto"), hide.ui = FALSE)),
      conditionalPanel(sprintf("input['%s']", id("interactive")),
        plotly::plotlyOutput(id("ly"), height = "auto"))),
    download = uiOutput(id("dl_ui")),
    dialogs = tagList(
      raisins_plot_dialog_ui(id("d_settings"), paste(title, "settings"), gre_ws_settings_ui(key, extra)),
      raisins_plot_dialog_ui(id("d_labels"), "Change labels",
        tags$p("Changes affect this figure only; data and statistical results stay unchanged."),
        textInput(id("title"), "Plot title", placeholder = "Use the original plot label"),
        textInput(id("x"), "Horizontal-axis title", placeholder = "Use the original plot label"),
        textInput(id("y"), "Vertical-axis title", placeholder = "Use the original plot label"),
        textAreaInput(id("note"), "Note under the plot", rows = 2, placeholder = "Use the original note"),
        if (series_labels) tagList(
          tags$hr(), tags$h4("Series and group labels"),
          div(class = "raisins-label-column-head", tags$span("Original name"), tags$span("Display label")),
          div(class = "raisins-label-list", uiOutput(NS(id("edit"), "fields"))),
          actionButton(NS(id("edit"), "reset"), "Reset labels", icon = icon("undo"),
                       class = "btn-default btn-sm"))),
      raisins_plot_dialog_ui(id("d_colours"), "Colours",
        raisins_ws_colours_ui(id("preset")),
        tags$hr(),
        tags$p("With Use my plot settings selected, these two colours lead the figure."),
        fluidRow(
          column(6, colourpicker::colourInput(id("accent"), "Main colour", "#0072B2", showColour = "both")),
          column(6, colourpicker::colourInput(id("contrast"), "Second colour", "#B34A00", showColour = "both")))),
      raisins_plot_dialog_ui(id("d_theme"), "Change theme",
        selectInput(id("theme"), "Theme",
          c("Minimal" = "minimal", "Classic" = "classic", "Black and white" = "bw",
            "Light" = "light", "Grey" = "grey"),
          selected = dv$theme, selectize = FALSE))))
}

## Export controls, drawn once the figure can be built.
gre_ws_export_ui <- function(key, input) {
  id <- function(field) gre_ws_id(key, field)
  dv <- gre_ws_defaults(key)
  tagList(
    div(class = "raisins-export-heading", "Download figure"),
    div(class = "raisins-plot-export-settings",
      sliderInput(id("width"), "Width (inches)", 1, 20, isolate(input[[id("width")]]) %||% dv$width, step = 0.5),
      sliderInput(id("height"), "Height (inches)", 1, 20, isolate(input[[id("height")]]) %||% dv$height, step = 0.5),
      raisins_ws_limit_dpi(sliderInput(id("dpi"), "DPI (max 600)", 72, 600,
                                       isolate(input[[id("dpi")]]) %||% dv$dpi, step = 50))),
    div(class = "raisins-plot-export-actions",
      radioButtons(id("format"), "Format:",
        c("PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "PDF" = "pdf", "SVG" = "svg"),
        selected = isolate(input[[id("format")]]) %||% dv$format, inline = TRUE),
      downloadButton(id("dl"), "Download Plot", class = "download-btn")))
}

## ---- reading the choices ---------------------------------------------------
gre_ws_opts <- function(input, key, labels = character()) {
  dv <- gre_ws_defaults(key)
  get <- function(field, default = NULL) {
    v <- input[[gre_ws_id(key, field)]]
    if (is.null(v) || !length(v) || identical(v, "")) default else v
  }
  num <- function(field, default = NA_real_) {
    v <- suppressWarnings(as.numeric(get(field, default)))
    if (length(v) == 1L && is.finite(v)) v else default
  }
  text <- function(field) trimws(paste(get(field, ""), collapse = " "))
  preset <- get("preset", "custom")
  if (!raisins_ws_colour_valid(preset)) preset <- "custom"
  hex <- function(field, default) {
    v <- get(field, default)
    if (is.character(v) && length(v) == 1L && grepl("^#[0-9A-Fa-f]{6}$", v)) v else default
  }
  accent <- hex("accent", "#0072B2")
  contrast <- hex("contrast", "#B34A00")
  if (preset != "custom") {
    pair <- raisins_ws_palette(preset, 2L, TRUE)
    accent <- pair[1]; contrast <- pair[2]
  }
  list(key = key, get = get, labels = labels, preset = preset, accent = accent,
       contrast = contrast, dark = "#263746",
       point = num("point", dv$point), line = num("line", dv$line),
       shape = as.integer(get("shape", "16")), linetype = get("linetype", "solid"),
       text = num("text", dv$text), font = get("font", "sans"),
       show_title = !isFALSE(get("show_title", TRUE)), show_note = !isFALSE(get("show_note", TRUE)),
       legend = if (isTRUE(get("hide_legend", FALSE))) "none" else get("legend", "bottom"),
       grid = get("grid", "major"), angle = as.numeric(get("angle", "0")),
       year_step = num("year_step", 0), value_min = num("value_min"), value_max = num("value_max"),
       theme = get("theme", dv$theme), title = text("title"), x = text("x"), y = text("y"), note = text("note"))
}

## Discrete colours: the two chosen colours lead, then a colour-blind safe set.
gre_ws_cols <- function(o, n) {
  if (o$preset != "custom") return(raisins_ws_palette(o$preset, n, TRUE))
  rep(c(o$accent, o$contrast, "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#D55E00", "#6B6B6B"),
      length.out = n)
}

## Display label for a name, wrapped so long names cannot crowd the figure out.
gre_ws_label <- function(o, x, wrap = 30L) {
  x <- as.character(x)
  i <- match(x, names(o$labels))
  x[!is.na(i)] <- unname(o$labels[i[!is.na(i)]])
  vapply(x, function(s) paste(strwrap(s, width = wrap), collapse = "\n"), "", USE.NAMES = FALSE)
}
gre_ws_factor <- function(o, x, levels = unique(as.character(x))) {
  factor(gre_ws_label(o, x), levels = unique(gre_ws_label(o, levels)))
}

gre_ws_year_breaks <- function(step, n = 5L) function(limits) {
  if (!all(is.finite(limits))) return(numeric())
  if (step > 0) return(seq(ceiling(limits[1] / step) * step, limits[2], by = step))
  b <- pretty(limits, n = n)
  b[b == round(b)]
}

gre_ws_wrap_note <- function(text, width = 110L) {
  if (is.null(text)) return(NULL)
  paste(unlist(lapply(strsplit(text, "
", fixed = TRUE)[[1]], strwrap, width = width)), collapse = "
")
}

## Applies everything the chart types have in common.
gre_ws_finish <- function(p, o, title = NULL, x = NULL, y = NULL, note = NULL,
                          value_axis = NULL, year_x = FALSE, year_n = 5L) {
  pick <- function(own, default) if (nzchar(own)) own else default
  p <- p + ggplot2::labs(
    title = if (o$show_title) pick(o$title, title),
    x = pick(o$x, x), y = pick(o$y, y),
    caption = if (o$show_note) gre_ws_wrap_note(pick(o$note, note)))
  if (year_x) p <- p + ggplot2::scale_x_continuous(breaks = gre_ws_year_breaks(o$year_step, year_n))
  limits <- c(o$value_min, o$value_max)
  if (!is.null(value_axis) && any(is.finite(limits)))
    p <- p + if (value_axis == "y") ggplot2::coord_cartesian(ylim = limits) else ggplot2::coord_cartesian(xlim = limits)
  theme <- switch(o$theme, classic = ggplot2::theme_classic, bw = ggplot2::theme_bw,
                  light = ggplot2::theme_light, grey = ggplot2::theme_grey, ggplot2::theme_minimal)
  p <- p + theme(base_size = o$text, base_family = o$font) + ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15)),
    plot.caption = ggplot2::element_text(hjust = 0, size = ggplot2::rel(0.8), margin = ggplot2::margin(t = 8)),
    legend.position = o$legend, legend.title = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 14, 9, 10))
  if (o$grid != "both") p <- p + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (o$grid == "none") p <- p + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  if (o$angle > 0) p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
    angle = o$angle, hjust = 1, vjust = if (o$angle == 90) 0.5 else 1))
  p
}

gre_ws_points <- function(o, mapping = NULL, ..., colour = NULL) {
  args <- list(mapping = mapping, size = o$point, shape = o$shape, ...)
  if (o$shape == 21L) args$fill <- "white"
  if (!is.null(colour)) args$colour <- colour
  do.call(ggplot2::geom_point, args)
}

## ---- builders, one per picture button --------------------------------------
gre_ws_check <- function(test, message) shiny::validate(shiny::need(test, message))

gre_ws_build_trend <- function(r, o) {
  gre_ws_check(length(r$trend_fits) > 0, "No trend has been fitted. Tick Trend fitting on the Data & Settings tab; a series also needs enough years and must not be constant.")
  fits <- names(r$trend_fits)
  show <- intersect(o$get("series", fits), fits)
  gre_ws_check(length(show) > 0, "Choose at least one series.")
  dd <- do.call(rbind, lapply(show, function(s) {
    f <- r$trend_fits[[s]]$fitted[c("year", "y", "fit")]; f$Series <- s; f }))
  # Series on very different scales are unreadable on one axis.
  sizes <- vapply(show, function(s) stats::median(abs(dd$y[dd$Series == s]), na.rm = TRUE), 0)
  auto <- length(show) > 1L && all(is.finite(sizes)) && max(sizes) > 5 * max(min(sizes), .Machine$double.eps)
  panels <- switch(o$get("layout", "auto"), panels = length(show) > 1L, same = FALSE, auto)
  dd$Series <- gre_ws_factor(o, dd$Series, show)
  p <- ggplot2::ggplot(dd, ggplot2::aes(year, colour = Series))
  if (!isFALSE(o$get("fit", TRUE)))
    p <- p + ggplot2::geom_line(ggplot2::aes(y = fit), linewidth = o$line, linetype = o$linetype)
  if (!isFALSE(o$get("points", TRUE))) p <- p + gre_ws_points(o, ggplot2::aes(y = y))
  p <- p + ggplot2::scale_colour_manual(values = gre_ws_cols(o, nlevels(dd$Series)))
  if (panels) p <- p + ggplot2::facet_wrap(ggplot2::vars(Series), scales = "free_y")
  gre_ws_finish(p, o, "Observed values with best-fit trend", r$year_col, "Value",
                note = "Points: observations. Lines: the fit with the lowest AICc for each series.",
                value_axis = "y", year_x = TRUE, year_n = if (panels) 3L else 5L)
}

gre_ws_build_pub <- function(r, o, kind) {
  choices <- switch(kind, trend = names(r$trend_fits), growth = unique(r$group_comparison$Series), character())
  series <- o$get("series", "")
  # The selector's last value outlives it, so never trust it when it is gone.
  if (!series %in% choices) series <- if (length(choices)) choices[1] else ""
  if (kind == "trend") {
    fit <- r$trend_fits[[series]]
    gre_ws_check(!is.null(fit), "No trend has been fitted. Tick Trend fitting on the Data & Settings tab; a series also needs enough years and must not be constant.")
    p <- ggplot2::ggplot(fit$fitted, ggplot2::aes(year)) +
      ggplot2::geom_line(ggplot2::aes(y = fit), colour = o$accent, linewidth = o$line, linetype = o$linetype) +
      gre_ws_points(o, ggplot2::aes(y = y), colour = o$dark)
    return(gre_ws_finish(p, o, NULL, "Year", gre_ws_label(o, series, 60L),
      paste0("Points: observations. Line: ", tolower(fit$best), " fit selected by AICc."),
      value_axis = "y", year_x = TRUE))
  }
  if (kind == "growth") {
    d <- r$group_comparison
    if (is.null(d)) {
      d <- r$growth
      gre_ws_check(!is.null(d), "Tick Growth rates on the Data & Settings tab and run the analysis to build this figure.")
      d$Group <- d$Series
      d$Scope <- "Group"
    } else d <- d[d$Series == series, , drop = FALSE]
    d <- d[is.finite(d$`CGR (%/yr)`), , drop = FALSE]
    gre_ws_check(nrow(d) > 0, "No compound growth estimates are available. CGR needs positive values.")
    d$Label <- gre_ws_factor(o, d$Group, rev(as.character(d$Group)))
    ci <- d[is.finite(d$`CGR CI low`) & is.finite(d$`CGR CI high`), , drop = FALSE]
    p <- ggplot2::ggplot(d, ggplot2::aes(`CGR (%/yr)`, Label)) +
      ggplot2::geom_vline(xintercept = 0, colour = "#777777", linetype = "dashed", linewidth = 0.35) +
      ggplot2::geom_segment(data = ci, ggplot2::aes(x = `CGR CI low`, xend = `CGR CI high`, yend = Label),
                            colour = o$dark, linewidth = o$line) +
      ggplot2::geom_point(ggplot2::aes(shape = Scope), colour = o$accent, size = o$point) +
      ggplot2::scale_shape_manual(values = c(Combined = 18, Group = o$shape), guide = "none")
    return(gre_ws_finish(p, o, if (nzchar(series)) gre_ws_label(o, series, 60L),
      "Compound growth (% per year)", NULL,
      paste0("Bars: ", 100 * (1 - r$alpha), "% model confidence intervals where available.",
             if (!is.null(r$group_comparison)) "\nDiamond: combined series. Other points: individual groups."),
      value_axis = "x"))
  }
  if (kind == "instability") {
    d <- r$instab
    gre_ws_check(!is.null(d), "Tick Instability on the Data & Settings tab and run the analysis to build this figure.")
    d <- d[is.finite(d$`CV (%)`) & is.finite(d$`CDVI (%)`), , drop = FALSE]
    gre_ws_check(nrow(d) > 0, "No valid instability estimates are available. They need positive means.")
    d$Label <- gre_ws_factor(o, d$Series, rev(d$Series))
    p <- ggplot2::ggplot(d, ggplot2::aes(y = Label)) +
      ggplot2::geom_segment(ggplot2::aes(x = `CV (%)`, xend = `CDVI (%)`, yend = Label),
                            colour = "#888888", linewidth = o$line) +
      ggplot2::geom_point(ggplot2::aes(x = `CV (%)`), shape = 21, colour = o$contrast, fill = "white", size = o$point) +
      ggplot2::geom_point(ggplot2::aes(x = `CDVI (%)`), shape = 18, colour = o$accent, size = o$point)
    return(gre_ws_finish(p, o, NULL, "Variability (%)", NULL,
      "Open circles: CV. Diamonds: Cuddy-Della Valle Index (CDVI).", value_axis = "x"))
  }
  d <- r$decomp
  gre_ws_check(!is.null(d), r$decomp_msg %||% "Tick Area-Yield decomposition on the Data & Settings tab and run the analysis to build this figure.")
  d$Label <- gre_ws_factor(o, d$Effect, rev(as.character(d$Effect)))
  d$Direction <- ifelse(d$Contribution < 0, "Decrease", "Increase")
  p <- ggplot2::ggplot(d, ggplot2::aes(Contribution, Label, fill = Direction)) +
    ggplot2::geom_col(width = 0.55, colour = o$dark, linewidth = 0.25) +
    ggplot2::geom_vline(xintercept = 0, colour = o$dark, linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c(Increase = o$accent, Decrease = o$contrast), guide = "none")
  gre_ws_finish(p, o, NULL, "Contribution (production units)", NULL,
    "Area, yield and interaction contributions sum to the total change.", value_axis = "x")
}

gre_index_years <- function(r) {
  valid <- Reduce("&", lapply(r$raw[r$series], function(x) is.finite(x) & x > 0))
  r$raw[[r$year_col]][valid]
}

gre_ws_build_explore <- function(r, o) {
  kind <- o$get("kind", "indexed")
  year <- r$raw[[r$year_col]]
  series <- r$series
  if (kind == "scatter") {
    gre_ws_check(length(series) >= 2L, "The paired scatter needs at least two series. Select another series on the Data & Settings tab and run the analysis again.")
    x <- o$get("x_series", series[1])
    y <- o$get("y_series", series[2])
    if (!x %in% series) x <- series[1]
    if (!y %in% series) y <- series[2]
    gre_ws_check(x != y, "Choose two different series.")
    d <- data.frame(Year = year, X = r$raw[[x]], Y = r$raw[[y]])
    d <- d[is.finite(d$X) & is.finite(d$Y), , drop = FALSE]
    gre_ws_check(nrow(d) >= 2L, "At least two years with both values are needed.")
    ramp <- if (o$preset == "custom") c(o$accent, o$contrast) else raisins_ws_palette(o$preset, 7L, TRUE)
    p <- ggplot2::ggplot(d, ggplot2::aes(X, Y, colour = Year)) +
      ggplot2::geom_point(size = o$point, shape = if (o$shape == 21L) 16L else o$shape) +
      ggplot2::scale_colour_gradientn(colours = ramp, breaks = function(l) { b <- pretty(l, n = 3L); b[b == round(b)] })
    return(gre_ws_finish(p, o, "Paired observations by year", gre_ws_label(o, x, 60L), gre_ws_label(o, y, 60L),
      "Each point is one year. Shared time trends can produce apparent associations.", value_axis = "y"))
  }
  if (kind == "indexed") {
    years <- gre_index_years(r)
    gre_ws_check(length(years) > 0L, "There is no common year with positive values for all selected series.")
    base <- suppressWarnings(as.numeric(o$get("base", years[1])))
    if (!base %in% years) base <- years[1]
    d <- do.call(rbind, lapply(series, function(name) {
      y <- r$raw[[name]]
      segment <- cumsum(c(TRUE, diff(year) != 1 | !is.finite(head(y, -1))))
      data.frame(Year = year, Series = name, Index = 100 * (y / y[year == base]), Segment = segment)
    }))
    d <- d[is.finite(d$Index), , drop = FALSE]
    d$Line <- paste(d$Series, d$Segment)
    d$Series <- gre_ws_factor(o, d$Series, series)
    p <- ggplot2::ggplot(d, ggplot2::aes(Year, Index, colour = Series)) +
      ggplot2::geom_hline(yintercept = 100, colour = "#8795a0", linetype = 2, linewidth = 0.4) +
      ggplot2::geom_line(ggplot2::aes(group = Line), linewidth = o$line, linetype = o$linetype) +
      gre_ws_points(o) +
      ggplot2::scale_colour_manual(values = gre_ws_cols(o, nlevels(d$Series)))
    return(gre_ws_finish(p, o, "Indexed trajectories", "Year", paste0("Index (", base, " = 100)"),
      "All series use the same base year. Line gaps indicate missing values or years.",
      value_axis = "y", year_x = TRUE))
  }
  d <- do.call(rbind, lapply(series, function(name) {
    y <- r$raw[[name]]
    previous <- c(NA_real_, head(y, -1))
    eligible <- c(FALSE, diff(year) == 1) & is.finite(previous) & is.finite(y) & previous > 0 & y >= 0
    change <- rep(NA_real_, length(y))
    change[eligible] <- 100 * ((y[eligible] - previous[eligible]) / previous[eligible])
    change[!is.finite(change)] <- NA_real_
    data.frame(Year = year, Series = name, Change = change)
  }))
  gre_ws_check(any(is.finite(d$Change)), "No eligible adjacent-year changes are available.")
  extent <- max(abs(d$Change), na.rm = TRUE)
  if (extent == 0) extent <- 1
  d$Series <- gre_ws_factor(o, d$Series, rev(series))
  p <- ggplot2::ggplot(d, ggplot2::aes(Year, Series, fill = Change)) +
    ggplot2::geom_tile(width = 0.96, height = 0.8, colour = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradient2(low = o$contrast, mid = "#FFFFFF", high = o$accent, midpoint = 0,
                                  limits = c(-extent, extent), na.value = "#D5DDE4", name = "Change (%)")
  gre_ws_finish(p, o, "Annual percentage change", "Year", NULL,
    "Grey: change unavailable. A valid pair needs adjacent calendar years and a positive starting value.",
    year_x = TRUE) + ggplot2::theme(legend.title = ggplot2::element_text())
}

gre_ws_build_diag <- function(r, o) {
  all_series <- r$residuals
  gre_ws_check(length(all_series) > 0, "No growth residuals are available. Tick Growth rates on the Data & Settings tab; a series also needs positive values.")
  series <- o$get("series", names(all_series)[1])
  if (!series %in% names(all_series)) series <- names(all_series)[1]
  d <- all_series[[series]]
  if (o$get("kind", "residual") == "lag") {
    adjacent <- diff(d$Year) == 1
    lagged <- data.frame(Previous = head(d$Residual, -1)[adjacent], Current = tail(d$Residual, -1)[adjacent])
    gre_ws_check(nrow(lagged) >= 3, "The one-year lag plot needs at least three pairs of adjacent years.")
    p <- ggplot2::ggplot(lagged, ggplot2::aes(Previous, Current)) +
      ggplot2::geom_hline(yintercept = 0, colour = "#a9b6bc", linetype = 2) +
      ggplot2::geom_vline(xintercept = 0, colour = "#a9b6bc", linetype = 2) +
      gre_ws_points(o, colour = o$contrast)
    return(gre_ws_finish(p, o, paste("One-year residual lag:", series), "Previous year", "Current year",
      "A clear upward or downward pattern warns that the CGR interval and p-value may be unreliable.",
      value_axis = "y"))
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(Year, Residual)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#83949b", linetype = 2) +
    ggplot2::geom_line(colour = o$accent, linewidth = o$line, linetype = o$linetype) +
    gre_ws_points(o, colour = o$accent)
  gre_ws_finish(p, o, paste("Residuals over time:", series), "Year", "Log-scale residual",
    "Residuals of the log-linear trend. They should scatter evenly around zero.",
    value_axis = "y", year_x = TRUE)
}

gre_ws_build_group <- function(r, o) {
  gre_ws_check(!is.null(r$group_comparison), "Choose a grouping column on the Data & Settings tab, tick Growth rates and run the analysis to see group plots.")
  choices <- unique(r$group_comparison$Series)
  series <- o$get("series", choices[1])
  if (!series %in% choices) series <- choices[1]
  if (o$get("kind", "forest") == "paths") {
    d <- r$group_data
    d <- d[d$Series == series & d$Scope == "Group", , drop = FALSE]
    gre_ws_check(any(is.finite(d$Value)), "No observations are available for this series.")
    d$Label <- gre_ws_factor(o, d$Group)
    p <- ggplot2::ggplot(d, ggplot2::aes(Year, Value, colour = Label)) +
      ggplot2::geom_line(linewidth = o$line, linetype = o$linetype) + gre_ws_points(o) +
      ggplot2::scale_colour_manual(values = gre_ws_cols(o, nlevels(d$Label)))
    return(gre_ws_finish(p, o, "Group time series", "Year", gre_ws_label(o, series, 60L),
                         value_axis = "y", year_x = TRUE))
  }
  d <- r$group_comparison
  d <- d[d$Series == series & is.finite(d$`CGR (%/yr)`), , drop = FALSE]
  gre_ws_check(nrow(d) > 0, "No compound growth rate could be estimated for this series.")
  d$Label <- gre_ws_factor(o, d$Group, rev(as.character(d$Group)))
  ci <- d[is.finite(d$`CGR CI low`) & is.finite(d$`CGR CI high`), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(`CGR (%/yr)`, Label, colour = Scope)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#83949b", linetype = 2) +
    ggplot2::geom_segment(data = ci, ggplot2::aes(x = `CGR CI low`, xend = `CGR CI high`, yend = Label),
                          linewidth = o$line) +
    gre_ws_points(o) +
    ggplot2::scale_colour_manual(values = c(Combined = o$contrast, Group = o$accent))
  gre_ws_finish(p, o, paste("Compound growth by group:", gre_ws_label(o, series, 60L)), "CGR (% per year)", NULL,
    paste0("Bars: ", 100 * (1 - r$alpha), "% model confidence intervals where available."), value_axis = "x")
}

gre_ws_build_decomp <- function(r, o) {
  d <- r$decomp
  gre_ws_check(!is.null(d), r$decomp_msg %||% "Tick Area-Yield decomposition on the Data & Settings tab and run the analysis to see this plot.")
  n <- nrow(d)
  value <- d$Contribution
  total <- seq_len(n) == n
  start <- c(0, cumsum(value[-n]))
  start[total] <- 0
  end <- start + value
  w <- data.frame(i = seq_len(n), start = start, end = end, low = pmin(start, end), high = pmax(start, end),
                  value = value,
                  Type = factor(ifelse(total, "Total", ifelse(value < 0, "Decrease", "Increase")),
                                levels = c("Increase", "Decrease", "Total")))
  p <- ggplot2::ggplot(w) +
    ggplot2::geom_hline(yintercept = 0, colour = "#83949b", linewidth = 0.35) +
    ggplot2::geom_rect(ggplot2::aes(xmin = i - 0.36, xmax = i + 0.36, ymin = low, ymax = high, fill = Type)) +
    ggplot2::scale_fill_manual(values = c(Increase = o$accent, Decrease = o$contrast, Total = o$dark)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.12))) +
    ggplot2::scale_x_continuous(breaks = w$i, labels = gre_ws_label(o, d$Effect, 18L))
  if (n > 1L)
    p <- p + ggplot2::geom_segment(data = w[-n, ], ggplot2::aes(x = i + 0.36, xend = i + 0.64, y = end, yend = end),
                                   colour = "#83949b", linewidth = 0.3)
  if (!isFALSE(o$get("values", TRUE)))
    p <- p + ggplot2::geom_text(ggplot2::aes(x = i, y = high, label = format(round(value, 2), big.mark = ",", trim = TRUE)),
                                vjust = -0.5, size = o$text / 3.4, family = o$font, colour = o$dark)
  gre_ws_finish(p, o, "From area and yield to total production change", NULL, "Change in production units",
    "Each bar starts where the previous one ended; the last bar is the total change.", value_axis = "y")
}

## ---- figures from recent crop growth and trend studies ----------------------
gre_ws_titles <- c(
  fig_trend = "Observed Values and Fitted Trend",
  fig_growth = "Compound Growth with Confidence Intervals",
  fig_instab = "Variability and Trend-Adjusted Instability",
  fig_contrib = "Area-Yield Contributions",
  period = "Period-Wise Growth and Instability",
  map = "Growth-Instability Map",
  change = "Change Point (Pettitt Test)",
  ita = "Innovative Trend Analysis",
  sen = "Mann-Kendall and Sen's Slope Trend")

## One series' usable years, in time order.
gre_ws_series_data <- function(r, series) {
  d <- data.frame(Year = suppressWarnings(as.numeric(r$raw[[r$year_col]])),
                  y = suppressWarnings(as.numeric(r$raw[[series]])))
  d <- d[is.finite(d$Year) & is.finite(d$y), , drop = FALSE]
  d[order(d$Year), , drop = FALSE]
}
gre_ws_pick_series <- function(r, o) {
  series <- o$get("series", r$series[1])
  if (series %in% r$series) series else r$series[1]
}

## Pettitt (1979) test for one abrupt change at an unknown time.
gre_pettitt <- function(y) {
  n <- length(y)
  if (n < 6L) return(NULL)
  U <- vapply(seq_len(n - 1L), function(k) sum(sign(outer(y[seq_len(k)], y[(k + 1L):n], function(a, b) b - a))), 0)
  K <- max(abs(U))
  list(at = which.max(abs(U)), K = K, p = min(1, 2 * exp(-6 * K^2 / (n^3 + n^2))), n = n)
}

gre_ws_periods <- function(years, breaks) {
  lo <- min(years); hi <- max(years)
  breaks <- sort(unique(suppressWarnings(as.numeric(breaks))))
  breaks <- breaks[is.finite(breaks) & breaks > lo & breaks < hi]
  if (!length(breaks)) breaks <- floor((lo + hi) / 2)
  starts <- c(lo, breaks + 1)
  ends <- c(breaks, hi)
  c(lapply(seq_along(starts), function(i) c(starts[i], ends[i])), list(c(lo, hi)))
}

gre_ws_build_period <- function(r, o) {
  years <- suppressWarnings(as.numeric(r$raw[[r$year_col]]))
  years <- years[is.finite(years)]
  periods <- gre_ws_periods(years, o$get("breaks", NULL))
  growth <- o$get("kind", "cgr") == "cgr"
  rows <- do.call(rbind, lapply(r$series, function(s) {
    d <- gre_ws_series_data(r, s)
    do.call(rbind, lapply(periods, function(span) {
      part <- d[d$Year >= span[1] & d$Year <= span[2], , drop = FALSE]
      if (nrow(part) < 3L) return(NULL)
      label <- paste0(span[1], "-", span[2])
      if (growth) {
        cg <- gre_cgr(part$Year, part$y, r$alpha)
        data.frame(Period = label, Series = s, value = cg$rate, low = cg$ci[1], high = cg$ci[2])
      } else {
        data.frame(Period = label, Series = s, value = gre_instability(part$Year, part$y)$cdvi, low = NA_real_, high = NA_real_)
      }
    }))
  }))
  if (!is.null(rows)) rows <- rows[is.finite(rows$value), , drop = FALSE]
  gre_ws_check(!is.null(rows) && nrow(rows) > 0,
    if (growth) "No period has a compound growth rate. Each period needs at least three years of positive values."
    else "No period has an instability value. Each period needs at least three usable years and a positive mean.")
  order_periods <- vapply(periods, function(span) paste0(span[1], "-", span[2]), "")
  rows$Period <- factor(rows$Period, levels = order_periods[order_periods %in% rows$Period])
  rows$Series <- gre_ws_factor(o, rows$Series, r$series)
  dodge <- ggplot2::position_dodge(width = 0.8)
  p <- ggplot2::ggplot(rows, ggplot2::aes(Period, value, fill = Series)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#83949b", linewidth = 0.35) +
    ggplot2::geom_col(position = dodge, width = 0.72) +
    ggplot2::scale_fill_manual(values = gre_ws_cols(o, nlevels(rows$Series)))
  ci <- rows[is.finite(rows$low) & is.finite(rows$high), , drop = FALSE]
  if (nrow(ci)) p <- p + ggplot2::geom_errorbar(data = ci, ggplot2::aes(ymin = low, ymax = high),
    position = dodge, width = 0.22, colour = o$dark, linewidth = o$line / 2)
  gre_ws_finish(p, o, if (growth) "Compound growth by period" else "Instability by period", NULL,
    if (growth) "CGR (% per year)" else "Cuddy-Della Valle Index (%)",
    paste0(if (growth) paste0("Bars: CGR with ", 100 * (1 - r$alpha), "% confidence intervals. ") else "",
           "The last group is the whole period. Periods with fewer than three usable years are left out."),
    value_axis = "y")
}

gre_ws_build_map <- function(r, o) {
  grouped <- !is.null(r$group_data)
  point <- function(label, kind, year, y) {
    ok <- is.finite(year) & is.finite(y)
    cg <- gre_cgr(year[ok], y[ok], r$alpha)
    data.frame(Label = label, Kind = kind, CGR = cg$rate, CDVI = gre_instability(year[ok], y[ok])$cdvi)
  }
  if (grouped) {
    choices <- unique(r$group_data$Series)
    series <- o$get("series", choices[1])
    if (!series %in% choices) series <- choices[1]
    g <- r$group_data[r$group_data$Series == series, , drop = FALSE]
    rows <- do.call(rbind, lapply(unique(as.character(g$Group)), function(name) {
      part <- g[as.character(g$Group) == name, , drop = FALSE]
      part <- part[order(part$Year), , drop = FALSE]
      point(name, as.character(part$Scope[1]), part$Year, part$Value)
    }))
  } else {
    series <- ""
    rows <- do.call(rbind, lapply(r$series, function(s) {
      d <- gre_ws_series_data(r, s)
      point(s, "Group", d$Year, d$y)
    }))
  }
  rows <- rows[is.finite(rows$CGR) & is.finite(rows$CDVI), , drop = FALSE]
  gre_ws_check(nrow(rows) > 0, "This map needs at least one series with a compound growth rate and an instability value, so positive values over at least three years.")
  rows$Label <- gre_ws_label(o, rows$Label)
  # Neighbouring labels sit on opposite sides of their points.
  rows$Above <- ifelse(rank(rows$CDVI, ties.method = "first") %% 2L == 1L, -0.9, 1.9)
  # Only the instability bands the data reach, so they cannot squash the points.
  bands <- c(15, 30)[c(15, 30) <= max(rows$CDVI)]
  p <- ggplot2::ggplot(rows, ggplot2::aes(CDVI, CGR, colour = Kind, label = Label)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#83949b", linewidth = 0.35) +
    gre_ws_points(o) +
    ggplot2::geom_text(ggplot2::aes(vjust = Above), size = o$text / 3.4, family = o$font, show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = c(Group = o$accent, Combined = o$contrast), guide = "none") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.12)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15))
  if (length(bands)) p <- p + ggplot2::geom_vline(xintercept = bands, colour = "#a9b6bc", linetype = 2, linewidth = 0.35)
  gre_ws_finish(p, o, if (nzchar(series)) paste("Growth and instability:", series) else "Growth and instability",
    "Instability, Cuddy-Della Valle Index (%)", "CGR (% per year)",
    "Instability under 15% is often called low, 15-30% medium and above 30% high: a convention in some studies, not a fixed standard. Dashed lines show the bands these values reach.",
    value_axis = "y")
}

gre_ws_build_change <- function(r, o) {
  series <- gre_ws_pick_series(r, o)
  d <- gre_ws_series_data(r, series)
  test <- gre_pettitt(d$y)
  gre_ws_check(!is.null(test), "The change-point test needs at least six usable years.")
  gre_ws_check(test$K > 0, "This series does not change, so there is no change point.")
  year <- d$Year[test$at]
  before <- mean(d$y[seq_len(test$at)])
  after <- mean(d$y[(test$at + 1L):nrow(d)])
  means <- data.frame(x = c(d$Year[1], d$Year[test$at + 1L]), xend = c(year, d$Year[nrow(d)]), y = c(before, after))
  p <- ggplot2::ggplot(d, ggplot2::aes(Year, y)) +
    ggplot2::geom_line(colour = o$accent, linewidth = o$line, linetype = o$linetype) +
    gre_ws_points(o, colour = o$accent) +
    ggplot2::geom_vline(xintercept = year + 0.5, colour = o$contrast, linetype = 2, linewidth = 0.6) +
    ggplot2::geom_segment(data = means, ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                          colour = o$contrast, linewidth = o$line * 1.3)
  gre_ws_finish(p, o, paste("Change point:", series), "Year", gre_ws_label(o, series, 60L),
    paste0("Pettitt (1979): most likely change after ", year, " (K = ", test$K, ", p ",
           if (test$p < 0.001) "< 0.001" else paste0("= ", formatC(test$p, digits = 3, format = "f")),
           "). Horizontal lines: mean before and after. A steady trend can also give a small p-value, so read this with the fitted trend."),
    value_axis = "y", year_x = TRUE)
}

gre_ws_build_ita <- function(r, o) {
  series <- gre_ws_pick_series(r, o)
  y <- gre_ws_series_data(r, series)$y
  half <- floor(length(y) / 2)
  gre_ws_check(half >= 4L, "Innovative trend analysis needs at least eight usable years.")
  first <- sort(head(y, half))
  second <- sort(tail(y, half))
  d <- data.frame(First = first, Second = second)
  range_all <- range(c(first, second))
  p <- ggplot2::ggplot(d, ggplot2::aes(First, Second))
  if (all(y > 0)) p <- p +
    ggplot2::geom_abline(slope = c(0.9, 1.1), intercept = 0, colour = "#a9b6bc", linetype = 2, linewidth = 0.35)
  p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, colour = o$contrast, linewidth = o$line) +
    gre_ws_points(o, colour = o$accent) +
    ggplot2::expand_limits(x = range_all, y = range_all)
  above <- round(100 * mean(second > first))
  gre_ws_finish(p, o, paste("Innovative trend analysis:", series),
    "First half of the years (sorted)", "Second half of the years (sorted)",
    paste0("Sen (2012). Points above the 1:1 line mean higher values in the later years, below it lower: ", above,
           "% of points are above. Dashed lines: 10% either side of 1:1. Shows change across low, middle and high values."),
    value_axis = "y")
}

gre_ws_build_sen <- function(r, o) {
  series <- gre_ws_pick_series(r, o)
  d <- gre_ws_series_data(r, series)
  gre_ws_check(nrow(d) >= 4L, "The Mann-Kendall test and Sen's slope need at least four usable years.")
  test <- gre_mann_kendall(d$y)
  slope <- gre_sens_slope(d$Year, d$y, r$alpha)
  # The line and interval pass through the middle of the data.
  x0 <- stats::median(d$Year); y0 <- stats::median(d$y)
  d$fit <- y0 + slope$slope * (d$Year - x0)
  p <- ggplot2::ggplot(d, ggplot2::aes(Year))
  if (is.finite(slope$lower) && is.finite(slope$upper)) {
    limits <- cbind(y0 + slope$lower * (d$Year - x0), y0 + slope$upper * (d$Year - x0))
    d$low <- pmin(limits[, 1], limits[, 2]); d$high <- pmax(limits[, 1], limits[, 2])
    p <- ggplot2::ggplot(d, ggplot2::aes(Year)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high), fill = o$accent, alpha = 0.15)
  }
  p <- p + ggplot2::geom_line(ggplot2::aes(y = fit), colour = o$accent, linewidth = o$line, linetype = o$linetype) +
    gre_ws_points(o, ggplot2::aes(y = y), colour = o$dark)
  gre_ws_finish(p, o, paste("Mann-Kendall and Sen's slope:", series), "Year", gre_ws_label(o, series, 60L),
    paste0("Sen's slope ", signif(slope$slope, 4), " per year; band: its ", 100 * (1 - r$alpha),
           "% confidence interval. Mann-Kendall tau ", formatC(test$tau, digits = 3, format = "f"), ", p ",
           if (test$p < 0.001) "< 0.001" else paste0("= ", formatC(test$p, digits = 3, format = "f")), "."),
    value_axis = "y", year_x = TRUE)
}

## ---- drawing and export -----------------------------------------------------
## ggplotly sizes itself from any open graphics device, so one device left behind
## by a failed plot would break every later interactive chart in this R process.
gre_ws_plotly <- function(p, height = NULL) {
  grDevices::graphics.off()
  plotly::ggplotly(p, height = height)
}

gre_ws_dimensions <- function(input, key) {
  dv <- gre_ws_defaults(key)
  get <- function(field, default) input[[gre_ws_id(key, field)]] %||% default
  width <- get("width", dv$width); height <- get("height", dv$height)
  dpi <- get("dpi", dv$dpi); format <- get("format", dv$format)
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

## Writes a figure, wrapping the title and note to fit the chosen width.
gre_ws_write <- function(p, file, d) {
  wrap <- function(text) {
    if (is.null(text)) return(NULL)
    width <- max(20L, floor(d$width * 11))
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    paste(unlist(lapply(lines, strwrap, width = width)), collapse = "\n")
  }
  labels <- ggplot2::get_labs(p)
  p <- p + ggplot2::labs(title = wrap(labels$title), caption = wrap(labels$caption))
  device <- gre_open_device(file, d$format, d)
  on.exit(grDevices::dev.off(device), add = TRUE)
  print(p)
  invisible(file)
}

## Connects one workspace. `build` takes the options and returns a ggplot; the
## returned reactive gives the same figure to the report without the typing delay.
gre_ws_server <- function(key, build, originals, input, output, session, dl = shiny::downloadHandler) {
  id <- function(field) gre_ws_id(key, field)
  dv <- gre_ws_defaults(key)
  labels <- gre_labels_server(id("edit"),
    originals = reactive(tryCatch(as.character(originals()), error = function(e) character())),
    dataset_key = reactive(input$rcbd_file1$datapath %||% input$demo_data_select %||% ""))
  opts <- reactive(gre_ws_opts(input, key, labels()))
  # Dragging a slider should redraw once, when it stops, not at every step.
  settled <- debounce(opts, 300)
  figure <- reactive(build(opts()))
  shown <- reactive(build(settled()))
  output[[id("plot")]] <- renderPlot(shown(), res = 96,
    width = function() {
      w <- session$clientData[[paste0("output_", id("plot"), "_width")]]
      if (is.null(w) || !is.finite(w) || w < 320) 320 else w
    },
    height = function() input[[id("screen")]] %||% dv$screen)
  output[[id("ly")]] <- plotly::renderPlotly(gre_ws_plotly(shown(), input[[id("screen")]] %||% dv$screen))
  output[[id("dl_ui")]] <- renderUI({
    ok <- tryCatch(!is.null(shown()), error = function(e) FALSE)
    if (ok) gre_ws_export_ui(key, input)
  })
  output[[id("dl")]] <- dl(
    filename = function() paste0("GRE_", key,
      if (!is.null(input[[id("kind")]])) paste0("_", input[[id("kind")]]), "_", Sys.Date(), ".",
      input[[id("format")]] %||% dv$format),
    content = function(file) gre_ws_write(figure(), file, gre_ws_dimensions(input, key)))
  figure
}
