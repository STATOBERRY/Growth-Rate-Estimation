# =============================================================================
# chat_csv_generator.R  ·  RAISINS CRD
# =============================================================================
# CSV template generation for the RA-One chat assistant.
# Builds an inline editable HTML table card (exactly like crd_datagen_ui.R)
# with:
#   - Subject and Day columns pre-filled and locked
#   - Empty editable cells (NO NA values)
#   - Paste from Excel (Ctrl+V)
#   - "Add row" button for unequal replication
#   - JS-powered "Download CSV" that reads live DOM values
#   - "Rebuild" button to regenerate with new t/r/resp
#   - Live error df display
#   - Replication auto-adjusted so error df >= 12
#
# PUBLIC FUNCTIONS (called by chat_server.R):
#   detect_datagen_request(msg)
#   parse_datagen_params(msg)
#   compute_min_replication(n_day)
#   build_rm_datagen_card(gen_id, n_day, n_rep, n_resp, ns_fn, output)
#   build_datagen_reply(n_day, n_rep, n_resp, error_df, low_df)
# =============================================================================

library(shiny)


#' Detect if user asks for a data template / CSV / layout / data sheet.
detect_datagen_request <- function(msg) {
  m <- tolower(msg)
  grepl(paste0(
    "generate.*data|create.*data|make.*data|prepare.*data|build.*data|",
    "setup.*data|set up.*data|",
    "data.*entry|data.*sheet|data.*template|data.*layout|",
    "template.*data|entry.*sheet|entry.*table|",
    "csv.*template|template.*csv|blank.*csv|empty.*csv|",
    "csv.*file.*for|csv.*for.*crd|model.*csv|csv.*model|",
    "csv.*file.*\\d+.*treat|csv.*\\d+.*treat|",
    "fill.*data|enter.*data|input.*data|",
    "make.*spreadsheet|create.*spreadsheet|generate.*spreadsheet|",
    "need.*template|want.*template|give.*template|",
    "need.*data.*sheet|want.*data.*sheet|",
    "how.*arrange.*data|arrange.*data|data.*arrangement|",
    "how.*prepare.*data|data.*preparation|data.*format|",
    "how.*data.*look|what.*data.*look|show.*layout|",
    "\\d+\\s*treat.*\\d+\\s*rep|\\d+\\s*rep.*\\d+\\s*treat|",
    "treatment.*replication.*variable|treatment.*replication.*character|",
    "blank.*sheet|empty.*sheet|blank.*table|empty.*table|",
    "download.*csv|downloadable.*data|csv.*file.*for|",
    "data.*file.*crd|crd.*data.*file|csv.*for.*crd"
  ), m, perl = TRUE)
}


#' Extract treatment, day, replication and response counts.
#' Returns list(n_trt, n_day, n_rep, n_resp) with NULLs if absent.
parse_datagen_params <- function(msg) {
  m <- tolower(msg)
  n_trt <- NULL; n_day <- NULL; n_rep <- NULL; n_resp <- NULL

  trt_match <- regmatches(m, regexpr(
    "(\\d+)\\s*(treatment|trt|group)s?", m, perl = TRUE))
  if (length(trt_match) > 0)
    n_trt <- suppressWarnings(as.integer(
      regmatches(trt_match, regexpr("\\d+", trt_match))))

  ## Repeated measures needs the within-subject factor as well. Accept the
  ## words people actually use for it.
  day_match <- regmatches(m, regexpr(
    paste0("(\\d+)\\s*(day|time\\s*point|timepoint|time|period|stage|",
           "occasion|visit|interval|week|month|year)s?"), m, perl = TRUE))
  if (length(day_match) > 0)
    n_day <- suppressWarnings(as.integer(
      regmatches(day_match, regexpr("\\d+", day_match))))

  rep_match <- regmatches(m, regexpr(
    "(\\d+)\\s*(replication|rep|replicate|block|subject|animal|plot)s?", m, perl = TRUE))
  if (length(rep_match) > 0)
    n_rep <- suppressWarnings(as.integer(
      regmatches(rep_match, regexpr("\\d+", rep_match))))

  # n_resp: keyword match like "3 variables", "2 characters", "5 traits"
  resp_match <- regmatches(m, regexpr(
    "(\\d+)\\s*(variable|character|trait|response|variate|column|observation|obs)s?",
    m, perl = TRUE))
  if (length(resp_match) > 0)
    n_resp <- suppressWarnings(as.integer(
      regmatches(resp_match, regexpr("\\d+", resp_match))))

  # Fallback: positional numbers - treatments, days, reps, responses
  if (is.null(n_trt) || is.null(n_rep)) {
    all_nums <- as.integer(regmatches(m, gregexpr("\\d+", m))[[1]])
    all_nums <- all_nums[!is.na(all_nums) & all_nums >= 1]
    if (is.null(n_trt)  && length(all_nums) >= 1) n_trt  <- all_nums[1]
    if (is.null(n_day)  && length(all_nums) >= 2) n_day  <- all_nums[2]
    if (is.null(n_rep)  && length(all_nums) >= 3) n_rep  <- all_nums[3]
    if (is.null(n_resp) && length(all_nums) >= 4) n_resp <- all_nums[4]
  }

  list(n_trt = n_trt, n_day = n_day, n_rep = n_rep, n_resp = n_resp)
}


#' Minimum number of SUBJECTS so the error df of the within-subject test >= 12.
#'
#' Model: response ~ Day + Error(Subject/Day).
#'   subject stratum df            = s - 1
#'   Subject x Day residual df     = (s - 1) * (d - 1)   <- tests Day
#' The residual is what the Day F ratio is built on, so that is the term the
#' >= 12 rule of thumb is applied to. Advice only - never coerced.
compute_min_replication <- function(n_day) {
  if (is.null(n_day) || is.na(n_day) || n_day < 2) n_day <- 2L
  ceiling(12 / (n_day - 1L)) + 1L
}

#' The error df of the one-way repeated measures model.
rm_error_df <- function(n_day, n_sub) {
  list(subject  = n_sub - 1L,
       residual = (n_sub - 1L) * (n_day - 1L))
}


# =============================================================================
# BUILD THE INLINE EDITABLE DATA ENTRY CARD
# =============================================================================
# Repeated measures layout: every subject (Treatment x Block) is measured on
# every Day, so the locked columns are Treatment, Day and Block and the table
# has t * d * r rows.
#
# @param gen_id   Unique ID prefix for this card instance
# @param n_trt    Number of treatments
# @param n_day    Number of days / time points (the within-subject factor)
# @param n_rep    Number of blocks (subjects) per treatment
# @param n_resp   Number of response columns
# @param ns_fn    The module ns() function (for namespacing Shiny inputs)
# @param output   Shiny output object (for fallback download handler)
# @return tagList

build_rm_datagen_card <- function(gen_id, n_day, n_rep, n_resp, ns_fn, output) {

  # ── Validate & enforce between-subject error df >= 12 ────────────────────
  n_day  <- max(2L, min(30L, as.integer(n_day)))
  n_resp <- max(1L, min(20L, as.integer(n_resp)))
  n_rep  <- max(2L, min(200L, as.integer(n_rep)))   # n_rep = number of subjects
  ## Build exactly what was asked for: the >=12 rule is advice, not a
  ## constraint. Subjects are people/plots, not a knob for the app to turn.
  min_rep <- compute_min_replication(n_day)
  adjusted <- (n_rep < min_rep)   # advisory flag only
  edf <- rm_error_df(n_day, n_rep)
  error_df <- edf$residual

  # ── Build table HTML ─────────────────────────────────────────────────────
  # every Subject x Day combination exactly once
  sub_col   <- rep(paste0("S", seq_len(n_rep)), times = n_day)
  day_col   <- rep(paste0("day", seq_len(n_day)), each = n_rep)
  resp_cols <- paste0("y", seq_len(n_resp))
  col_names <- c("Subject", "Day", resp_cols)
  n_rows    <- n_day * n_rep

  # shared cell styles - used by BOTH the static HTML and the Add-row JS, so
  # rows added later look identical to the generated ones
  lock_style <- paste0("padding:6px 10px;background:#f3eaf6;font-weight:600;",
                       "color:#560466;font-size:12px;white-space:nowrap;")
  resp_style <- paste0("padding:6px 10px;font-size:12px;min-width:80px;",
                       "border:1px solid #e8d9ed;outline:none;")

  # Header
  th_cells <- paste0(
    "<th style='background:linear-gradient(90deg,#2d1a30,#560466);color:#fff;",
    "padding:7px 12px;font-size:12px;font-weight:600;white-space:nowrap;'>",
    col_names, "</th>", collapse = "")

  # Body rows - Treatment/Day/Block locked, response cells empty & editable
  body_rows <- paste(sapply(seq_len(n_rows), function(i) {
    lock_cells <- paste0(
      "<td style='", lock_style, "'>", sub_col[i], "</td>",
      "<td style='", lock_style, "'>", day_col[i], "</td>")
    resp_cells <- paste(rep(paste0(
      "<td contenteditable='true' style='", resp_style, "' ",
      "onkeydown=\"if(event.key==='Tab'){event.preventDefault();",
      "var cells=this.closest('table').querySelectorAll('td[contenteditable]');",
      "var idx=Array.from(cells).indexOf(this);",
      "if(idx+1<cells.length)cells[idx+1].focus();}\" ",
      "onpaste=\"var self=this;setTimeout(function(){",
      "var lines=self.innerText.split('\\n');",
      "var row=self.closest('tr');",
      "var rows=[row];var next=row;",
      "for(var l=1;l<lines.length;l++){next=next.nextElementSibling;if(next)rows.push(next);}",
      "var cols=self.cellIndex;",
      "rows.forEach(function(r,ri){",
      "var parts=lines[ri].split('\\t');",
      "parts.forEach(function(v,ci){",
      "var c=r.cells[cols+ci];if(c&&c.contentEditable==='true')c.innerText=v.trim();",
      "});});},0);\"></td>"
    ), n_resp), collapse = "")
    paste0("<tr>", lock_cells, resp_cells, "</tr>")
  }), collapse = "\n")

  table_id   <- paste0(gen_id, "_tbl")
  add_btn_id <- paste0(gen_id, "_addbtn")
  dl_fn_name <- paste0("dlcsv_", gsub("[^a-zA-Z0-9]", "_", gen_id))

  # ── Shiny download handler (fallback - JS download is primary) ───────────
  output[[paste0(gen_id, "_dl")]] <- downloadHandler(
    filename = function() paste0("rm_oneway_data_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- data.frame(
        Subject = sub_col,
        Day     = day_col,
        stringsAsFactors = FALSE)
      for (col in resp_cols) df[[col]] <- ""
      write.csv(df, file, row.names = FALSE)
    }
  )


  # ── Build the card UI ────────────────────────────────────────────────────
  tagList(
    div(style = "margin-top:10px;background:#fff;border:1.5px solid #e8d9ed;border-radius:12px;overflow:hidden;",
        
        # Card header
        div(style = paste0("background:linear-gradient(135deg,#560466,#2d1a30);",
                           "color:#fff;padding:10px 16px;font-size:13px;font-weight:600;"),
            tags$i(class = "fas fa-table", style = "margin-right:6px;"),
            paste0("One-way Repeated Measures Data Entry \u2014 ", n_rep, " subjects \u00d7 ",
                   n_day, " days, ", n_resp, " response(s)")),
        
        # Card body
        div(style = "padding:14px;",
            
            # ── Regenerate controls ──────────────────────────────────────────
            div(style = "display:flex;gap:8px;flex-wrap:wrap;align-items:flex-end;margin-bottom:12px;",
                div(
                  tags$label(style = "font-size:10.5px;font-weight:700;color:#9A9790;display:block;margin-bottom:3px;text-transform:uppercase;letter-spacing:.08em;",
                             "Subjects"),
                  tags$input(id = paste0(gen_id, "_nrep"), type = "number",
                             value = n_rep, min = 2, max = 200,
                             style = "width:70px;font-size:13px;border:1.5px solid #d4c4ee;border-radius:7px;padding:4px 7px;")),
                div(
                  tags$label(style = "font-size:10.5px;font-weight:700;color:#9A9790;display:block;margin-bottom:3px;text-transform:uppercase;letter-spacing:.08em;",
                             "Days / time points"),
                  tags$input(id = paste0(gen_id, "_nday"), type = "number",
                             value = n_day, min = 2, max = 30,
                             style = "width:70px;font-size:13px;border:1.5px solid #d4c4ee;border-radius:7px;padding:4px 7px;")),
                div(
                  tags$label(style = "font-size:10.5px;font-weight:700;color:#9A9790;display:block;margin-bottom:3px;text-transform:uppercase;letter-spacing:.08em;",
                             "Response columns"),
                  tags$input(id = paste0(gen_id, "_nresp"), type = "number",
                             value = n_resp, min = 1, max = 20,
                             style = "width:70px;font-size:13px;border:1.5px solid #d4c4ee;border-radius:7px;padding:4px 7px;")),
                
                # Rebuild button
                tags$button(
                  style = paste0("background:linear-gradient(135deg,#560466,#2d1a30);",
                                 "color:#fff;border:none;border-radius:20px;padding:7px 16px;",
                                 "font-size:12px;font-weight:600;cursor:pointer;align-self:flex-end;"),
                  onclick = paste0(
                    "(function(){",
                    "var d=parseInt(document.getElementById('", gen_id, "_nday').value)||3;",
                    "var r=parseInt(document.getElementById('", gen_id, "_nrep').value)||6;",
                    "var y=parseInt(document.getElementById('", gen_id, "_nresp').value)||1;",
                    "var ta=document.getElementById('", ns_fn("user_input"), "');",
                    "if(ta){ta.value='Generate one-way repeated measures data for '+r+' subjects '+d+' days '+y+' responses';",
                    "ta.dispatchEvent(new Event('input',{bubbles:true}));",
                    "document.getElementById('", ns_fn("send-btn"), "').click();}",
                    "})();"),
                  tags$i(class = "fas fa-rotate-right"), " Rebuild"),
                
                # Error df display
                tags$span(
                  id = paste0(gen_id, "_dfdisp"),
                  style = "font-size:11px;color:#5e4363;align-self:flex-end;padding-bottom:6px;",
                  paste0("Error df (Subject \u00d7 Day) = ", edf$residual,
                         if (edf$residual >= 12) " \u2705" else " \u26a0\ufe0f <12",
                         "  |  subject df = ", edf$subject))
            ),
            
            # ── Editable table ───────────────────────────────────────────────
            div(style = "overflow-x:auto;border:1.5px solid #e8d9ed;border-radius:8px;",
                HTML(paste0(
                  "<table id='", table_id, "' ",
                  "style='border-collapse:collapse;width:100%;'>",
                  "<thead><tr>", th_cells, "</tr></thead>",
                  "<tbody>", body_rows, "</tbody></table>"))),
            
            # ── Buttons ──────────────────────────────────────────────────────
            div(style = "display:flex;gap:8px;margin-top:10px;flex-wrap:wrap;",
                
                # Add row
                tags$button(
                  id = add_btn_id,
                  style = paste0("background:#fff;border:1.5px solid #e8d9ed;",
                                 "color:#5e4363;border-radius:20px;padding:6px 14px;",
                                 "font-size:12px;font-weight:600;cursor:pointer;"),
                  onclick = paste0(
                    "(function(){",
                    "var tbl=document.getElementById('", table_id, "');",
                    "var lastRow=tbl.rows[tbl.rows.length-1];",
                    "var newRow=tbl.insertRow();",
                    "for(var k=0;k<2;k++){",
                    "  var tc=newRow.insertCell();",
                    "  tc.contentEditable='true';",
                    "  tc.style='", lock_style, "';",
                    "  tc.innerText=lastRow.cells[k].innerText;",
                    "}",
                    "for(var i=2;i<lastRow.cells.length;i++){",
                    "  var rc=newRow.insertCell();",
                    "  rc.contentEditable='true';",
                    "  rc.style='", resp_style, "';",
                    "}",
                    "newRow.cells[0].focus();",
                    "})();"),
                  tags$i(class = "fas fa-plus"), " Add row"),
                
                tags$button(
                  id = paste0(gen_id, "_addcol"),
                  style = paste0("background:#fff;border:1.5px solid #e8d9ed;",
                                 "color:#5e4363;border-radius:20px;padding:6px 14px;",
                                 "font-size:12px;font-weight:600;cursor:pointer;"),
                  onclick = paste0(
                    "(function(){",
                    "  var tbl=document.getElementById('", table_id, "');",
                    "  if(!tbl)return;",
                    "  var hdr=tbl.rows[0];",
                    "  var colCount=hdr.cells.length;",
                    "  var th=document.createElement('th');",
                    "  th.contentEditable='true';",
                    "  th.style='padding:8px 10px;background:linear-gradient(135deg,#560466,#2d1a30);color:#fff;font-size:12px;font-weight:700;white-space:nowrap;min-width:90px;border-right:1px solid rgba(255,255,255,.15);';",
                    "  th.innerText='y'+colCount;",
                    "  hdr.appendChild(th);",
                    "  for(var i=1;i<tbl.rows.length;i++){",
                    "    var td=tbl.rows[i].insertCell();",
                    "    td.contentEditable='true';",
                    "    td.style='", resp_style, "';",
                    "  }",
                    "})();"),
                  tags$i(class = "fas fa-plus"), " Add column"),
                
                # JS-powered CSV download (reads live DOM - no NA)
                tags$button(
                  style = paste0("background:linear-gradient(135deg,#1e8449,#27ae60);",
                                 "color:#fff;border:none;border-radius:20px;padding:7px 18px;",
                                 "font-size:12px;font-weight:600;cursor:pointer;",
                                 "box-shadow:0 3px 10px rgba(30,132,73,.25);"),
                  onclick = paste0(dl_fn_name, "();"),
                  tags$i(class = "fas fa-file-csv"), " Download CSV")
            ),
            
            # Hint
            tags$p(style = "font-size:11px;color:#9A9790;margin-top:8px;",
                   HTML(paste0(
                     "\U0001f4a1 Paste data from Excel (Ctrl+V). ",
                     "Click <strong>Add row</strong> for an extra subject/measurement. ",
                     "Download CSV \u2192 upload in <strong>Analysis</strong> tab.")))
        )
    ),
    
    # JS: CSV download function + live error df update
    tags$script(HTML(paste0(
      "function ", dl_fn_name, "(){",
      "  var tbl=document.getElementById('", table_id, "');",
      "  if(!tbl){alert('Table not found.');return;}",
      "  var rows=Array.from(tbl.rows);",
      "  var csv=rows.map(function(r){",
      "    return Array.from(r.cells).map(function(c){",
      "      var v=c.innerText.trim();",
      "      if(v.indexOf(',')>-1) v='\"'+v+'\"';",
      "      return v;",
      "    }).join(',');",
      "  }).join('\\n');",
      "  var blob=new Blob([csv],{type:'text/csv;charset=utf-8;'});",
      "  var url=URL.createObjectURL(blob);",
      "  var a=document.createElement('a');",
      "  a.href=url;a.download='crd_data.csv';a.style.display='none';",
      "  document.body.appendChild(a);a.click();",
      "  setTimeout(function(){document.body.removeChild(a);URL.revokeObjectURL(url);},100);",
      "}",
      "(function(){",
      "  function upd(){",
      "    var t=parseInt(document.getElementById('", gen_id, "_ntrt').value)||1;",
      "    var r=parseInt(document.getElementById('", gen_id, "_nrep').value)||1;",
      "    var df=t*r-t;",
      "    var el=document.getElementById('", gen_id, "_dfdisp');",
      "    if(el)el.innerText='Error df = '+df+(df>=12?' \u2705':' \u26a0\ufe0f <12');",
      "  }",
      "  var nt=document.getElementById('", gen_id, "_ntrt');",
      "  var nr=document.getElementById('", gen_id, "_nrep');",
      "  if(nt)nt.addEventListener('input',upd);",
      "  if(nr)nr.addEventListener('input',upd);",
      "})();"
    )))
  )
  
  # tagList(
  #   div(style = "margin-top:10px;background:#fff;border:1.5px solid #e8d9ed;border-radius:12px;overflow:hidden;",
  # 
  #       # Card header
  #       div(style = paste0("background:linear-gradient(135deg,#560466,#2d1a30);",
  #                          "color:#fff;padding:10px 16px;font-size:13px;font-weight:600;"),
  #           tags$i(class = "fas fa-table", style = "margin-right:6px;"),
  #           paste0("CRD Data Entry \u2014 ", n_trt, " treatments \u00d7 ", n_rep,
  #                  " reps, ", n_resp, " response(s)")),
  # 
  #       # Card body
  #       div(style = "padding:14px;",
  # 
  #           # ── Regenerate controls ──────────────────────────────────────────
  #           div(style = "display:flex;gap:8px;flex-wrap:wrap;align-items:flex-end;margin-bottom:12px;",
  #               div(
  #                 tags$label(style = "font-size:10.5px;font-weight:700;color:#9A9790;display:block;margin-bottom:3px;text-transform:uppercase;letter-spacing:.08em;",
  #                            "Treatments"),
  #                 tags$input(id = paste0(gen_id, "_ntrt"), type = "number",
  #                            value = n_trt, min = 2, max = 50,
  #                            style = "width:70px;font-size:13px;border:1.5px solid #d4c4ee;border-radius:7px;padding:4px 7px;")),
  #               div(
  #                 tags$label(style = "font-size:10.5px;font-weight:700;color:#9A9790;display:block;margin-bottom:3px;text-transform:uppercase;letter-spacing:.08em;",
  #                            "Reps / treatment"),
  #                 tags$input(id = paste0(gen_id, "_nrep"), type = "number",
  #                            value = n_rep, min = 1, max = 100,
  #                            style = "width:70px;font-size:13px;border:1.5px solid #d4c4ee;border-radius:7px;padding:4px 7px;")),
  #               div(
  #                 tags$label(style = "font-size:10.5px;font-weight:700;color:#9A9790;display:block;margin-bottom:3px;text-transform:uppercase;letter-spacing:.08em;",
  #                            "Response columns"),
  #                 tags$input(id = paste0(gen_id, "_nresp"), type = "number",
  #                            value = n_resp, min = 1, max = 20,
  #                            style = "width:70px;font-size:13px;border:1.5px solid #d4c4ee;border-radius:7px;padding:4px 7px;")),
  # 
  #               # Rebuild button
  #               tags$button(
  #                 style = paste0("background:linear-gradient(135deg,#560466,#2d1a30);",
  #                                "color:#fff;border:none;border-radius:20px;padding:7px 16px;",
  #                                "font-size:12px;font-weight:600;cursor:pointer;align-self:flex-end;"),
  #                 onclick = paste0(
  #                   "(function(){",
  #                   "var t=parseInt(document.getElementById('", gen_id, "_ntrt').value)||4;",
  #                   "var r=parseInt(document.getElementById('", gen_id, "_nrep').value)||3;",
  #                   "var y=parseInt(document.getElementById('", gen_id, "_nresp').value)||1;",
  #                   "var minR=Math.ceil((12+t)/t);",
  #                   "if(r<minR){r=minR;document.getElementById('", gen_id, "_nrep').value=r;",
  #                   "  alert('Replication auto-adjusted to '+r+' so error df = '+(t*r-t)+' \\u2265 12.');}",
  #                   "var ta=document.getElementById('", ns_fn("user_input"), "');",
  #                   "if(ta){ta.value='Generate CRD data for '+t+' treatments '+r+' reps '+y+' responses';",
  #                   "ta.dispatchEvent(new Event('input',{bubbles:true}));",
  #                   "document.getElementById('", ns_fn("send-btn"), "').click();}",
  #                   "})();"),
  #                 tags$i(class = "fas fa-rotate-right"), " Rebuild"),
  # 
  #               # Error df display
  #               tags$span(
  #                 id = paste0(gen_id, "_dfdisp"),
  #                 style = "font-size:11px;color:#5e4363;align-self:flex-end;padding-bottom:6px;",
  #                 paste0("Error df = ", error_df,
  #                        if (error_df >= 12) " \u2705" else " \u26a0\ufe0f <12"))
  #           ),
  # 
  #           # ── Editable table ───────────────────────────────────────────────
  #           div(style = "overflow-x:auto;border:1.5px solid #e8d9ed;border-radius:8px;",
  #               HTML(paste0(
  #                 "<table id='", table_id, "' ",
  #                 "style='border-collapse:collapse;width:100%;'>",
  #                 "<thead><tr>", th_cells, "</tr></thead>",
  #                 "<tbody>", body_rows, "</tbody></table>"))),
  # 
  #           # ── Buttons ──────────────────────────────────────────────────────
  #           div(style = "display:flex;gap:8px;margin-top:10px;flex-wrap:wrap;",
  # 
  #               # Add row
  #               tags$button(
  #                 id = add_btn_id,
  #                 style = paste0("background:#fff;border:1.5px solid #e8d9ed;",
  #                                "color:#5e4363;border-radius:20px;padding:6px 14px;",
  #                                "font-size:12px;font-weight:600;cursor:pointer;"),
  #                 onclick = paste0(
  #                   "(function(){",
  #                   "var tbl=document.getElementById('", table_id, "');",
  #                   "var lastRow=tbl.rows[tbl.rows.length-1];",
  #                   "var newRow=tbl.insertRow();",
  #                   "var tc=newRow.insertCell();",
  #                   "tc.contentEditable='true';",
  #                   "tc.style='padding:6px 10px;background:#f3eaf6;font-weight:600;color:#560466;font-size:12px;';",
  #                   "tc.innerText=lastRow.cells[0].innerText;",
  #                   "for(var i=1;i<lastRow.cells.length;i++){",
  #                   "  var rc=newRow.insertCell();",
  #                   "  rc.contentEditable='true';",
  #                   "  rc.style='padding:6px 10px;font-size:12px;min-width:80px;';",
  #                   "}",
  #                   "newRow.cells[0].focus();",
  #                   "})();"),
  #                 tags$i(class = "fas fa-plus"), " Add row"),
  # 
  #               # JS-powered CSV download (reads live DOM - no NA)
  #               tags$button(
  #                 style = paste0("background:linear-gradient(135deg,#1e8449,#27ae60);",
  #                                "color:#fff;border:none;border-radius:20px;padding:7px 18px;",
  #                                "font-size:12px;font-weight:600;cursor:pointer;",
  #                                "box-shadow:0 3px 10px rgba(30,132,73,.25);"),
  #                 onclick = paste0(dl_fn_name, "();"),
  #                 tags$i(class = "fas fa-file-csv"), " Download CSV")
  #           ),
  # 
  #           # Hint
  #           tags$p(style = "font-size:11px;color:#9A9790;margin-top:8px;",
  #                  HTML(paste0(
  #                    "\U0001f4a1 Paste data from Excel (Ctrl+V). ",
  #                    "Click <strong>Add row</strong> for unequal replication. ",
  #                    "Download CSV \u2192 upload in <strong>Analysis</strong> tab.")))
  #       )
  #   ),
  # 
  #   # JS: CSV download function + live error df update
  #   tags$script(HTML(paste0(
  #     "function ", dl_fn_name, "(){",
  #     "  var tbl=document.getElementById('", table_id, "');",
  #     "  if(!tbl){alert('Table not found.');return;}",
  #     "  var rows=Array.from(tbl.rows);",
  #     "  var csv=rows.map(function(r){",
  #     "    return Array.from(r.cells).map(function(c){",
  #     "      var v=c.innerText.trim();",
  #     "      if(v.indexOf(',')>-1) v='\"'+v+'\"';",
  #     "      return v;",
  #     "    }).join(',');",
  #     "  }).join('\\n');",
  #     "  var blob=new Blob([csv],{type:'text/csv;charset=utf-8;'});",
  #     "  var url=URL.createObjectURL(blob);",
  #     "  var a=document.createElement('a');",
  #     "  a.href=url;a.download='crd_data.csv';a.style.display='none';",
  #     "  document.body.appendChild(a);a.click();",
  #     "  setTimeout(function(){document.body.removeChild(a);URL.revokeObjectURL(url);},100);",
  #     "}",
  #     "(function(){",
  #     "  function upd(){",
  #     "    var t=parseInt(document.getElementById('", gen_id, "_ntrt').value)||1;",
  #     "    var r=parseInt(document.getElementById('", gen_id, "_nrep').value)||1;",
  #     "    var df=t*r-t;",
  #     "    var el=document.getElementById('", gen_id, "_dfdisp');",
  #     "    if(el)el.innerText='Error df = '+df+(df>=12?' \u2705':' \u26a0\ufe0f <12');",
  #     "  }",
  #     "  var nt=document.getElementById('", gen_id, "_ntrt');",
  #     "  var nr=document.getElementById('", gen_id, "_nrep');",
  #     "  if(nt)nt.addEventListener('input',upd);",
  #     "  if(nr)nr.addEventListener('input',upd);",
  #     "})();"
  #   )))
  # )
}


#' Markdown reply that accompanies the datagen card.
build_datagen_reply <- function(n_day, n_rep, n_resp, error_df, low_df = FALSE) {
  msg <- paste0(
    "\U0001f4cb **One-way Repeated Measures Data Entry** \u2014 ", n_rep, " subjects \u00d7 ",
    n_day, " days \u00d7 ", n_resp, " response variable(s)",
    "\n\nColumns: **Subject**, **Day** + your response(s). ",
    "Every subject appears once per day - that repeated measurement on the same ",
    "subject is what the Error(Subject/Day) term in the model needs.")

  if (isTRUE(low_df))
    msg <- paste0(msg, "\n\n\u26a0\ufe0f Heads-up: with ", n_rep,
                  " subjects across ", n_day, " days the error df is **", error_df,
                  "** (below the usual rule of thumb of 12), so the test will have low ",
                  "power. Add subjects if your protocol allows; the template is built ",
                  "exactly as you asked either way.")

  paste0(msg, "\n\n",
    "Enter your values directly in the table below, or **paste from Excel** (Ctrl+V). ",
    "Click **Download CSV** when done \u2014 then upload it in the **Analysis** tab.")
}


#' Local fallback
if (!exists("%||%")) `%||%` <- function(a, b) if (!is.null(a)) a else b
