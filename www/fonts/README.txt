Bundled report fonts
====================

These TrueType fonts are used by the PDF report (xelatex + fontspec) as free,
metric-compatible substitutes for the proprietary fonts offered in the app's
"Select Font" dropdown. They are redistributable under open licenses, so they
ship with the app and need no server-side font installation.

  Dropdown choice                 Bundled twin   Source / License
  ------------------------------  -------------  -----------------------------
  cambria                         Caladea        Huerta Tipografica (OFL 1.1)
  Times New Roman                 Tinos          Google / Croscore (Apache 2.0)
  Georgia                         Gelasio        Google (OFL 1.1)
  Arial, Helvetica                Arimo          Google / Croscore (Apache 2.0)
  Courier New                     Cousine        Google / Croscore (Apache 2.0)
  Verdana, Tahoma, Trebuchet MS   Carlito        Google (OFL 1.1) - closest sans

Each family provides four styles: <Family>-Regular.ttf, -Bold.ttf, -Italic.ttf
and -BoldItalic.ttf. The mapping lives in dir/crdp_table_style.R
(crdp_font_twin / crdp_font_pdf_args).
