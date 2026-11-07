# =============================================================================
# gre_raone_bubble.R - floating RA-One button (same as RAISINS RBD and CRD)
# =============================================================================
# Hidden until login, then opens the RA-One tab. gre_server() sends
# "show_raone_bubble" once a user is signed in.
# =============================================================================

gre_raone_bubble <- function() {
  tagList(
    tags$style(HTML("
#raone-bubble {
  position: fixed;
  bottom: 28px;
  right: 28px;
  width: 68px;
  height: 68px;
  border-radius: 50%;
  background: #560466;
  box-shadow: 0 6px 20px rgba(86,4,102,0.55), 0 0 18px rgba(193,139,204,0.45);
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  z-index: 2147483647;
  border: none;
  outline: none;
  -webkit-tap-highlight-color: transparent;
  isolation: isolate;
  animation: raone-float 6s ease-in-out infinite;
  display: none !important;
}
#raone-bubble.raone-visible {
  display: flex !important;
}

@keyframes raone-float {
  0%   { transform: translateY(0) rotate(0deg); }
  70%  { transform: translateY(0) rotate(0deg); }
  75%  { transform: translateY(-3px) rotate(-4deg); }
  80%  { transform: translateY(0) rotate(4deg); }
  85%  { transform: translateY(-3px) rotate(-3deg); }
  90%  { transform: translateY(0) rotate(2deg); }
  100% { transform: translateY(0) rotate(0deg); }
}
#raone-bubble:hover, #raone-bubble:focus-visible {
  animation-play-state: paused;
  box-shadow: 0 8px 28px rgba(86,4,102,0.65), 0 0 22px rgba(193,139,204,0.55);
}
#raone-bubble::after {
  position: absolute;
  right: 78px;
  bottom: 14px;
  background: #2d1a30;
  color: #f2eaf5;
  padding: 7px 13px;
  border-radius: 8px;
  font-size: 13px;
  font-family: Arial, sans-serif;
  white-space: nowrap;
  pointer-events: none;
  box-shadow: 0 2px 10px rgba(0,0,0,0.25);
  content: 'Hi! I am RA-One';
  opacity: 0;
  transform: translateY(8px);
  transition: opacity 0.25s ease, transform 0.25s ease;
}
#raone-bubble::before {
  content: '';
  position: absolute;
  right: 72px;
  bottom: 22px;
  border: 6px solid transparent;
  border-left-color: #2d1a30;
  pointer-events: none;
  opacity: 0;
  transition: opacity 0.25s ease;
}
#raone-bubble:hover::after, #raone-bubble:focus-visible::after {
  content: 'Hi! I am RA-One';
  opacity: 1;
  transform: translateY(0);
}
#raone-bubble:hover::before, #raone-bubble:focus-visible::before { opacity: 1; }
@keyframes raone-tooltip-auto {
  0%,  68% { opacity: 0; transform: translateY(8px); }
  73%, 87% { opacity: 1; transform: translateY(0); }
  100%     { opacity: 0; transform: translateY(8px); }
}
#raone-bubble::after { animation: raone-tooltip-auto 6s ease-in-out infinite; }
#raone-bubble:hover::after, #raone-bubble:focus-visible::after {
  animation: none !important; opacity: 1 !important; transform: translateY(0) !important;
}
#raone-bubble img {
  width: 40px; height: 50px; pointer-events: none;
  filter: drop-shadow(0 1px 3px rgba(0,0,0,0.35));
}
@media (prefers-reduced-motion: reduce) {
  #raone-bubble, #raone-bubble::after { animation: none !important; }
}
")),
    tags$div(
      id           = "raone-bubble",
      role         = "button",
      tabindex     = "0",
      `aria-label` = "Open RA-One AI Assistant",
      tags$img(
        src = "https://firebasestorage.googleapis.com/v0/b/raisins-2025.firebasestorage.app/o/RAISINS_GENERAL%2Fbot.webp?alt=media",
        alt = "RA-One"
      )
    ),
    tags$script(HTML("
// Show bubble only after login
Shiny.addCustomMessageHandler('show_raone_bubble', function(msg) {
  var el = document.getElementById('raone-bubble');
  if (el) { el.classList.add('raone-visible'); }
});

(function () {
  function openRAOne() {
    var link = document.querySelector('a[data-value=\"RA-One\"]');
    if (link) { link.click(); }
  }
  function wire() {
    var el = document.getElementById('raone-bubble');
    if (!el) return;
    el.addEventListener('click', openRAOne);
    el.addEventListener('keydown', function (e) {
      if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); openRAOne(); }
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', wire);
  else wire();
})();
"))
  )
}
