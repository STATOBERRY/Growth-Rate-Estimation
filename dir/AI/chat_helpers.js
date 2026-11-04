// www/chat_helpers.js
// Include via: tags$head(tags$script(src = "chat_helpers.js"))
// or it's registered automatically when you source chat_ui.R

Shiny.addCustomMessageHandler("clearChatInput", function(message) {

  var el = document.getElementById(message.id);

  if (el) {
    el.value = "";        // clear text
    el.focus();           // return cursor to textbox

    // Trigger input event so Shiny updates internal state
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }

});