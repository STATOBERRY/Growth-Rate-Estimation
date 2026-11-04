/* Reusable, title-bar movement and edge/corner resizing for RAISINS plot dialogs. */
(function () {
  'use strict';
  var drag = null;
  function panel(modal) { return modal.querySelector('.modal-dialog'); }
  function place(box, left, top) {
    var rect = box.getBoundingClientRect();
    if (box.style.height) {
      box.style.height = Math.min(rect.height, Math.max(1, window.innerHeight - 24)) + 'px';
      rect = box.getBoundingClientRect();
    }
    box.style.position = 'fixed';
    box.style.margin = '0';
    box.style.transform = 'none';
    box.style.width = Math.min(rect.width, Math.max(1, window.innerWidth - 24)) + 'px';
    var width = Math.min(rect.width, Math.max(1, window.innerWidth - 24));
    box.style.left = Math.max(12, Math.min(left, window.innerWidth - width - 12)) + 'px';
    box.style.top = Math.max(12, Math.min(top, Math.max(12, window.innerHeight - rect.height - 12))) + 'px';
  }
  function finish(event) { if (!event || !drag || event.pointerId === drag.pointer) drag = null; }
  function resizeBox(state, dx, dy) {
    var edge = state.edge, right = state.left + state.width, bottom = state.top + state.height;
    var minW = Math.min(320, Math.max(1, window.innerWidth - 24));
    var minH = Math.min(240, Math.max(1, window.innerHeight - 24));
    var left = state.left, top = state.top;
    if (edge.indexOf('w') >= 0) left = Math.max(12, Math.min(state.left + dx, right - minW));
    if (edge.indexOf('n') >= 0) top = Math.max(12, Math.min(state.top + dy, bottom - minH));
    if (edge.indexOf('e') >= 0) right = Math.min(window.innerWidth - 12, Math.max(state.left + minW, right + dx));
    if (edge.indexOf('s') >= 0) bottom = Math.min(window.innerHeight - 12, Math.max(state.top + minH, bottom + dy));
    state.box.style.width = (right - left) + 'px';
    state.box.style.height = (bottom - top) + 'px';
    state.box.classList.add('raisins-dialog-sized');
    // Apply the requested rectangle directly. Reading layout here can return
    // an intermediate Bootstrap/CSS size and overwrite the user's new size.
    state.box.style.left = left + 'px';
    state.box.style.top = top + 'px';
  }
  function begin(event) {
    var handle = event.target.closest('.raisins-plot-dialog .raisins-dialog-resize');
    var resizeModal = handle ? handle.closest('.raisins-plot-dialog') : event.target.closest('.raisins-plot-dialog');
    var resizePanel = resizeModal ? panel(resizeModal) : null;
    var edge = handle ? handle.getAttribute('data-resize-dir') : '';
    // Border hit testing also works when a child covers a resize handle.
    if (!edge && resizePanel && !event.target.closest('button,a,input,select,textarea,label,.selectize-dropdown')) {
      var r = resizePanel.getBoundingClientRect();
      if (event.clientX >= r.left && event.clientX <= r.left + r.width && event.clientY >= r.top && event.clientY <= r.top + r.height) {
        if (event.clientY - r.top <= 8) edge += 'n';
        else if (r.top + r.height - event.clientY <= 8) edge += 's';
        if (event.clientX - r.left <= 8) edge += 'w';
        else if (r.left + r.width - event.clientX <= 8) edge += 'e';
      }
    }
    if (edge && event.button === 0) {
      var bounds = resizePanel.getBoundingClientRect();
      place(resizePanel, bounds.left, bounds.top);
      bounds = resizePanel.getBoundingClientRect();
      drag = { box: resizePanel, edge: edge, pointer: event.pointerId,
        x: event.clientX, y: event.clientY, left: bounds.left, top: bounds.top, width: bounds.width, height: bounds.height };
      var capture = handle || resizePanel;
      if (capture.setPointerCapture) { try { capture.setPointerCapture(event.pointerId); } catch (ignore) {} }
      event.preventDefault();
      return;
    }
    var header = event.target.closest('.raisins-plot-dialog .modal-header');
    if (!header || event.button !== 0 || event.target.closest('button,a,input,select,textarea')) return;
    var box = panel(header.closest('.raisins-plot-dialog'));
    var rect = box.getBoundingClientRect();
    place(box, rect.left, rect.top);
    rect = box.getBoundingClientRect();
    drag = { box: box, pointer: event.pointerId, x: event.clientX, y: event.clientY, left: rect.left, top: rect.top };
    if (header.setPointerCapture) { try { header.setPointerCapture(event.pointerId); } catch (ignore) {} }
    event.preventDefault();
  }
  function move(event) {
    if (!drag || drag.pointer !== event.pointerId) return;
    if (event.buttons === 0) { finish(); return; }
    if (drag.edge) resizeBox(drag, event.clientX - drag.x, event.clientY - drag.y);
    else place(drag.box, drag.left + event.clientX - drag.x, drag.top + event.clientY - drag.y);
    event.preventDefault();
  }
  document.addEventListener('pointerdown', begin, true);
  document.addEventListener('pointermove', move, { passive: false });
  // Older embedded browsers may provide mouse events without Pointer Events.
  if (!window.PointerEvent) {
    function mouse(event, handler) {
      handler({ target:event.target, button:event.button, buttons:event.buttons, pointerId:1,
        clientX:event.clientX, clientY:event.clientY, preventDefault:function(){event.preventDefault();} });
    }
    document.addEventListener('mousedown', function(event){mouse(event,begin);}, true);
    document.addEventListener('mousemove', function(event){mouse(event,move);}, {passive:false});
    document.addEventListener('mouseup', function(event){mouse(event,finish);});
  }
  document.addEventListener('pointerup', finish);
  document.addEventListener('pointercancel', finish);
  window.addEventListener('blur', function () { finish(); });
  document.addEventListener('keydown', function (event) {
    var header = event.target.closest('.raisins-plot-dialog .modal-header');
    if (!header || event.target.closest('button,a,input,select,textarea')) return;
    var moves = { ArrowLeft: [-16, 0], ArrowRight: [16, 0], ArrowUp: [0, -16], ArrowDown: [0, 16] };
    var delta = moves[event.key];
    if (!delta) return;
    var box = panel(header.closest('.raisins-plot-dialog'));
    var rect = box.getBoundingClientRect();
    place(box, rect.left + delta[0], rect.top + delta[1]);
    event.preventDefault();
  });
  window.addEventListener('resize', function () {
    document.querySelectorAll('.raisins-plot-dialog.in .modal-dialog').forEach(function (box) {
      if (box.style.position !== 'fixed') return;
      var rect = box.getBoundingClientRect();
      place(box, rect.left, rect.top);
    });
  });
  $(document).on('shown.bs.modal.raisinsPlotDrag', '.raisins-plot-dialog', function () {
    $('.modal-backdrop').last().addClass('raisins-plot-backdrop');
  }).on('hidden.bs.modal.raisinsPlotDrag', '.raisins-plot-dialog', function () {
    finish();
    var box = panel(this);
    box.classList.remove('raisins-dialog-sized');
    ['position','margin','transform','width','height','left','top'].forEach(function (name) { box.style[name] = ''; });
  });
})();
