var resize = 100;
var pts = [];

/* onload functions */
$(function() {
  $("#delin_img").click(function(e) {
    if ((e.ctrlKey || e.metaKey) && e.shiftKey) { new_pt(e); }
  });
  
  $('#quickhelp').insertAfter(".sidebar-toggle");
  
  $( window ).resize(function() {
    var h = $( window ).height();
    h = h - $('#delin_box_box').offset().top;
    h = h - 14;
    console.log(h);
    $('#delin_box_box').css({height: h});
  });
  
  $("#delin_img img").attr('draggable', false);
});

/* keyboard shortcuts */
$(document).keydown(function(e) {
  if ((e.ctrlKey || e.metaKey) && e.shiftKey) {
    $('#delin_img').css('cursor', 'crosshair');
    quickhelp('Click to add a point');
  } else if ((e.ctrlKey || e.metaKey) && e.which == KEYCODE.s) {
    $("#delin_save").click();
    e.preventDefault();
  } else if ((e.ctrlKey || e.metaKey) && e.which == KEYCODE.right_arrow) {
    $("#next_img").click();
    e.preventDefault();
  } else if ((e.ctrlKey || e.metaKey) && e.which == KEYCODE.left_arrow) {
    $("#prev_img").click();
    e.preventDefault();
    
  }
}).keyup(function(e) {
  if (!((e.ctrlKey || e.metaKey) && e.shiftKey)) {
    $('#delin_img').css('cursor', 'auto');
    quickhelp();
  }
});

function quickhelp(text, fadeout) {
    if (text === undefined || text.trim() === '') {
        $('#quickhelp').fadeOut();
    } else {
        $('#quickhelp').html(text).fadeIn();
    }

    if (fadeout !== undefined && fadeout > 100 && fadeout < 10000) {
        setTimeout(function() { $('#quickhelp').fadeOut(); }, fadeout);
    }
}

function update_pts() {
  $('.pt').each(function() {
    var data = $(this).data();
    pts[data.i].x = data.x;
    pts[data.i].y = data.y;
  });
  Shiny.setInputValue("pts", pts);
  //console.log(pts);
}


function new_pt(e) {
  var x = (e.originalEvent.layerX) / resize * 100;
  var y = (e.originalEvent.layerY) / resize * 100;

  pts.push({x: x, y: y});

  var $pt = $('<div class="pt" />').appendTo('#delin_box');
  $pt.data({
      'i': pts.length - 1,
      'x': x,
      'y': y
  });

  $pt.css({
      top: e.originalEvent.layerY,
      left: e.originalEvent.layerX
  });
  
  $pt.draggable({stop: function(e, ui) {
    var x = (ui.position.left) / resize * 100;
    var y = (ui.position.top) / resize * 100;
    $(this).data({'x': x, 'y': y});
    update_pts();
  }});
  
  update_pts();
}

function delin_resize() {
  $('.pt').each(function() {
    var data = $(this).data();
    var x = data.x * resize/100;
    var y = data.y * resize/100;
    //console.log(x, y);
    $(this).css( {'left': x, 'top': y } );
    
  });
}

function delin_clear() {
  $(".pt").remove();
  pts = [];
  Shiny.setInputValue("pts", pts);
}

