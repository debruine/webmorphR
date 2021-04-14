var pts = [];

$(function() {
  $("#delin_img").click(function(e) {
    var imgoffset = $("#delin_img").offset();
    var x = (e.pageX - imgoffset.left);
    var y = (e.pageY - imgoffset.top);

    pts.push({x: x, y: y});
    Shiny.setInputValue("pts", pts);
    console.log(pts);

    var $pt = $('<div class="pt" />').appendTo('#delin_img');
    $pt.data({
        'x': x,
        'y': y
    });

    $pt.css({
        top: e.pageY,
        left: e.pageX
    });
  });
});

function delin_clear() {
  $(".pt").remove();
  pts = [];
  Shiny.setInputValue("pts", pts);
}

