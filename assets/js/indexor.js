$(document).ready(function() {

  $("h2").append("<h4>Table of Contents</h4><ul id='cc-tableofcontent__list'></ul>");

  //Instanciate an index for each h3 
  $(".cc-content h3").each( function() {
    var section_id = $(this).attr("id");
    var line = "<li><a href=#"+section_id+'>'+$(this).text()+'</a></li>';
    var $newli = $(line);
    $("#cc-tableofcontent__list").append($newli);
  });
  
  //Smooth scrolling
  $('.cc-content__text ul li a').click(function(){
    $('html, body').animate({
        scrollTop: $( $(this).attr('href') ).offset().top - 20
    }, 500);
    return false;
  });

  // Show or hide the sticky footer button
  $(window).scroll(function() {
  if ($(this).scrollTop() > 200) {
    $('.cc-go-top').fadeIn(200);
  } else {
    $('.cc-go-top').fadeOut(200);
  }
});

// Animate the scroll to top
$('.cc-go-top').click(function(event) {
  event.preventDefault();
  
  $('html, body').animate({scrollTop: 0}, 300);
})
});