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


//Pricer
$.ajax({
  url: 'https://console.clever-cloud.com/ccapi/v1/instances',
  datatype: 'jsonp',
  success: _.bind(function(ii) {
     $.ajax({
        url: 'https://console.clever-cloud.com/ccapi/v1/prices',
      datatype: 'jsonp',
        success: _.bind(function(pp) {
          var change = _.find(pp, function(p) { return p.currency == 'EUR'; }).value;
          _.foldl(
              ii, function($ii, i){
                _.each(i.flavors, function (f){
                  $ii.append('<tr><td class="cc-col__price"><span class="label cc-label__price label-info">'
                    +f.name+
                    '</span></td><td>Worker '
                    +i.name+
                    '</td><td>'+f.price+' Drops</td><td>' + (Math.round(f.price*change*1000) / 1000) + ' €</td></tr>');
                });
                return $ii;
              }, $('.billing-table')
            )
        }, this)
  });
  }, this)
});