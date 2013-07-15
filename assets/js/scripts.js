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
                    '</span></td><td>'
                    +i.name+
                    '</td><td>'+f.price+' Drops</td><td>' + (Math.round((f.price*6)*change*1000) / 1000) + ' €</td></tr>');
                });
                return $ii;
              }, $('.billing-table')
            )
        }, this)
  });
  }, this)
});

$(document).ready(function() {

  //Instanciate an index for each h3
  if($(".cc-content h3").length > 1) {
    $("h2").append("<h4>Table of Contents</h4><ul id='cc-tableofcontent__list'></ul>");
    $(".cc-content h3").each( function() {
      var section_id = $(this).attr("id");
      var line = "<li><a href=#"+section_id+'>'+$(this).text()+'</a></li>';
      var $newli = $(line);
      $("#cc-tableofcontent__list").append($newli);
    });
  }
  
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

  // Parts of menu hidden if needed
  activeHeadbar();

  // hide non concerned parts in left menu bar
  hideNonConcernedMenuElts();
  
  // reorder left-menu
  reorderLeftMenu();

});

var lgList = ["java", "php", "scala", "nodejs", "python"];

var activeHeadbar = function() {
  _.each(lgList, function(x, y) {
    if (_.contains(window.location.pathname.split( '/' ), x)) {
      $($(".cc_headbar__menu li")[y]).addClass("active");
    }
  })
}

var list = ["Clever Cloud Overview", "Java Runtime", "PHP Runtime", "Scala Runtime", "Node.js Runtime", "Python Runtime", "Databases and Services", "Add-ons", "Admin Console", "Get Help"];

var reorderLeftMenu = function() {
  var temp = Array();
  _.each($(".cc-sidebar span"), function(key, val){
    temp.push({"val":$(key).text(), "span": key, "ul": $(".cc-sidebar ul")[val]});
  })
  var html = "";
  _.each(list, function(elt) {
    var htmltemp = _.find(temp, function(elt1){
      return elt1.val == elt;
    });
    html = html + htmltemp.span.outerHTML + htmltemp.ul.outerHTML;
  });
  $(".cc-sidebar").html(html);
}

var hideNonConcernedMenuElts = function() {
  _.each(lgList, function(x, y) {
    if (!_.contains(window.location.pathname.split( '/' ), x)) {
      $($($(".cc-sidebar ." + x))[0]).hide();
      $($($(".cc-sidebar ." + x))[1]).hide();
    }
  })
}