$(document).ready(function() {

  // redirections
  checkRedirections();

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

  // initiate pricing table if needed
  initatePricingTable();

});

var lgList = ["ruby", "java", "php", "scala", "nodejs", "python"];

var activeHeadbar = function() {
  _.each(lgList, function(x, y) {
    if (_.contains(window.location.pathname.split( '/' ), x)) {
      $($(".cc_headbar__menu li")[y]).addClass("active");
    }
  })
}

var list = ["Clever Cloud Overview", "Ruby Runtime", "Java Runtime", "PHP Runtime", "Scala Runtime", "Node.js Runtime", "Python Runtime", "Databases and Services", "Add-ons", "Admin Console", "Get Help"];

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

var checkRedirections = function() {
  if (window.location.pathname == "/admin-console/apps-management/" && window.location.hash == "#ssh-keys") {
    window.location = "/admin-console/ssh-keys/";
  }

  if (window.location.pathname == "/admin-console/apps-management/" && window.location.hash == "#custom-domain-names") {
    window.location = "/admin-console/custom-domain-names/";
  }
}
$(document).ready(function() {
  $('.cc-content-img').magnificPopup({
    delegate: 'a',
    type: 'image'
  });
});

var initatePricingTable = function() {
  if (!_.contains(window.location.pathname.split( '/' ), "pricing")) {
    return;
  }

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
                    '</td><td>' + (Math.round((f.price*6)*change*1000) / 1000) + ' €</td></tr>');
                });
                return $ii;
              }, $('.billing-table')
            )
            initSortingTable();
          }, this)
    });
    }, this)
  });
}

/* Table initialisation */
//$(document).ready(function() {
var initSortingTable = function() {
  $('#example').dataTable( {
    "sDom": "<'row-fluid'<'span6'l><'span6'f>r>t<'row'<'span6'i><'span6'p>>",
    "sPaginationType": "bootstrap",
    "bPaginate": false,
    "bInfo": false,
    "oLanguage": {
      "sLengthMenu": "_MENU_ records per page"
    },
    "aaSorting": [],
    "aoColumnDefs": [
      //{ "asSorting": [ "asc" ], "aDataSort": [ 1 ], "aTargets": [ 1 ] }
    ]
  });
}
//} );
