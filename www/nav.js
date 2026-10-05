$(document).on('shiny:connected', function(){
  Shiny.addCustomMessageHandler('showPage', function(id){
    $('.page-section').removeClass('active-page');
    $('#page_'+id).addClass('active-page');
    $('.nav-link').removeClass('active');
    $('#nav_'+id).addClass('active');
    window.scrollTo({top:0,behavior:'smooth'});
  });
});
