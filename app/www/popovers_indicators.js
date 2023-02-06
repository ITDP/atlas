$(document).on('shiny:connected', function(event) {    
    
$('#indicator_bike :button').attr({'data-container':'body', 'data-toggle':'popover'});
$('#indicator_city :button').attr({'data-container':'body', 'data-toggle':'popover'});
$('#indicator_walk :button').attr({'data-container':'body', 'data-toggle':'popover'});
$('#indicator_transit :button').attr({'data-container':'body', 'data-toggle':'popover'});


$('#indicator_city > div > div:nth-child(1) > button').attr({'title':'Ahhhhhh', 'data-content': 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat' });


 $('[data-toggle="popover"]').popover(
          {trigger: 'hover', html: true, 
          viewport : {selector: 'indicator_bike .btn', padding: 0}, container: 'body'
          }
    );
    
    
}