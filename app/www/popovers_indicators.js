$('#indicator_bike > div > div:nth-child(1) > button').attr('data-container', 'body');

$('#indicator_bike > div > div:nth-child(1) > button').popover(
  {trigger: 'hover', html: true,  title:'Popover title', content:'<p>Popover content.</p>', boundary: 'viewport', container: 'body', offset: 10}
  );
  
  