shinyjs.seek = function(params) {
  var defaultParams = {
    time : 0.0
  };
  params = shinyjs.getParams(params, defaultParams);
  document.getElementById('showreel').currentTime = params.time;
};



//shinyjs.currentPage = function(tbl) {
//  return tbl.page();
//};

//shinyjs.setCurrentPage = function(tbl, pageNum) {
//  tbl.page(pageNum).draw('page');
//};



//shinyjs.listenForDelete = function(params) {
//  var defaultParams = {
//    tablename : 'commentsTable'
//  };
//  params = shinyjs.getParams(params, defaultParams);
//  var tbl = document.getElementById(params.tablename);
//  tbl.addEventListener('keypress', function(evt) {
//    console.log("Pressed key: " + evt.key);
//  });
//}; 
