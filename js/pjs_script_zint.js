// scraper_PaddyPower.js

// Create a webpage object
var page = require('webpage').create();

// Include the File System module for writing to files
var fs = require('fs');

// Specify source and path to output file
//var url  = 'https://orise.orau.gov/internships-fellowships/recent-graduates.html'
var url = "https://www.zintellect.com/Catalog"
var path = 'z_int_catalog.html'

page.open(url, function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});