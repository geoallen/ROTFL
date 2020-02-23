var filterdate = imageCollection4.filterDate('2013-06-01','2019-03-01')

//var workaround = [1];

var GaugeLocations = ee.FeatureCollection([
ee.Feature(ee.Geometry.Point([-69.7155556,46.70055556]),{"system:index":"0"}),
ee.Feature(ee.Geometry.Point([-69.0880556,47.11305556]),{"system:index":"1"}),
ee.Feature(ee.Geometry.Point([-69.0794444,47.0697222]),{"system:index":"2"}),
ee.Feature(ee.Geometry.Point([-68.58277778,47.2375]),{"system:index":"3"}),
ee.Feature(ee.Geometry.Point([-68.3716667,46.52305556]),{"system:index":"4"})])


var locational = function(feature){   
       var geom = feature.geometry();//set feature's geometry property to a variable
       var filterSpot = filterdate.filterBounds(geom);//filter image collection around the geometry 

      
        var setdate = function(image){
              
             var num = ee.String(image.get('DATE_ACQUIRED'));
             var setted =  image.set('date',num);
            return setted;
      
        };
        
        var times = filterSpot.map(setdate);
       
        var aggregate = times.aggregate_array('date');
        var darray = feature.set('dates_captured', aggregate);
        return darray;
}
var multiple = ee.FeatureCollection(three.map(locational));
var agPos = multiple.aggregate_array('dates_captured');
//print(agPos);


print(multiple);
Export.table.toDrive({
  collection: multiple,
  description: 'EveryReturn',
  fileFormat: 'CSV'
});
