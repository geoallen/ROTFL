//tool to collect capture dates of landsat images over a specific locations, such as USGS gauge locations//
//if clouds are present near gauge, return '0' instead of image capture date//

//five sample gauge locations
var GaugeLocations = ee.FeatureCollection([
ee.Feature(ee.Geometry.Point([-69.7155556,46.70055556]),{"system:index":"0"}),
ee.Feature(ee.Geometry.Point([-69.0880556,47.11305556]),{"system:index":"1"}),
ee.Feature(ee.Geometry.Point([-69.0794444,47.0697222]),{"system:index":"2"}),
ee.Feature(ee.Geometry.Point([-68.58277778,47.2375]),{"system:index":"3"}),
ee.Feature(ee.Geometry.Point([-68.3716667,46.52305556]),{"system:index":"4"})])

var RADIX = 2;//for bitwise conversion portion
var filterdate = imageCollection.filterDate('2005-01-02','2013-06-05');



var locational = function(feature){   
       var geom = feature.geometry();//set feature's geometry property to a variable
       var filterSpot = filterdate.filterBounds(geom);//filter image collection around the geometry 


        var setdate = function(image){
			//check for clouds before date is set
              var image_qa = image.select('BQA');
              var cloudMask = ee.Image(0);
              
              ////////////////////////////////////////////////////////////////////////////////////////////////
              /////////////////////convert BQA band to a cloud mask//////////////////////////////////////////
              var extractQABits = function (qaBand, bitStart, bitEnd) {
                var numBits = bitEnd - bitStart + 1;
                var qaBits = qaBand.rightShift(bitStart).mod(Math.pow(RADIX, numBits));
                //Map.addLayer(qaBits, {min:0, max:(Math.pow(RADIX, numBits)-1)}, 'qaBits');
                return qaBits;
              };
              
              // Create a mask for the dual QA bit "Cloud Confidence".
              var bitStartCloudConfidence = 5;
              var bitEndCloudConfidence = 6;
              var qaBitsCloudConfidence = extractQABits(image_qa, bitStartCloudConfidence, bitEndCloudConfidence);
              // Test for clouds, based on the Cloud Confidence value.
              var testCloudConfidence = qaBitsCloudConfidence.gte(2);
              cloudMask = cloudMask.where(testCloudConfidence.eq(1),1)
              //////////////////////////////////////////////////////////////////////////////////////////////
              /////////////////////////////////////////////////////////////////////////////////////////////
              
              //sum over the cloud mask (no clouds should = 0)
              var sum = ee.Number(cloudMask.reduceRegion({
                                reducer: ee.Reducer.sum(),
                                geometry: geom.buffer(500),
                                scale: 20,
                                maxPixels: 1e9
                              }).get('constant'));
              
              //create list with sum then filter list for nulls(georeferencing problem with landsat footprint?)
              var tempA = ee.List([sum,-9999])
              var tempB = ee.Number(tempA.reduce(ee.Reducer.firstNonNull()))
              
              var setted;
              //if no band values(georeferencing probelm?) set return to 'error'
              var error = ee.String('error');
              var dateNum = ee.String(image.get('DATE_ACQUIRED'));
              var def = ee.Number(0);//result to return if there are clouds within buffer
              
              //is the image actually over the gauge?
              var conditional2 = function(image){
                return ee.Algorithms.If((tempB).lt(-100),
                          image.set('date',error),
                          setted = conditional(image))
              };   
              
              //are there any clouds over the gauge? 
              var conditional = function(image) {
              return ee.Algorithms.If((tempB).eq(0),
                          image.set('date',dateNum),
                          image.set('date',def));
              };          
              
              //return either: no image over gauge('error'), clouds over gauge('0'), gauge visible('date_aquired')
              var setted1 = conditional2(image);
              return setted1;
      
        };
        
        var times = filterSpot.map(setdate);
       
        var aggregate = times.aggregate_array('date');
        var darray = feature.set('dates_captured', aggregate);
        return darray;
}
var allGaugesReturn = ee.FeatureCollection(GaugeLocations.map(locational));
var agPos = allGaugesReturn.aggregate_array('dates_captured');
//print(agPos);

//export to CSV
print(allGaugesReturn);
Export.table.toDrive({
  collection: allGaugesReturn,
  description: 'GaugeDates',
  fileFormat: 'CSV'
});
