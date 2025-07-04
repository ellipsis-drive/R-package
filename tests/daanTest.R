
#INSTALL

#install.packages("remotes")
#remotes::install_github("ellipsis-drive/R-package", DEP = FALSE)


#list root
library(EDPackage)
token = EDPackage::account.logIn(username = 'demo_user', password = 'demo_user')
items = EDPackage::account.listRoot(rootName = 'myDrive', pathTypes = list('raster', 'vector') , token = token)

#upload raster file
pathToYourLocalFile = "/test.tif"

token = EDPackage::account.logIn(username = 'demo_user', password = 'demo_user')
rasterLayerId = EDPackage::path.raster.add( "some name", token)$id

timestampId = EDPackage::path.raster.timestamp.add(pathId = rasterLayerId, token = token)$id

EDPackage::path.raster.timestamp.file.add(pathId =rasterLayerId, timestampId=timestampId, filePath= pathToYourLocalFile, fileFormat='tif', token=token)
#don't forget to activate the timestamp once upload is completed
EDPackage::path.raster.timestamp.activate(rasterLayerId, timestampId, token)

#upload vector file
pathToYourLocalFile = "/test.geojson"

token = EDPackage::account.logIn(username = 'demo_user', password = 'demo_user')
vectorLayerId = EDPackage::path.vector.add( "some name", token)$id

timestampId = EDPackage::path.vector.timestamp.add(pathId = vectorLayerId, token = token)$id

EDPackage::path.vector.timestamp.file.add(pathId =vectorLayerId, timestampId=timestampId, filePath= pathToYourLocalFile, fileFormat='geojson', token=token)




