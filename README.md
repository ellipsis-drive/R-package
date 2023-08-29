# R-package

The EDPackage is a wrapper around the Ellipsis Drive API. Using this package you can perform all API transactions using simple R objects.

##Install
You can install this package using remotes. If remotes is not yet installed, install remotes first.

```
install.packages('remotes')
```
With remotes installed you can install the EDPackage

```
remotes::install_github("ellipsis-drive/R-package")
```
The pakcage is now installed under the name EDPackage

```
library(EDPackage)
```

##Examples
List layers in your my Drive root:
```
token = EDPackage::account.logIn(username = 'demo_user', password = 'demo_user')
items = EDPackage::account.listRoot(rootName = 'myDrive', pathTypes = list('raster', 'vector') , token = token)
```

Create a raster layer in your Drive and upload a raster file to this layer.

```
pathToYourLocalFile = "/test.tif"

token = EDPackage::account.logIn(username = 'demo_user', password = 'demo_user')
rasterLayerId = EDPackage::path.raster.add( "some name", token)$id

timestampId = EDPackage::path.raster.timestamp.add(pathId = rasterLayerId, token = token)$id

EDPackage::path.raster.timestamp.file.add(pathId =rasterLayerId, timestampId=timestampId, filePath= pathToYourLocalFile, fileFormat='tif', token=token)
#don't forget to activate the timestamp once upload is completed
EDPackage::path.raster.timestamp.activate(rasterLayerId, timestampId, token)

```

Create a vector layer and upload a vector file to this layer:
```
pathToYourLocalFile = "/test.geojson"

token = EDPackage::account.logIn(username = 'demo_user', password = 'demo_user')
vectorLayerId = EDPackage::path.vector.add( "some name", token)$id

timestampId = EDPackage::path.vector.timestamp.add(pathId = vectorLayerId, token = token)$id

EDPackage::path.vector.timestamp.file.add(pathId =vectorLayerId, timestampId=timestampId, filePath= pathToYourLocalFile, fileFormat='geojson', token=token)

```

##Documentation
The function names and parameters are exactly the same as for the python package. You can find the documentation here: https://ellipsis-package.readthedocs.io/en/latest/


