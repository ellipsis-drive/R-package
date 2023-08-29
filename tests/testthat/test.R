library(here)
token = EDPackage::account.logIn(username = 'BStarkenburg', password='Xpj3YhajXuE3yJw')
# print("works")
# ##access token
 EDPackage::account.accessToken.create(description = 'hoi', accessList = list(list('pathId'= '46e1e919-8b73-42a3-a575-25c6d45fd93b' , 'access'=list('accessLevel'=100))), token = token)
 tokenId = EDPackage::account.accessToken.get(token, listAll = TRUE)[['result']][[1]][['id']]
 EDPackage::account.accessToken.revoke(accessTokenId = tokenId, token = token)

folderId = '4f36d9af-e4d2-4b9d-a91c-6953a2c358a5'
print("access token done")



# ##account
demo_token = EDPackage::account.logIn("demo_user", "demo_user")
#
EDPackage::account.listRoot('myDrive', pathTypes = list('raster'), token = demo_token)
#
 EDPackage::account.listRoot('sharedWithMe',pathTypes = list('folder'), token = demo_token)
#
 r_raster = EDPackage::path.search(pathTypes = list('raster'), token=token);
#
 r_vector = EDPackage::path.search(pathTypes = list('vector'), token=token);
print("account done")
#
#
# ###files
 filePath = here::here("test_files", "0.tif")
 print(filePath)
 pathId = EDPackage::path.file.add(filePath, demo_token)[['id']]
 Sys.sleep(10)
 EDPackage::path.file.download(pathId = pathId, filePath =  filePath)
 EDPackage::path.trash(pathId, demo_token)
 EDPackage::path.delete(pathId, demo_token)
print("files done")
#
# ##user
#
 result = EDPackage::user.search('bstarkenburg')
 daanId <- list()
 for (x in result)
 {
   if (x[["username"]] == "bstarkenburg")
     daanId <- append(daanId, x)
 }

 daanId = daanId[[1]]
 EDPackage::user.get(daanId)
#
 result = EDPackage::user.search('demo_user')
demoId <- list()
for (x in result)
{
  if (x[["username"]] == "demo_user")
    demoId <- append(daanId, x)
}

demoId = demoId[[1]]
print("user done")
# # ##path
  rasterInfo = EDPackage::path.get('4f36d9af-e4d2-4b9d-a91c-6953a2c358a5', token)
#
info = EDPackage::path.get(folderId, token)
#
folderId = info[['id']]
 #maps = EDPackage::path.folder.listFolder(folderId, pathTypes=list('raster', 'vector'), token = token, listAll = TRUE)
 #folders = EDPackage::path.folder.listFolder(folderId, pathTypes=list('folder'), token = token, listAll = TRUE)

  # mapId = list()
  # for (m in maps[["result"]])
  # {
  #  if (!m[["trashed"]][[1]][["id"]])
  #     mapId <- append(mapId, m)
  # }
 # print(mapId)
#mapId <- mapId[[1]]
#
# #crash
#
#
#EDPackage::path.editMetaData(pathId = mapId, token = token, description = 'test')
#
 addedFolderId = EDPackage::path.folder.add(  'test', token = token, parentId = folderId)[['id']]
#
 EDPackage::path.trash(addedFolderId, token)
 EDPackage::path.recover(addedFolderId, token)
#
 addedRasterId = EDPackage::path.raster.add(  'test2', token = token, parentId = folderId)[['id']]
#
# # Daan case
EDPackage::path.move(list(addedRasterId), addedFolderId, token)
#
#
 EDPackage::path.trash(addedFolderId, token)
# # Daan case 2, apimanager zegt dat includeTrashed false is terwijl true
EDPackage::path.delete(pathId = addedFolderId, token = token, recursive = TRUE)
#
#
#EDPackage::path.editPublicAccess(pathId = folderId, token = token, access=list('accessLevel'=0), hidden = FALSE)
#EDPackage::path.editPublicAccess(pathId = folderId, token = token, access= list('accessLevel'=100), hidden = TRUE)
#
 EDPackage::path.favorite(folderId, token=token)
 EDPackage::path.unfavorite(folderId, token=token)
#
 print("path done")
# ###invites
# inviteId =  EDPackage::path.invite.send(pathId = folderId, token=token, userId = demoId, access = list('accessLevel'= 200, 'processingUnits'=10000))[['id']]
# # #
#  EDPackage::path.invite.getPathInvites(folderId, demo_token)
# # #
# # #
# EDPackage::path.invite.getYourInvites(demo_token)
# # #
# EDPackage::path.invite.revoke(pathId = folderId, inviteId = inviteId, token = token)
# # #
# # #
#  inviteId =  EDPackage::path.invite.send(pathId = folderId, token=token, userId = demoId, access = list('accessLevel'= 200, 'processingUnits':10000))[['id']]
# # #
# EDPackage::path.invite.decline(folderId, inviteId, demo_token)
# # #
# # #
# inviteId =  EDPackage::path.invite.send(pathId = folderId, token=token, userId = demoId, access = list('accessLevel'= 200, 'processingUnits'=10000))[['id']]
#  EDPackage::path.invite.accept(folderId, inviteId, demo_token)
#
 print("invites done")
# ##members
 #members = EDPackage::path.member.get(folderId, token, memberType = list('direct'))
# # #
# # #
#  daanMemberId = list()
# # #
#  for (m in members)
#  {
#    print(m)
#    if (m[["user"]][["username"]] == "bstarkenburg")
#      daanMemberId <- append(daanMemberId, m)
#  }
#  print(glue::glue("daanmemberId: {daanMemberId}"))
#  daanMemberId <- daanMemberId[[1]]
# # #
#  EDPackage::path.member.edit(pathId = folderId, userId = daanMemberId, access = list('accessLevel' = 200) ,token = token)
# # #
#  EDPackage::path.member.delete(folderId, daanMemberId, token)
#
 print("members done")
# ##usage
pathId = '8a11c27b-74c3-4570-bcd0-64829f7cd311'
#
users = EDPackage::path.usage.getActiveUsers(pathId = pathId, token = token, listAll=FALSE)
EDPackage::path.usage.getUsage(pathId = pathId, userId = users[['result']][[1]][['user']][['id']], token =token)
#EDPackage::path.usage.getAggregatedUsage(pathId = pathId, loggedIn = FALSE, token = token)
print("members done")
##raster and uploads
mapId = EDPackage::path.raster.add( 'test', token, parentId = folderId)[['id']]
#
# # Daan case 3
# # Crash want geen banden
#EDPackage::path.raster.edit(mapId, token, interpolation = 'nearest')
#
timestampId = EDPackage::path.raster.timestamp.add(mapId, token)[['id']]
#
#
dateFrom = Sys.time()
dateTo = Sys.time()
EDPackage::path.raster.timestamp.edit(mapId, timestampId, token, description = 'hoi', date=list('from'=dateFrom, 'to'=dateTo))
#
uploadId = EDPackage::path.raster.timestamp.file.add(pathId = mapId, timestampId = timestampId, filePath = filePath, fileFormat = 'tif', token = token,noDataValue = -1)[['id']]
Sys.sleep(10)
EDPackage::path.raster.timestamp.file.download(pathId = mapId, timestampId = timestampId, fileId = uploadId, filePath = 'C:/Users/spamb/Downloads/out.tif', token = token)
#
EDPackage::path.raster.timestamp.file.trash(pathId = mapId, timestampId = timestampId, fileId= uploadId, token = token)
EDPackage::path.raster.timestamp.file.recover(pathId = mapId, timestampId = timestampId, fileId= uploadId, token = token)
EDPackage::path.raster.timestamp.file.trash(pathId = mapId, timestampId = timestampId, fileId= uploadId, token = token)
EDPackage::path.raster.timestamp.file.delete(mapId, timestampId, uploadId, token)
uploadId = EDPackage::path.raster.timestamp.file.add(pathId = mapId, timestampId = timestampId, filePath=filePath, fileFormat='tif', token = token)[['id']]
#
uploads = EDPackage::path.raster.timestamp.file.get(mapId, timestampId, token)
#
#
EDPackage::path.raster.timestamp.activate(mapId, timestampId, token)
#
while (EDPackage::path.get(mapId, token)[['raster']][['timestamps']][[1]][['status']] != 'active')
   Sys.sleep(2)

EDPackage::path.raster.timestamp.deactivate(mapId, timestampId, token)
while (EDPackage::path.get(mapId, token)[['raster']][['timestamps']][[1]][['status']] != 'passive')
   Sys.sleep(2)
 EDPackage::path.raster.timestamp.activate(mapId, timestampId, token)

 info = EDPackage::path.get(mapId, token)
 timestamp = info[['raster']][['timestamps']][[1]]

 while (EDPackage::path.get(mapId, token)[['raster']][['timestamps']][[1]][['status']] != 'active')
   Sys.sleep(1)

 info = EDPackage::path.get(mapId, token)
 timestamp = info[['raster']][['timestamps']][[1]]
#
EDPackage::path.raster.editBand(mapId, 1, 'hoi', token)
#
#
 EDPackage::path.raster.timestamp.trash(mapId, timestampId, token)
EDPackage::path.raster.timestamp.recover(mapId, timestampId, token)
EDPackage::path.raster.timestamp.trash(mapId, timestampId, token)
EDPackage::path.raster.timestamp.delete(mapId, timestampId, token)
#
EDPackage::path.trash(mapId, token)
EDPackage::path.recover(mapId, token)
EDPackage::path.trash(mapId, token)
EDPackage::path.delete(mapId, token)
#
print("raster and uploads done")
# ##raster information retrieval
mapId = '59caf510-bab7-44a8-b5ea-c522cfde4ad7'
timestampId = 'f25e120e-ca8f-451f-a5f4-33791db0f2c5'
#
#
styleId = EDPackage::path.get(mapId, token)[['raster']][['styles']][[1]][['id']]
#
xMin  = 5.60286
yMin=  52.3031
xMax  = 5.60315
yMax  = 52.30339
#
extent = list('xMin'=xMin,'yMin'=yMin,'xMax'=xMax,'yMax'=yMax )
result = EDPackage::path.raster.timestamp.getRaster(pathId = mapId, timestampId = timestampId, style=styleId, extent = extent, token = token, epsg = 4326)
#
raster = result[['raster']]
#

 EDPackage::util.plotRaster(raster)
#
# # Daan case 4 (extent not loaded)
r = EDPackage::path.raster.timestamp.getSampledRaster(pathId = mapId, timestampId=timestampId, style=styleId, extent = extent, width = 1024, height = 1024, epsg=4326, token = token)
raster = r[['raster']]
EDPackage::util.plotRaster(raster)
#
#
bounds = EDPackage::path.raster.timestamp.getBounds(mapId, timestampId, token)
#
data = EDPackage::path.raster.timestamp.analyse(mapId, list(timestampId), bounds, token=token, epsg = 4326)
print("raster retrieval done")
###raster downloads
mapId = '1eea3d2f-27b3-4874-b716-87852c3407c1'
timestampId = "ba5b418a-a39e-4d84-9411-e23c096085a3"
uploads = EDPackage::path.raster.timestamp.file.get(mapId, timestampId, token)
uploadId = uploads[['result']][[1]][['id']]
#
file_out = here::here("test_files", "out.tif")#'C:/Users/spamb/Downloads/out.tif'
Sys.sleep(10)
 EDPackage::path.raster.timestamp.file.download(pathId = mapId, timestampId = timestampId, token = token, fileId = uploadId, filePath = file_out)
#
# ##from here
pendinDownloads = EDPackage::path.raster.timestamp.order.get(token)

print("raster downloads done")
####raster styling
parameters = list("angle"=45,"bandNumber"=1,"alpha"=1)
method = "hillShade"
layerId = EDPackage::path.raster.style.add(mapId, 'hoi2', method, parameters, token,  default = TRUE)[['id']]
EDPackage::path.raster.style.edit(mapId, layerId, method= method, token=token, parameters = parameters, default = FALSE)
EDPackage::path.raster.style.delete(mapId, layerId, token)


print("raster styling done")
###vector layers
 mapId = EDPackage::path.vector.add( 'test', token)[['id']]
#
#
layerId = EDPackage::path.vector.timestamp.add(mapId,  token = token)[['id']]
#
EDPackage::path.vector.timestamp.edit(mapId, layerId, token, description = 'adsfd')
#
EDPackage::path.vector.timestamp.trash(mapId, layerId, token)
 EDPackage::path.vector.timestamp.recover(mapId, layerId, token)
 EDPackage::path.vector.timestamp.trash(mapId, layerId, token)
 EDPackage::path.vector.timestamp.delete(mapId, layerId, token)
 layerId = EDPackage::path.vector.timestamp.add(mapId, description = 'test', token = token)[['id']]
#
#
 print("vector layers done")
# ###vector uploads
filePath = here::here("test_files", "test.zip")
print(filePath)
 uploadId = EDPackage::path.vector.timestamp.file.add(pathId = mapId, timestampId = layerId, filePath = filePath, token = token, fileFormat = 'zip')[['id']]
Sys.sleep(10)
file_out = here::here("test_files", "out.zip")#paste0(filePath, "/out.zip")#'C:/Users/spamb/Downloads/out.zip'
EDPackage::path.vector.timestamp.file.download(pathId = mapId, timestampId = layerId, fileId = uploadId, filePath = file_out, token = token)
#
upload = EDPackage::path.vector.timestamp.file.get(pathId = mapId,  timestampId = layerId, token = token)[['result']][[1]]
while (upload[['status']] != 'completed')
{
   Sys.sleep(1)
   upload = EDPackage::path.vector.timestamp.file.get(pathId = mapId,  timestampId = layerId, token = token)[['result']][[1]]
 }

upload = EDPackage::path.vector.timestamp.file.get(pathId = mapId,  timestampId = layerId, token = token)[['result']][[1]]
#
#
print("vector uploads done")
# #layer methods

bounds = EDPackage::path.vector.timestamp.getBounds(mapId, layerId, token)

xMin = sf::st_bbox(bounds)[[1]]
yMin = sf::st_bbox(bounds)[[2]]
xMax = sf::st_bbox(bounds)[[3]]
yMax = sf::st_bbox(bounds)[[4]]


bounds = list('xMin'=xMin, 'xMax'=xMax, 'yMin'=yMin, 'yMax'=yMax)

 sh = EDPackage::path.vector.timestamp.getFeaturesByExtent(pathId = mapId, timestampId = layerId, extent =  bounds, token = token, listAll = TRUE, epsg = 4326)
 sh = EDPackage::path.vector.timestamp.getFeaturesByExtent(pathId = mapId, timestampId = layerId, extent =  bounds, token = token, listAll = FALSE, pageStart = sh[['nextPageStart']], epsg = 4326)


 Sys.sleep(30)
sh = EDPackage::path.vector.timestamp.listFeatures(mapId, layerId, token, listAll = FALSE)

 featureIds = sh[['result']][['properties.id']]

r =  EDPackage::path.vector.timestamp.getFeaturesByIds(mapId, layerId, featureIds, token)


r = EDPackage::path.vector.timestamp.getChanges(mapId, layerId, token, listAll = TRUE)

EDPackage::path.vector.editFilter(mapId, list(list('property'='gml_id')), token)

# r = EDPackage::path.get(mapId, token)
# blocked = r[['vector']][['timestamps']][[1]][['availability']][['blocked']]
# while (blocked)
# {
#   Sys.sleep(1)
#   blocked = r[['vector']][['timestamps']][[1]][['availability']][['blocked']]
# }

r = EDPackage::path.get(mapId, token)
blocked = r[['vector']][['timestamps']][[1]][['availability']][['blocked']]
print("layer done")

###feature module

features = sh[['result']][1:2,]
#print(features)
featureIds = features[['properties.id']]

EDPackage::path.vector.timestamp.feature.add(mapId, layerId, features, token, cores = 10)

levelsOfDetail1 = sf::st_simplify(features, preserveTopology = TRUE, dTolerance = 1)
EDPackage::path.vector.timestamp.feature.add(pathId = mapId, timestampId =  layerId, features=features, levelOfDetail1=levelsOfDetail1, token=token)



EDPackage::path.vector.timestamp.feature.edit(mapId, layerId, featureIds = features[['properties.id']], token = token, features = features)

EDPackage::path.vector.timestamp.feature.trash(mapId, layerId, featureIds, token)
EDPackage::path.vector.timestamp.feature.recover(mapId, layerId, featureIds, token)
featureId = featureIds[[1]]
EDPackage::path.vector.timestamp.feature.versions(mapId, layerId, featureId, token)

print("feature done")
###message module
EDPackage::path.vector.timestamp.feature.message.add(mapId, layerId, featureId, token, text= 'hoi')
set.seed(42)
image <- as.im(matrix(runif(256*256), nrow = 256, ncol = 256))
EDPackage::path.vector.timestamp.feature.message.add(mapId, layerId, featureId, token, text= 'hoi', image = image)

messages = EDPackage::path.vector.timestamp.feature.message.get(mapId, layerId, featureIds=list(featureId), token = token)
messageId <- list()
for (m in messages[["result"]])
{
  if (!is.null(m[["thumbnail"]]))
    messageId <- append(messageId, m)
}
messageId <- messageId[[1]][["id"]]

EDPackage::path.vector.timestamp.feature.message.getImage(mapId, layerId, messageId, token)

EDPackage::path.vector.timestamp.feature.message.trash(mapId, layerId, messageId, token)
EDPackage::path.vector.timestamp.feature.message.recover(mapId, layerId, messageId, token)

print("message done")
###series module
date = Sys.time()

seriesData <- data.frame(list('x'= list(1,2,3,4)))
seriesData[['date']] = date
EDPackage::path.vector.timestamp.feature.series.add(pathId = mapId, timestampId = layerId, featureId = featureId, seriesData = seriesData, token = token)


EDPackage::path.vector.timestamp.feature.series.info(mapId, layerId, featureId,token)

r = EDPackage::path.vector.timestamp.feature.series.get(mapId, layerId, featureId, token = token, listAll = TRUE)


seriesId = r[['result']][['id']][[1]]

# Stupid bug, ask daan
EDPackage::path.vector.timestamp.feature.series.trash(mapId, layerId, featureId, list(seriesId), token)

EDPackage::path.vector.timestamp.feature.series.recover(mapId, layerId, featureId, list(seriesId), token)
# No bugs...
EDPackage::path.vector.timestamp.feature.series.changelog(mapId, layerId, featureId, token = token)

print("series done")
#style module

parameters = list("alpha"=0.5,"width"=2,"radius"=list("method"="constant","parameters"=list("value"=7)),"property"="gml_id")
styleId = EDPackage::path.vector.style.add(mapId, 'test', 'random', parameters, token = token, default = FALSE)[['id']]

EDPackage::path.vector.style.edit(mapId, styleId, token, name = 'sfd', default = FALSE)
EDPackage::path.vector.style.delete(mapId, styleId, token)

print("style done"
)
## properties module
featurePropertyId = EDPackage::path.vector.featureProperty.add(pathId = mapId, name = 'new', featurePropertyType = 'string', token = token)[['id']]
EDPackage::path.vector.featureProperty.trash(mapId,  featurePropertyId, token)

EDPackage::path.vector.featureProperty.recover(mapId, featurePropertyId, token)
EDPackage::path.vector.featureProperty.edit(mapId, featurePropertyId, token, required = TRUE)
print("properties done")
### order module

orderId = EDPackage::path.vector.timestamp.order.add(mapId, layerId, token, extent = list('xMin'=xMin, 'xMax'=xMax, 'yMin'=yMin, 'yMax'=yMax))[['id']]

order = EDPackage::path.vector.timestamp.order.get(token)[[1]]
while (order[['status']] != 'completed')
  Sys.sleep(1)
order = EDPackage::path.vector.timestamp.order.get(token)[[1]]

file_out = here::here("test_files", "out.zip")#paste0(file'C:/Users/spamb/Downloads/out.zip'
EDPackage::path.vector.timestamp.order.download(orderId, file_out, token)
EDPackage::path.trash(mapId, token)
EDPackage::path.delete(mapId, token)

print("order done")


########3some more specific bounds tests
rasterId = '56e20fa2-f014-44c1-b46a-cde78e7e6b7e'
timestampId = EDPackage::path.get(rasterId,token)[['raster']][['timestamps']][[1]][['id']]
EDPackage::path.raster.timestamp.file.get(pathId = rasterId, timestampId = timestampId, token = token)

EDPackage::path.raster.timestamp.getBounds(pathId = rasterId, timestampId = timestampId, token=token)

vectorId = '67e66823-8bbc-4ace-816a-c4e34282676c'
timestampId = 'bc73c75a-cc74-4bb5-a609-ef01992bcc9a'

EDPackage::path.vector.timestamp.file.get(pathId = vectorId, timestampId = timestampId, token = token)

EDPackage::path.vector.timestamp.getBounds(pathId = vectorId, timestampId = timestampId, token=token)
print("bounds tests done")



###hashtags
pathId = '59caf510-bab7-44a8-b5ea-c522cfde4ad7'
EDPackage::path.hashtag.add(pathId = pathId, hashtag = 'xxx', token = token)
EDPackage::path.hashtag.search('x')
EDPackage::path.hashtag.delete(pathId = pathId, hashtag = 'xxx', token = token)
print("hashtags done")
####views

viewId = EDPackage::view.add(pathId = pathId, name = 'temp', persistent = TRUE, layers = list(list('type'='ellipsis', 'id'=pathId, 'selected'=TRUE ), list('type'='base', 'selected'=TRUE )), token = token)[['id']]

EDPackage::view.get(viewId = viewId)

EDPackage::view.listViews(token = token)

EDPackage::view.edit(viewId = viewId, name = 'temp2', token = token)

EDPackage::view.delete(viewId = viewId, token = token)
print("views done")
