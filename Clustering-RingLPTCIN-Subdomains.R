# Excise and visualize a subdomain of ring LPTC input neurons (rLPTCINs)
# to be used for clustering

library(elmr)

al=catmaid_get_annotationlist()

# fetch all the LPTC Input Neurons (LPTCINs)
lptcin=read.neurons.catmaid('LPTCIN')
regtemplate(lptcin)=FAFB

# add the side (Left/Right) and the subtype (uni,bi,ring) as a label
lptcin[,'label']=sub("LPTCIN.*","LPTCIN", lptcin[,'name'])

# Get all the RHS ring LPTCINs
right.rings=subset(lptcin, label=="Right rLPTCIN", rval='name')
right.rings
# Take one of the ring neurons and excise a piece of it flanked by two tags

ringo = read.neuron.catmaid((right.rings[1]))

ringo.cutone=subset(ringo,distal_to(ringo,node.pointno=ringo$tags$`upper bound end`),invert = T)
ringo.cuttwo=subset(ringo.cutone,distal_to(ringo.cutone,node.pointno=ringo$tags$`upper bound start`))

ringo.cutonebot=subset(ringo,distal_to(ringo,node.pointno=ringo$tags$`lower bound end`),invert = T)
ringo.cuttwobot=subset(ringo.cutonebot,distal_to(ringo.cutonebot,node.pointno=ringo$tags$`lower bound start`))

# Visualize the excised regions
nopen3d()
plot3d(ringo)
plot3d(ringo.cuttwo)
plot3d(ringo.cuttwobot)

#It is unclear how to apply the same process to the entire list

#lptcin.mids=nlapply(lptcin,subset,ringo.cuttwo, OmitFailures = T)
#lptcin.mids=nlapply(lptcin,subset,xyzmatrix(ringo.cuttwo), OmitFailures = T)
#lptcin.mids[,]
