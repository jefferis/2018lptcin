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
right.rings=subset(lptcin, label=="Right rLPTCIN")
right.rings[,]
# Take one of the ring neurons and excise a piece of it flanked by two tags

ringo = right.rings[[1]]

extract_ring_subdomain <- function(ringo) {
  ringo.cutone=subset(ringo,distal_to(ringo,node.pointno=ringo$tags$`upper bound end`),invert = T)
  ringo.cuttwo=subset(ringo.cutone,distal_to(ringo.cutone,node.pointno=ringo$tags$`upper bound start`))
  # ringo.cuttwo.nodes=distal_to(ringo.cutone,node.pointno=ringo$tags$`upper bound start`)
  ringo.cuttwo.nodeids=ringo.cuttwo$d$PointNo

  # ringo.cutone.downstream=distal_to(ringo,node.pointno=ringo$tags$`upper bound end`)
  # ringo.cuttwo.downstream=distal_to(ringo.cutone,node.pointno=ringo$tags$`upper bound start`)
  # ringo.top.tokeep=setdiff(ringo.cuttwo.downstream, ringo.cutone.downstream)


  ringo.cutonebot=subset(ringo,distal_to(ringo,node.pointno=ringo$tags$`lower bound end`),invert = T)
  # ringo.cuttwobot.nodes=distal_to(ringo.cutonebot,node.pointno=ringo$tags$`lower bound start`)
  ringo.cuttwobot=subset(ringo.cutonebot,distal_to(ringo.cutonebot,node.pointno=ringo$tags$`lower bound start`))
  ringo.cuttwobot.nodeids=ringo.cuttwobot$d$PointNo

  # now use the nodes from the top and bottom to define a single subset
  # first though, we need to translate the indices from e.g. ringo.cutone back to
  ringo.both.nodeids=c(ringo.cuttwo.nodeids, ringo.cuttwobot.nodeids)
  # now subset wants node indices not unique ids
  node.indices=match(ringo.both.nodeids, ringo$d$PointNo)
  ringo.both=subset(ringo, node.indices)
  ringo.both
}

ringo1.cut=extract_ring_subdomain(right.rings[[1]])


ring.subdomains=nlapply(right.rings, extract_ring_subdomain)

# Visualize the excised regions
#nopen3d()
#plot3d(ringo)
#plot3d(ringo.cuttwo)
#plot3d(ringo.cuttwobot)
#

ring.subdomains.dps=dotprops(ring.subdomains/1e3, k=5, resample=1)
nopen3d()
plot3d(ring.subdomains.dps)

ring.subdomains.aba=nblast_allbyall(ring.subdomains.dps)
ring.subdomains.hc=nhclust(scoremat = ring.subdomains.aba)

plot(ring.subdomains.hc)

plot(ring.subdomains.hc, labels = ring.subdomains[,"name"])


