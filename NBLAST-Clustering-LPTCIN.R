library(elmr)

al=catmaid_get_annotationlist()
types=c("rLPTCIN", "uLPTCIN", "bLPTCIN")

# fetch all the neurons
lptcin=read.neurons.catmaid('LPTCIN')
regtemplate(lptcin)=FAFB
ongoing=catmaid_skids("Chiappe-Ongoing")

# nb as.character because skids are numeric
lptcin=setdiff(lptcin, as.character(ongoing))

lptcin[,'side']='R'

rside='RIGHT HEMISPHERE'
lside='LEFT HEMISPHERE'

lskids=as.character(intersect(names(lptcin), catmaid_skids(lside)))
lptcin[lskids,'side']='L'

nopen3d()
plot3d(lptcin, soma=T, col=side)
plot3d(FAFB)

# example of looking for a neuron (draw a box in rgl window)
# find.neuron(db = lptcin)

lptcin[,'label']=sub("LPTCIN.*","LPTCIN", lptcin[,'name'])

## Cluster those neurons with NBLAST

lptcin.dps=dotprops(lptcin/1000, resample=1, k=5)
lptcin.aba=nblast_allbyall(lptcin.dps)
lptcin.hc=nhclust(scoremat = lptcin.aba)

plot(lptcin.hc, labels=lptcin[,'label'])
plot3d(lptcin.hc, db=lptcin, k=4, soma=T)
clear3d()

library(dendroextras)
lptcin.dend=colour_clusters(lptcin.hc, k=4)

# fetch labels in dendrogram order and then set them
labels(lptcin.dend) <- lptcin[labels(lptcin.dend),'label']
par(mar=c(8,2,2,2))
plot(lptcin.dend)

# Let's mirror these neurons

lptcin.jfrc2013 <- xform_brain(lptcin, reference = JFRC2013)
lptcin.jfrc2013.m <- mirror_brain(lptcin.jfrc2013, subset=lptcin[,"side"]=="L")
clear3d()
plot3d(lptcin.jfrc2013.m, col=side)

# let's cluster
lptcin.jfrc2013.dps=dotprops(lptcin.jfrc2013.m, resample=1, k=5)
lptcin.jfrc2013.aba=nblast_allbyall(lptcin.jfrc2013.dps)
lptcin.jfrc2013.hc=nhclust(scoremat = lptcin.jfrc2013.aba)
plot(lptcin.jfrc2013.hc,labels=lptcin[,'label'])

# Interim conclusion is that LHS and RHS are too different after mapping


## Let's try clustering RHS neurons only

rids=subset(lptcin, side=='R', rval='name')
lptcin.hcr=nhclust(neuron_names = rids, scoremat = lptcin.aba)
plot(lptcin.hcr, labels = lptcin[rids,'label'])
abline(h=.65, lty=2) # gives 6 groups

# ... if we want the coloured dendrogram, see above
lptcin.dr=as.dendrogram(lptcin.hcr)
plot(lptcin.dr)

nopen3d()
plot3d(lptcin.hcr, k=6, groups = 1:3, db=lptcin)

# nb dendroextras::slice gives groups numbered in dendrogram order
# unlike cutree which gives groups numbered by the original sort order of the
# first neuron in the group
lptcin.hcr.groups=dendroextras::slice(lptcin.hcr, k=6)
g5=subset(lptcin.hcr, groups=5, k=6)
g6=subset(lptcin.hcr, groups=6, k=6)

lptcin[,'group']=lptcin.hcr.groups[match(lptcin[,'skid'], names(lptcin.hcr.groups))]


# this group contains incomplete neurons
g3=subset(lptcin.hcr, k=6, groups=3)
# catmaid_set_annotations_for_skeletons(g3, "ME_TRACE_MORE")
