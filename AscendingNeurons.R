# Short session with Mert Erginkaya and Kathrin Steck

library(elmr)
setwd("~/projects/EugeniaChiappe/")
sel=read_catmaid_selection('1002426-ANBranchAllTracedInputs-180407.json')

# remove central brain neuron
sel=subset(sel, skid!=1002425)

an=fetchn_fafb(sel$skid, mirror = F, reference=JFRC2013)
an.dps=fetchdp_fafb(sel$skid, mirror = F, reference=JFRC2013)

an.dps.tract=nlapply(an.dps, subset, s)

an.dps.tract=an.dps.tract[nvertices(an.dps.tract)>0]
an.dps.tract.aba=nblast_allbyall(an.dps.tract)
an.dps.tract.hc=nhclust(scoremat = an.dps.tract.aba)


s2=select3d()
an.dps.brain=nlapply(an.dps, subset, s2, invert=T)
an.dps.brain.aba=nblast_allbyall(an.dps.brain)
an.dps.brain.hc=nhclust(scoremat = an.dps.brain.aba)

# convert to JFRC2010
an.jfrc2=xform_brain(an, reference = JFRC2)
an.jfrc2.m=mirror_brain(an.jfrc2)
library(nat.amira)

open_amira(an.jfrc2, subdir=cutree(an.dps.tract.hc, k=4))
open_amira(an.jfrc2.m, subdir=paste0("g",cutree(an.dps.tract.hc, k=4)))


# contralateral inputs

an.contra=read_catmaid_selection('catmaid-AN-skeletons-2018-7-4.json')
an.contra=read_catmaid_selection('catmaid-skeletons-2018-7-4.json', readNeurons = T)

an.contra.jfrc2=xform_brain(an.contra, sample=FAFB,reference = JFRC2)
open_amira(an.contra.jfrc2)
