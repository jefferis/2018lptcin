# code comparing input numbers onto HS cells
# and input position
# assumes NBLAST-Clustering-LPTCIN.R has already been run

hsrout=catmaid_get_connectors_between(pre_skids = 'HS Cell Right', post_skids = lptcin[,'skid'])
hsrin=catmaid_get_connectors_between(post_skids = 'HS Cell Right', pre_skids = lptcin[,'skid'])

hsrout$groupname="other"
hsrout$groupname[hsrout$post_skid %in% g6]="uLPTCIN incomplete"
hsrout$groupname[hsrout$post_skid %in% g5]="uLPTCIN complete"

hsrout$group=lptcin.hcr.groups[match(hsrout$post_skid, names(lptcin.hcr.groups))]

table(hsrout$post_skid,hsrout$pre_skid)

table(hsrin$pre_skid, hsrin$post_skid)
hscellnames=sub(".*(HS[NES]).*","\\1",catmaid_get_neuronnames('HS Cell Right'))

hsrout$hscell=hscellnames[match(hsrout$pre_skid,names(hscellnames))]
table(hsrout$post_skid,hsrout$hscell)
hsrin$hscell=hscellnames[match(hsrin$post_skid,names(hscellnames))]
table(hsrin$pre_skid, hsrin$hscell)
hsrin$group=lptcin.hcr.groups[match(hsrin$pre_skid, names(lptcin.hcr.groups))]
table(hsrin$group, hsrin$hscell)
groupnames=c(`1`='funny', '2'='ring', '3'='ring incomplete', '4'='bilat', '5'='uni', '6'='uni incomplete')
hsrin$groupname=groupnames[match(hsrin$group, names(groupnames))]
table(hsrin$groupname, hsrin$hscell)


head(hsrin)

hssin=subset(hsrin, hscell=='HSS')
points3d(hssin[,c("post_node_x","post_node_y","post_node_z")])

hscells=read.neurons.catmaid('HS Cell Right')
hscells[,'hscell']=hscellnames
plot3d(hscells, hscell=='HSS')
clear3d()

plot3d(hscells, hscell=='HSS')
spheres3d(subset(hssin[,c("post_node_x","post_node_y","post_node_z")], hssin[,'groupname']=="bilat"), radius=500)
spheres3d(subset(hssin[,c("post_node_x","post_node_y","post_node_z")], grepl("uni", hssin[,'groupname'])), col='blue', radius=500)
spheres3d(subset(hssin[,c("post_node_x","post_node_y","post_node_z")], hssin[,'groupname']%in%c("ring", "funny", "ring incomplete")), col='cyan', radius=500)


hsrin.all=catmaid_get_connectors_between(post_skids = 'HS Cell Right')
hsrin.all.nolptc=subset(hsrin.all, !pre_skid%in%lptcin[,'skid'])
spheres3d(subset(hsrin.all.nolptc, post_skid==4058824)[,c("post_node_x","post_node_y","post_node_z")], col='grey', , radius=500)

clear3d()
plot_hs_inputs <- function(cell=c("HSN", "HSE", "HSS")) {
  hsin=subset(hsrin, hscell%in%cell)
  xyz=c("post_node_x","post_node_y","post_node_z")

  spheres3d(subset(hsin[,xyz], hsin[,'groupname']=="bilat"), radius=500)
  spheres3d(subset(hsin[,xyz], grepl("uni", hsin[,'groupname'])), col='blue', radius=500)
  spheres3d(subset(hsin[,xyz], hsin[,'groupname']%in%c("ring", "funny", "ring incomplete")), col='cyan', radius=500)
  hscellnames <- c(`827034` = "HSE", `830793` = "HSN", `4058824` = "HSS")
  sel=hscellnames[hscellnames%in%cell]
  spheres3d(subset(hsrin.all.nolptc, post_skid%in%names(sel))[,xyz], col='grey', radius=500)
  plot3d(names(sel), db=hscells)
}

plot_hs_inputs("HSS")
nview3d()
clear3d()
plot_hs_inputs()
