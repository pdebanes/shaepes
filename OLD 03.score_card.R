############################################################################
############################################################################
###                                                                      ###
###                              SCORE CARD                              ###
###                                                                      ###
############################################################################
############################################################################



# compute score by dimension ----------------------------------------------
table(total_reset_q2$w_indic)
total_score_dim<-total_reset_q2 |> mutate(w_indic=as.numeric(w_indic))|> group_by(territoire, trimestre, dimension) |> reframe(score_dim=sum(w_indic))


# weighting of program RESET ----------------------------------------------
table(total_score_dim$dimension)
dim_weight_reset <- data.frame(
  dim_name = c("Security", 
               "Humanitarian", 
               "Access", 
               "Political and social inclusion", 
               "Environmental Hazards", 
               "Economic"),
  dim_weight = c(0.25, 0.25, 0.15, 0.1, 0.1, 0.15)
)

total_score_dim<-merge(total_score_dim, dim_weight_reset, by.x="dimension", by.y="dim_name", all.x=T)


total_score_dim<-total_score_dim |> mutate(tot=score_dim*dim_weight)

total_shaepes<-total_score_dim |> 
  group_by(territoire, trimestre) |> reframe(score_tot=round(sum(tot), 2))

write.xlsx(total_shaepes, "total_shaepes.xlsx")

write.xlsx(total_score_dim, "shaepes_score_dim.xlsx")
