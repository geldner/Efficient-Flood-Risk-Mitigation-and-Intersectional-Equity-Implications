
library("data.table")

data_dir<-"D:/Projects/New Orleans Equity/analytica/outputs/"
setwd(data_dir)


full_table<-fread("D:/Projects/New Orleans Equity/communication/anonymized_data_table.csv")

#When selecting upgrades based on net present value of damage, we're interested not in the residual npv of damage but the prior npv of damage

setkey(full_table, "Structure_IDs", "elev")

full_table$npv_protection_norm<-full_table$npv_struct_protection/full_table$replacement_cost
full_table[,npv_damage:= npv_struct_damage + npv_cont_damage]
full_table[,npv_damage_norm:= npv_struct_damage/replacement_cost]

full_table$ce<-full_table$npv_protection/full_table$nsc
full_table$ce_norm<- full_table$npv_protection_norm/full_table$nsc

full_table<-full_table[order(ce, decreasing = TRUE)]
full_table$ce_index<-1:nrow(full_table)
full_table<-full_table[order(ce_norm, decreasing = TRUE)]
full_table$ce_norm_index<-1:nrow(full_table)





get_strategy_elevs<-function(full_table, metric_index, budget){
  #The set of elevations returned is the first one to exceed the budget
  
  residual_budget<-budget
  budget_allocated<-0
  
  
  full_table$cum_cost<-0
  full_table$max_elev<-0
  
  residual_table<-full_table
  
  implement_table<-full_table[0]
  while(residual_budget > 0){
    #We keep adding elevation projects from most cost effective to least, while considering
    #different elevations for the same building as different projects. Once the budget is expended
    #we get rid of duplicative projects (e.g. a two-foot elevation for a building if we've also selected
    #a three-foot elevation), and iterate until we've expended the budget without duplicative projects
    setkeyv(residual_table, metric_index)
    residual_table$cum_cost<-cumsum(residual_table$nsc)
    
    
    over_ind<-min(residual_table[cum_cost > residual_budget, ..metric_index])
    
    
    implement_table<-rbind(implement_table, residual_table[get(metric_index) <= over_ind])
    
    implement_table[,max_elev:=max(elev), by = Structure_IDs]
    implement_table<-implement_table[elev == max_elev,]
    implement_table$cum_cost<-cumsum(implement_table$nsc)
    
    
    
    residual_table<-residual_table[get(metric_index) > over_ind]
    
    
    
    residual_budget<-budget - max(implement_table$cum_cost)
    
    print(residual_budget)
    
  }
  
  zero_table<-full_table[elev==0 & !(Structure_IDs %in% implement_table$Structure_IDs)]
  
  implement_table<-rbind(implement_table, zero_table)
  
  implement_table$cum_benefit<-cumsum(implement_table$npv_protection)
  
  implement_table$order<-1:nrow(implement_table)
  
  
  return(implement_table)
  
}


add_race_breakdowns<-function(dataset){
  #with a list of housing elevation investments in order of most cost effective to least,
  #we produce cumulative costs, benefits, etc by race and income group
  
  dataset$cum_cost<-cumsum(dataset$nsc)
  dataset$cum_cost_white <- cumsum(dataset$nsc*dataset$prop_white)
  dataset$cum_cost_black<- cumsum(dataset$nsc*dataset$prop_black)
  dataset$cum_cost_native<-cumsum(dataset$nsc*dataset$prop_native)
  dataset$cum_cost_asian_pacific<-cumsum(dataset$nsc*dataset$prop_asian_pacific)
  dataset$cum_cost_other_multi<-cumsum(dataset$nsc*dataset$prop_other_multi)
  
  dataset$cum_cost_pir_under_pt_5<-cumsum(dataset$nsc*dataset$prop_pir_under_pt_5)
  dataset$cum_cost_pir_pt_5_to_1<-cumsum(dataset$nsc*dataset$prop_pir_pt_5_to_1)
  dataset$cum_cost_pir_1_to_2<-cumsum(dataset$nsc*dataset$prop_pir_1_to_2)
  dataset$cum_cost_pir_2_plus<-cumsum(dataset$nsc*dataset$prop_pir_2_plus)
  
  
  dataset$cum_benefit_white <- cumsum(dataset$npv_protection*dataset$prop_white)
  dataset$cum_benefit_black<- cumsum(dataset$npv_protection*dataset$prop_black)
  dataset$cum_benefit_native<-cumsum(dataset$npv_protection*dataset$prop_native)
  dataset$cum_benefit_asian_pacific<-cumsum(dataset$npv_protection*dataset$prop_asian_pacific)
  dataset$cum_benefit_other_multi<-cumsum(dataset$npv_protection*dataset$prop_other_multi)
  
  dataset$cum_benefit_pir_under_pt_5<-cumsum(dataset$npv_protection*dataset$prop_pir_under_pt_5)
  dataset$cum_benefit_pir_pt_5_to_1<-cumsum(dataset$npv_protection*dataset$prop_pir_pt_5_to_1)
  dataset$cum_benefit_pir_1_to_2<-cumsum(dataset$npv_protection*dataset$prop_pir_1_to_2)
  dataset$cum_benefit_pir_2_plus<-cumsum(dataset$npv_protection*dataset$prop_pir_2_plus)
  
  dataset$cum_benefit_norm_white <- cumsum(dataset$npv_protection_norm*dataset$prop_white)
  dataset$cum_benefit_norm_black<- cumsum(dataset$npv_protection_norm*dataset$prop_black)
  dataset$cum_benefit_norm_native<-cumsum(dataset$npv_protection_norm*dataset$prop_native)
  dataset$cum_benefit_norm_asian_pacific<-cumsum(dataset$npv_protection_norm*dataset$prop_asian_pacific)
  dataset$cum_benefit_norm_other_multi<-cumsum(dataset$npv_protection_norm*dataset$prop_other_multi)
  
  dataset$cum_benefit_norm_pir_under_pt_5<-cumsum(dataset$npv_protection_norm*dataset$prop_pir_under_pt_5)
  dataset$cum_benefit_norm_pir_pt_5_to_1<-cumsum(dataset$npv_protection_norm*dataset$prop_pir_pt_5_to_1)
  dataset$cum_benefit_norm_pir_1_to_2<-cumsum(dataset$npv_protection_norm*dataset$prop_pir_1_to_2)
  dataset$cum_benefit_norm_pir_2_plus<-cumsum(dataset$npv_protection_norm*dataset$prop_pir_2_plus)
  
  return(dataset)
  
}

bar_chart_breakdown_strategy<-function(dataset, zero_bar){
  #top-line cumulative estimates broken down by race group
  
  costs<-data.table(matrix(nrow = 5, ncol = 1))
  names(costs)<-c("value")
  races<-c("White", "Black", "Native American", "Asian / Pacific Islander", "Other / Multi")
  
  cum_cost_white <- sum(dataset$nsc*dataset$prop_white)
  cum_cost_black<- sum(dataset$nsc*dataset$prop_black)
  cum_cost_native<-sum(dataset$nsc*dataset$prop_native)
  cum_cost_asian_pacific<-sum(dataset$nsc*dataset$prop_asian_pacific)
  cum_cost_other_multi<-sum(dataset$nsc*dataset$prop_other_multi)
  
  costs$race<-races
  costs$measure<-"cost"
  costs$value<-c(cum_cost_white, cum_cost_black, cum_cost_native, cum_cost_asian_pacific, cum_cost_other_multi)
  
  household_costs<-data.table(matrix(nrow = 5, ncol = 1))
  names(household_costs)<-c("value")
  household_costs$race<-races
  household_costs$measure<-"household cost"
  household_costs$value<-c(cum_cost_white, cum_cost_black, cum_cost_native, cum_cost_asian_pacific, cum_cost_other_multi)/zero_bar[measure=="households",value]
  
  
  cum_benefit_white <- sum(dataset$npv_protection*dataset$prop_white)
  cum_benefit_black<- sum(dataset$npv_protection*dataset$prop_black)
  cum_benefit_native<-sum(dataset$npv_protection*dataset$prop_native)
  cum_benefit_asian_pacific<-sum(dataset$npv_protection*dataset$prop_asian_pacific)
  cum_benefit_other_multi<-sum(dataset$npv_protection*dataset$prop_other_multi)
  
  benefit_value<-data.table(matrix(nrow = 5, ncol = 1))
  names(benefit_value)<-c("value")
  benefit_value$race<-races
  benefit_value$measure<-"risk: value"
  benefit_value$value<-zero_bar[measure == "risk: value",value] -
    c(cum_benefit_white,cum_benefit_black,cum_benefit_native, cum_benefit_asian_pacific,cum_benefit_other_multi )
  
  benefit_value_frac<-data.table(matrix(nrow = 5, ncol = 1))
  names(benefit_value_frac)<-c("value")
  benefit_value_frac$race<-races
  benefit_value_frac$measure<-"DED reduction frac"
  benefit_value_frac$value<- c(cum_benefit_white,cum_benefit_black,cum_benefit_native,
                               cum_benefit_asian_pacific,cum_benefit_other_multi )/ sum(zero_bar[measure=="risk: value", value])
  
  household_risk_value<-data.table(matrix(nrow = 5, ncol = 1))
  names(household_risk_value)<-c("value")
  household_risk_value$race<-races
  household_risk_value$measure<-"household risk: value"
  household_risk_value$value<-benefit_value$value/zero_bar[measure=="households",value]
  
  
  cum_benefit_norm_white <- sum(dataset$npv_protection_norm*dataset$prop_white)
  cum_benefit_norm_black<- sum(dataset$npv_protection_norm*dataset$prop_black)
  cum_benefit_norm_native<-sum(dataset$npv_protection_norm*dataset$prop_native)
  cum_benefit_norm_asian_pacific<-sum(dataset$npv_protection_norm*dataset$prop_asian_pacific)
  cum_benefit_norm_other_multi<-sum(dataset$npv_protection_norm*dataset$prop_other_multi)
  
  benefit_structure<-data.table(matrix(nrow = 5, ncol = 1))
  names(benefit_structure)<-c("value")
  benefit_structure$race<-races
  benefit_structure$measure<-"risk: structure"
  benefit_structure$value<-zero_bar[measure == "risk: structure",value] -
    c(cum_benefit_norm_white,cum_benefit_norm_black,cum_benefit_norm_native,
      cum_benefit_norm_asian_pacific,cum_benefit_norm_other_multi )
  
  benefit_structure_frac<-data.table(matrix(nrow = 5, ncol = 1))
  names(benefit_structure_frac)<-c("value")
  benefit_structure_frac$race<-races
  benefit_structure_frac$measure<-"RLE reduction frac"
  benefit_structure_frac$value<- c(cum_benefit_norm_white,cum_benefit_norm_black,cum_benefit_norm_native,
                                   cum_benefit_norm_asian_pacific,cum_benefit_norm_other_multi )/ sum(zero_bar[measure=="risk: structure", value])
  
  household_risk_structure<-data.table(matrix(nrow = 5, ncol = 1))
  names(household_risk_structure)<-c("value")
  household_risk_structure$race<-races
  household_risk_structure$measure<-"household risk: structure"
  household_risk_structure$value<-benefit_structure$value/zero_bar[measure=="households",value]
  
  
  
  
  return(rbind(costs, benefit_value, household_risk_value, benefit_structure,benefit_value_frac, benefit_structure_frac, household_risk_structure))
}

bar_chart_breakdown_strategy_pov<-function(dataset, zero_bar){
  #top-line cumulative estimates broken down by poverty group
  
  
  costs<-data.table(matrix(nrow = 4, ncol = 1))
  names(costs)<-c("value")
  pir_bins<-c("PIR under 0.5", "PIR under 1.0", " PIR under 2.0", "PIR 2+")
  
  cum_cost_pir_under_pt_5 <- sum(dataset$nsc*dataset$prop_pir_under_pt_5)
  cum_cost_pir_under_1<- sum(dataset$nsc*dataset$prop_pir_pt_5_to_1)
  cum_cost_pir_under_2<-sum(dataset$nsc*dataset$prop_pir_1_to_2)
  cum_cost_pir_2_plus<-sum(dataset$nsc*dataset$prop_pir_2_plus)
  
  costs$pir_bin<-pir_bins
  costs$measure<-"cost"
  costs$value<-c(cum_cost_pir_under_pt_5, cum_cost_pir_under_1, cum_cost_pir_under_2, cum_cost_pir_2_plus)
  
  household_costs<-data.table(matrix(nrow = 4, ncol = 1))
  names(household_costs)<-c("value")
  
  household_costs$pir_bin<-pir_bins
  household_costs$measure<-"household cost"
  household_costs$value<-c(cum_cost_pir_under_pt_5, cum_cost_pir_under_1, cum_cost_pir_under_2, cum_cost_pir_2_plus)/zero_bar[measure=="households",value]
  
  
  cum_benefit_pir_under_pt_5 <- sum(dataset$npv_protection*dataset$prop_pir_under_pt_5)
  cum_benefit_pir_under_1<- sum(dataset$npv_protection*dataset$prop_pir_pt_5_to_1)
  cum_benefit_pir_under_2<-sum(dataset$npv_protection*dataset$prop_pir_1_to_2)
  cum_benefit_pir_2_plus<-sum(dataset$npv_protection*dataset$prop_pir_2_plus)
  
  benefit_value<-data.table(matrix(nrow = 4, ncol = 1))
  names(benefit_value)<-c("value")
  benefit_value$pir_bin<-pir_bins
  benefit_value$measure<-"risk: value"
  benefit_value$value<-zero_bar[measure == "risk: value",value] -
    c(cum_benefit_pir_under_pt_5,cum_benefit_pir_under_1,cum_benefit_pir_under_2, cum_benefit_pir_2_plus)
  
  benefit_value_frac<-data.table(matrix(nrow = 4, ncol = 1))
  names(benefit_value_frac)<-c("value")
  benefit_value_frac$pir_bin<-pir_bins
  benefit_value_frac$measure<-"DED reduction frac"
  benefit_value_frac$value<-  c(cum_benefit_pir_under_pt_5,cum_benefit_pir_under_1,cum_benefit_pir_under_2,
                                cum_benefit_pir_2_plus)/ sum(zero_bar[measure=="risk: value", value])
  
  household_risk_value<-data.table(matrix(nrow = 4, ncol = 1))
  names(household_risk_value)<-c("value")
  household_risk_value$pir_bin<-pir_bins
  household_risk_value$measure<-"household risk: value"
  household_risk_value$value<-benefit_value$value/zero_bar[measure=="households",value]
  
  
  cum_benefit_norm_pir_under_pt_5 <- sum(dataset$npv_protection_norm*dataset$prop_pir_under_pt_5)
  cum_benefit_norm_pir_under_1<- sum(dataset$npv_protection_norm*dataset$prop_pir_pt_5_to_1)
  cum_benefit_norm_pir_under_2<-sum(dataset$npv_protection_norm*dataset$prop_pir_1_to_2)
  cum_benefit_norm_pir_2_plus<-sum(dataset$npv_protection_norm*dataset$prop_pir_2_plus)
  
  benefit_structure<-data.table(matrix(nrow = 4, ncol = 1))
  names(benefit_structure)<-c("value")
  benefit_structure$pir_bin<-pir_bins
  benefit_structure$measure<-"risk: structure"
  benefit_structure$value<-zero_bar[measure == "risk: structure",value] -
    c(cum_benefit_norm_pir_under_pt_5,cum_benefit_norm_pir_under_1,cum_benefit_norm_pir_under_2,
      cum_benefit_norm_pir_2_plus )
  
  benefit_structure_frac<-data.table(matrix(nrow = 4, ncol = 1))
  names(benefit_structure_frac)<-c("value")
  benefit_structure_frac$pir_bin<-pir_bins
  benefit_structure_frac$measure<-"RLE reduction frac"
  benefit_structure_frac$value<- c(cum_benefit_norm_pir_under_pt_5,cum_benefit_norm_pir_under_1,
                                   cum_benefit_norm_pir_under_2,cum_benefit_norm_pir_2_plus )/ sum(zero_bar[measure=="risk: structure", value])
  
  household_risk_structure<-data.table(matrix(nrow = 4, ncol = 1))
  names(household_risk_structure)<-c("value")
  household_risk_structure$pir_bin<-pir_bins
  household_risk_structure$measure<-"household risk: structure"
  household_risk_structure$value<-benefit_structure$value/zero_bar[measure=="households",value]
  
  
  
  
  return(rbind(costs, household_costs, benefit_value, household_risk_value, benefit_structure,benefit_value_frac, benefit_structure_frac, household_risk_structure))
}

bar_chart_breakdown_fwoa<-function(dataset){
  
  dataset$npv_damage_norm<-dataset$npv_damage/dataset$replacement_cost
  
  pop<-data.table(matrix(nrow = 5, ncol = 1))
  names(pop)<-c("value")
  races<-c("White", "Black", "Native American", "Asian / Pacific Islander", "Other / Multi")
  
  pop_white <- sum(dataset$structure_pop*dataset$prop_white)
  pop_black<- sum(dataset$structure_pop*dataset$prop_black)
  pop_native<-sum(dataset$structure_pop*dataset$prop_native)
  pop_asian_pacific<-sum(dataset$structure_pop*dataset$prop_asian_pacific)
  pop_other_multi<-sum(dataset$structure_pop*dataset$prop_other_multi)
  
  pop$race<-races
  pop$measure<-"pop"
  pop$value<-c(pop_white, pop_black, pop_native, pop_asian_pacific, pop_other_multi)
  
  households<-data.table(matrix(nrow = 5, ncol = 1))
  names(households)<-c("value")
  
  households_white <- sum(dataset$prop_white)
  households_black<- sum(dataset$prop_black)
  households_native<-sum(dataset$prop_native)
  households_asian_pacific<-sum(dataset$prop_asian_pacific)
  households_other_multi<-sum(dataset$prop_other_multi)
  
  households$race<-races
  households$measure<-"households"
  households$value<-c(households_white, households_black, households_native, households_asian_pacific, households_other_multi)
  
  risk_value<-data.table(matrix(nrow = 5, ncol = 1))
  names(risk_value)<-c("value")
  
  risk_value_white <- sum(dataset$prop_white*dataset$npv_damage)
  risk_value_black<- sum(dataset$prop_black*dataset$npv_damage)
  risk_value_native<-sum(dataset$prop_native*dataset$npv_damage)
  risk_value_asian_pacific<-sum(dataset$prop_asian_pacific*dataset$npv_damage)
  risk_value_other_multi<-sum(dataset$prop_other_multi*dataset$npv_damage)
  
  risk_value$race<-races
  risk_value$measure<-"risk: value"
  risk_value$value<-c(risk_value_white, risk_value_black, risk_value_native, risk_value_asian_pacific, risk_value_other_multi)
  
  risk_structure<-data.table(matrix(nrow = 5, ncol = 1))
  names(risk_structure)<-c( "value")
  
  risk_structure_white <- sum(dataset$prop_white*dataset$npv_damage_norm)
  risk_structure_black<- sum(dataset$prop_black*dataset$npv_damage_norm)
  risk_structure_native<-sum(dataset$prop_native*dataset$npv_damage_norm)
  risk_structure_asian_pacific<-sum(dataset$prop_asian_pacific*dataset$npv_damage_norm)
  risk_structure_other_multi<-sum(dataset$prop_other_multi*dataset$npv_damage_norm)
  
  risk_structure$race<-races
  risk_structure$measure<-"risk: structure"
  risk_structure$value<-c(risk_structure_white, risk_structure_black, risk_structure_native, risk_structure_asian_pacific, risk_structure_other_multi)
  
  household_risk_value<-data.table(matrix(nrow = 5, ncol = 1))
  names(household_risk_value)<-c("value")
  household_risk_value$race<-races
  household_risk_value$measure<- "household risk: value"
  household_risk_value$value<- risk_value$value/households$value
  
  household_risk_structure<-data.table(matrix(nrow = 5, ncol = 1))
  names(household_risk_structure)<-c("value")
  household_risk_structure$race<-races
  household_risk_structure$measure<- "household risk: structure"
  household_risk_structure$value<- risk_structure$value/households$value
  
  
  
  
  
  return(rbind(pop, households, risk_value, household_risk_value, risk_structure, household_risk_structure))
}


bar_chart_breakdown_fwoa_pov<-function(dataset){
  
  dataset$npv_damage_norm<-dataset$npv_damage/dataset$replacement_cost
  
  pop<-data.table(matrix(nrow = 4, ncol = 1))
  names(pop)<-c("value")
  pir_bins<-c("PIR under 0.5", "PIR under 1.0", " PIR under 2.0", "PIR 2+")
  
  pop_pir_under_pt_5 <- sum(dataset$structure_pop*dataset$prop_pir_under_pt_5)
  pop_pir_under_1<- sum(dataset$structure_pop*dataset$prop_pir_pt_5_to_1)
  pop_pir_under_2<-sum(dataset$structure_pop*dataset$prop_pir_1_to_2)
  pop_pir_2_plus<-sum(dataset$structure_pop*dataset$prop_pir_2_plus)
  
  
  pop$pir_bin<-pir_bins
  pop$measure<-"pop"
  pop$value<-c(pop_pir_under_pt_5, pop_pir_under_1, pop_pir_under_2, pop_pir_2_plus)
  
  households<-data.table(matrix(nrow = 4, ncol = 1))
  names(households)<-c("value")
  
  households_pir_under_pt_5 <- sum(dataset$prop_pir_under_pt_5)
  households_pir_under_1<- sum(dataset$prop_pir_pt_5_to_1)
  households_pir_under_2<-sum(dataset$prop_pir_1_to_2)
  households_pir_2_plus<-sum(dataset$prop_pir_2_plus)
  
  
  households$pir_bin<-pir_bins
  households$measure<-"households"
  households$value<-c(households_pir_under_pt_5, households_pir_under_1, households_pir_under_2, households_pir_2_plus)
  
  risk_value<-data.table(matrix(nrow = 4, ncol = 1))
  names(risk_value)<-c("value")
  
  risk_value_pir_under_pt_5 <- sum(dataset$prop_pir_under_pt_5*dataset$npv_damage)
  risk_value_pir_under_1<- sum(dataset$prop_pir_pt_5_to_1*dataset$npv_damage)
  risk_value_pir_under_2<-sum(dataset$prop_pir_1_to_2*dataset$npv_damage)
  risk_value_pir_2_plus<-sum(dataset$prop_pir_2_plus*dataset$npv_damage)
  
  
  risk_value$pir_bin<-pir_bins
  risk_value$measure<-"risk: value"
  risk_value$value<-c(risk_value_pir_under_pt_5, risk_value_pir_under_1, risk_value_pir_under_2, risk_value_pir_2_plus)
  
  risk_structure<-data.table(matrix(nrow = 4, ncol = 1))
  names(risk_structure)<-c( "value")
  
  risk_structure_pir_under_pt_5 <- sum(dataset$prop_pir_under_pt_5*dataset$npv_damage_norm)
  risk_structure_pir_under_1<- sum(dataset$prop_pir_pt_5_to_1*dataset$npv_damage_norm)
  risk_structure_pir_under_2<-sum(dataset$prop_pir_1_to_2*dataset$npv_damage_norm)
  risk_structure_pir_2_plus<-sum(dataset$prop_pir_2_plus*dataset$npv_damage_norm)
  
  risk_structure$pir_bin<-pir_bins
  risk_structure$measure<-"risk: structure"
  risk_structure$value<-c(risk_structure_pir_under_pt_5, risk_structure_pir_under_1, risk_structure_pir_under_2, risk_structure_pir_2_plus)
  
  household_risk_value<-data.table(matrix(nrow = 4, ncol = 1))
  names(household_risk_value)<-c("value")
  household_risk_value$pir_bin<-pir_bins
  household_risk_value$measure<- "household risk: value"
  household_risk_value$value<- risk_value$value/households$value
  
  household_risk_structure<-data.table(matrix(nrow = 4, ncol = 1))
  names(household_risk_structure)<-c("value")
  household_risk_structure$pir_bin<-pir_bins
  household_risk_structure$measure<- "household risk: structure"
  household_risk_structure$value<- risk_structure$value/households$value
  
  
  return(rbind(pop, households, risk_value, household_risk_value, risk_structure, household_risk_structure))
}

med_structure_cost<-median(full_table$replacement_cost)

scalarized_list<-list()
for(structure_weight in 0:100/100){
  full_table<-full_table[order(ce_norm*structure_weight*med_structure_cost + ce*(1-structure_weight), decreasing = TRUE)]
  full_table$working_index<-1:nrow(full_table)
  
  
  scalarized_list[[paste0(structure_weight)]]<-get_strategy_elevs(full_table, "working_index", 100000000)
  
}

zero_table<-full_table[elev==0]
zero_table$strategy<-"no action"




zero_table<-add_race_breakdowns(zero_table)
ce_table_100mil<-get_strategy_elevs(full_table, "ce_index", 100000000)
ce_table_100mil$strategy<-"cost effectiveness"
ce_table_100mil<-add_race_breakdowns(ce_table_100mil)
ce_norm_table_100mil<-get_strategy_elevs(full_table, "ce_norm_index", 100000000)
ce_norm_table_100mil$strategy<-"normalized cost effectiveness"
ce_norm_table_100mil<-add_race_breakdowns(ce_norm_table_100mil)








zero_bar<-bar_chart_breakdown_fwoa(zero_table)
zero_bar$strategy<-"no action"
zero_bar_pov<-bar_chart_breakdown_fwoa_pov(zero_table)
zero_bar_pov$strategy<-"no action"

value_bar<-bar_chart_breakdown_strategy(ce_table_100mil, zero_bar)
value_bar$strategy<- "value"
value_bar_pov<-bar_chart_breakdown_strategy_pov(ce_table_100mil, zero_bar_pov)
value_bar_pov$strategy<- "value"

structure_bar<-bar_chart_breakdown_strategy(ce_norm_table_100mil, zero_bar)
structure_bar$strategy<-"structure"
structure_bar_pov<-bar_chart_breakdown_strategy_pov(ce_norm_table_100mil, zero_bar_pov)
structure_bar_pov$strategy<-"structure"

interp_bar<-data.table()
interp_bar_pov<-data.table()

for(structure_weight in 0:100/100){
  this_bar_table<-bar_chart_breakdown_strategy(scalarized_list[[paste0(structure_weight)]],zero_bar)
  this_bar_table$strategy<-"interp"
  this_bar_table$structure_weight<-structure_weight
  interp_bar<-rbind(interp_bar, this_bar_table)
  
  this_bar_table_pov<-bar_chart_breakdown_strategy_pov(scalarized_list[[paste0(structure_weight)]],zero_bar_pov)
  this_bar_table_pov$strategy<-"interp"
  this_bar_table_pov$structure_weight<-structure_weight
  interp_bar_pov<-rbind(interp_bar_pov, this_bar_table_pov)
}



#write.csv(rbind(zero_bar, value_bar, structure_bar, interp_bar, fill = TRUE), "bar_chart_output.csv", row.names = FALSE)

#write.csv(rbind(zero_bar_pov, value_bar_pov, structure_bar_pov, interp_bar_pov, fill = TRUE), "bar_chart_output_pov.csv", row.names = FALSE)

zero_bar$breakdown <- "race"
structure_bar$breakdown <- "race"
value_bar$breakdown<- "race"
interp_bar$breakdown<- "race"
zero_bar_pov$breakdown<-"pov"
value_bar_pov$breakdown<-"pov"
structure_bar_pov$breakdown<-"pov"
interp_bar_pov$breakdown<-"pov"

#produces the data shown in Figure 1 and Figure 4

write.csv(rbind(zero_bar, value_bar, structure_bar, interp_bar, zero_bar_pov, value_bar_pov, structure_bar_pov, interp_bar_pov,
                fill = TRUE), "bar_chart_output_anonymized.csv", row.names = FALSE)

race_households<-colSums(zero_table[,c("prop_white", "prop_black", "prop_native",
                                       "prop_asian_pacific", "prop_other_multi")])

pov_households<-colSums(zero_table[,c("prop_pir_under_pt_5", "prop_pir_pt_5_to_1", "prop_pir_1_to_2", "prop_pir_2_plus")])

race_init_risks<-colSums(zero_table[, list(white_risk = prop_white * (npv_struct_damage + npv_cont_damage),
                                           black_risk = prop_black * (npv_struct_damage + npv_cont_damage),
                                           native_risk = prop_native*(npv_struct_damage + npv_cont_damage),
                                           asian_pacific_risk = prop_asian_pacific*(npv_struct_damage + npv_cont_damage),
                                           other_multi_risk = prop_other_multi*(npv_struct_damage + npv_cont_damage))])

pov_init_risks<-colSums(zero_table[,list(pir_under_pt_5_risk = prop_pir_under_pt_5 * (npv_struct_damage + npv_cont_damage),
                                         pir_pt_5_to_1_risk = prop_pir_pt_5_to_1 * (npv_struct_damage + npv_cont_damage),
                                         pir_1_to_2_risk = prop_pir_1_to_2 * (npv_struct_damage + npv_cont_damage),
                                         pir_2_plus_risk = prop_pir_2_plus * (npv_struct_damage + npv_cont_damage))])

race_init_risks_norm<-colSums(zero_table[, list(white_risk = prop_white * npv_struct_damage/replacement_cost,
                                                black_risk = prop_black * npv_struct_damage/replacement_cost,
                                                native_risk = prop_native*npv_struct_damage/replacement_cost,
                                                asian_pacific_risk = prop_asian_pacific*npv_struct_damage/replacement_cost,
                                                other_multi_risk = prop_other_multi*npv_struct_damage/replacement_cost)])

pov_init_risks_norm<-colSums(zero_table[,list(pir_under_pt_5_risk = prop_pir_under_pt_5 * npv_struct_damage/replacement_cost,
                                              pir_pt_5_to_1_risk = prop_pir_pt_5_to_1 * npv_struct_damage/replacement_cost,
                                              pir_1_to_2_risk = prop_pir_1_to_2 * npv_struct_damage/replacement_cost,
                                              pir_2_plus_risk = prop_pir_2_plus * npv_struct_damage/replacement_cost)])

out_table_100mil<-rbind(ce_table_100mil, ce_norm_table_100mil, 
                        fill = TRUE)

#here are cumulative estimates of risk reduction by race and income group
#as investment budget increases from 0 to 100 million dollars
write.csv(out_table_100mil, "equity_table_100mil_anonymized.csv")

out_table_100mil<-rbind(ce_table_100mil, ce_norm_table_100mil, 
                        zero_table, fill = TRUE)

mapping_data<-out_table_100mil[,c("Structure_IDs", "nsc", "elev", "npv_cont_damage", "npv_protection", 
                                  "replacement_cost", "nola_nbhd_id", "prop_white", "prop_black", "prop_asian_pacific",
                                  "npv_protection_norm", "strategy", "LUP_LAB", "GNOCDC_LAB", "structure_pop",
                                  "prop_pir_under_pt_5", "prop_pir_pt_5_to_1", "npv_struct_damage")]

mapping_data$replacement_cost<-as.numeric(mapping_data$replacement_cost)

mapping_data[,npv_damage_norm:=npv_struct_damage/replacement_cost]

mapping_data[,nsc:=mean(nsc), by = c("nola_nbhd_id", "strategy")]
mapping_data[,nsc_total:=sum(nsc), by = c("nola_nbhd_id", "strategy")]
mapping_data[,elev:=round(mean(elev)), by = c("nola_nbhd_id", "strategy")]
mapping_data[,npv_damage:=mean(npv_struct_damage+npv_cont_damage), by = c("nola_nbhd_id", "strategy")]
mapping_data[,npv_protection:=mean(npv_protection), by = c("nola_nbhd_id", "strategy")]
mapping_data[,econ_damage_reduction_frac:= npv_protection/(npv_damage + npv_protection), by = c("nola_nbhd_id", "strategy")]
mapping_data[,replacement_cost:=mean(replacement_cost), by = c("nola_nbhd_id", "strategy")]
mapping_data[,npv_protection_norm:=mean(npv_protection_norm), by = c("nola_nbhd_id", "strategy")]
mapping_data[,npv_damage_norm:=mean(npv_damage_norm), by = c("nola_nbhd_id", "strategy")]
mapping_data[,residence_losses_equiv_frac:= npv_protection_norm/(npv_protection_norm + npv_damage_norm), by = c("nola_nbhd_id", "strategy")]
mapping_data[,prop_black:= sum(structure_pop*prop_black)/sum(structure_pop), by = c("nola_nbhd_id", "strategy")]
mapping_data[,prop_white:= sum(structure_pop*prop_white)/sum(structure_pop), by = c("nola_nbhd_id", "strategy")]
mapping_data[,prop_white:= sum(structure_pop*prop_white)/sum(structure_pop), by = c("nola_nbhd_id", "strategy")]
mapping_data[,prop_asian_pacific:= sum(structure_pop*prop_asian_pacific)/sum(structure_pop), by = c("nola_nbhd_id", "strategy")]
mapping_data[,poverty_rate:= sum(structure_pop*(prop_pir_under_pt_5 + prop_pir_pt_5_to_1))/sum(structure_pop), by = c("nola_nbhd_id", "strategy")]

#this is the data shown in figures 2, 3
write.csv(mapping_data, "mapping_table_anonymized.csv")





