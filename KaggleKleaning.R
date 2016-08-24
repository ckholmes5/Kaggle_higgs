library(dplyr)
library(Hmisc)
library(VIM)

train = read.csv('/Users/cholmes/Desktop/training.csv')
train[train == -999.000] = NA

#Selecting columns with missing data
selected_train = select(train, PRI_jet_num, DER_deltaeta_jet_jet, DER_mass_jet_jet, DER_prodeta_jet_jet, DER_lep_eta_centrality, PRI_jet_leading_pt, PRI_jet_leading_eta, PRI_jet_leading_phi, PRI_jet_subleading_pt, PRI_jet_subleading_eta, PRI_jet_subleading_phi,DER_mass_MMC)

missing_2 = select(train, PRI_jet_leading_pt, PRI_jet_leading_eta,PRI_jet_leading_phi)
missing_4 = select(train, DER_deltaeta_jet_jet,DER_mass_jet_jet,DER_prodeta_jet_jet,DER_lep_eta_centrality,PRI_jet_subleading_pt,PRI_jet_subleading_eta,PRI_jet_subleading_phi)

#Examining data broken out by PRI_jet_num
summarise(group_by(train, PRI_jet_num), sum(is.na(PRI_jet_leading_pt))) #missing2 group
summarise(group_by(train, PRI_jet_num), sum(is.na(PRI_jet_leading_pt)-1)) #missing2 group
summarise(group_by(train, PRI_jet_num), sum(is.na(DER_deltaeta_jet_jet))) #missing4 group
summarise(group_by(train, PRI_jet_num), sum(is.na(DER_deltaeta_jet_jet)-1)) #missing4 group
summarise(group_by(train, PRI_jet_num), mean(PRI_jet_leading_pt), mean(PRI_jet_leading_eta), mean(PRI_jet_leading_phi)) #missing2 group
summarise(group_by(train, PRI_jet_num), mean(DER_deltaeta_jet_jet), mean(DER_mass_jet_jet), mean(DER_prodeta_jet_jet),mean(DER_lep_eta_centrality),mean(PRI_jet_subleading_pt),mean(PRI_jet_subleading_eta),mean(PRI_jet_subleading_phi)) #missing4 group


#K-Nearest Neighbors using square root of length
neighbors = sqrt(nrow(train))

#Creating seperate datasets by PRI_jet_num (0's and 1's have tons of missing values)
df_0 = filter(train, PRI_jet_num == 0)
df_1 = filter(train, PRI_jet_num == 1)
df_2 = filter(train, PRI_jet_num == 2)
df_3 = filter(train, PRI_jet_num == 3)

df_0['DER_mass_MMC'] = impute(df_0['DER_mass_MMC'], "random")
df_1['DER_mass_MMC'] = impute(df_1['DER_mass_MMC'], "random")
df_2['DER_mass_MMC'] = impute(df_2['DER_mass_MMC'], "random")
df_3['DER_mass_MMC'] = impute(df_3['DER_mass_MMC'], "random")

drops_0 = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi','PRI_jet_leading_pt', 'PRI_jet_leading_eta','PRI_jet_leading_phi')
drops_1 = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi')

df_0 = df_0[ , !(names(df_0) %in% drops_0)]
df_1 = df_1[ , !(names(df_1) %in% drops_1)]


##df_0,df_1,df_2,df_3 are the dataframes we want to apply algorithms to



#Experimental Imputation Method
df_impute = train

grouped_2_cols = c('PRI_jet_leading_pt', 'PRI_jet_leading_eta','PRI_jet_leading_phi')
grouped_4_cols = c('DER_deltaeta_jet_jet','DER_mass_jet_jet','DER_prodeta_jet_jet','DER_lep_eta_centrality','PRI_jet_subleading_pt','PRI_jet_subleading_eta','PRI_jet_subleading_phi')

for (i in 2:ncol(grouped_2)) {
  change = as.numeric(((grouped_2[4,i] - grouped_2[3,i]) + (grouped_2[3,i] - grouped_2[2,i]))/2)
  change_jet0 = grouped_2[2,i] - change
  df_impute[grouped_2_cols[i-1]][is.na(df_impute[grouped_2_cols[i-1]])] = change_jet0
}

impute_0 = filter(df_impute, PRI_jet_num == 0)
impute_1 = filter(df_impute, PRI_jet_num == 1)
impute_23 = filter(df_impute, PRI_jet_num >= 2)

for (i in 2:ncol(grouped_4)) {
  change = as.numeric(grouped_4[4,i] - grouped_4[3,i])
  change_jet1 = grouped_4[3,i] - change
  change_jet0 = change_jet1 - change
  
  impute_0[grouped_4_cols[i-1]][is.na(impute_0[grouped_4_cols[i-1]])] = change_jet0
  impute_1[grouped_4_cols[i-1]][is.na(impute_1[grouped_4_cols[i-1]])] = change_jet1
  
  
}
exp_impute_train = rbind(impute_0, impute_1, impute_23)








