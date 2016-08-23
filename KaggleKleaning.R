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









