library(dplyr)
library(Hmisc)
library(VIM)

train = read.csv('/Users/cholmes/Desktop/training.csv')
train[train == -999.000] = NA
test_train = train
mean_train = train[,-33]
rand_train = train[,-33]

selected_train = select(train, PRI_jet_num, DER_deltaeta_jet_jet, DER_mass_jet_jet, DER_prodeta_jet_jet, DER_lep_eta_centrality, PRI_jet_leading_pt, PRI_jet_leading_eta, PRI_jet_leading_phi, PRI_jet_subleading_pt, PRI_jet_subleading_eta, PRI_jet_subleading_phi)



missing_2 = select(train, PRI_jet_leading_pt, PRI_jet_leading_eta,PRI_jet_leading_phi)
missing_4 = select(train, DER_deltaeta_jet_jet,DER_mass_jet_jet,DER_prodeta_jet_jet,DER_lep_eta_centrality,PRI_jet_subleading_pt,PRI_jet_subleading_eta,PRI_jet_subleading_phi)
is.na()
summarise(group_by(train, PRI_jet_num), sum(is.na(PRI_jet_leading_pt))) #missing2 group
summarise(group_by(train, PRI_jet_num), sum(is.na(PRI_jet_leading_pt)-1)) #missing2 group
summarise(group_by(train, PRI_jet_num), sum(is.na(DER_deltaeta_jet_jet))) #missing4 group
summarise(group_by(train, PRI_jet_num), sum(is.na(DER_deltaeta_jet_jet)-1)) #missing4 group


summarise(group_by(train, PRI_jet_num), mean(PRI_jet_leading_pt), mean(PRI_jet_leading_eta), mean(PRI_jet_leading_phi)) #missing2 group
summarise(group_by(train, PRI_jet_num), mean(DER_deltaeta_jet_jet), mean(DER_mass_jet_jet), mean(DER_prodeta_jet_jet),mean(DER_lep_eta_centrality),mean(PRI_jet_subleading_pt),mean(PRI_jet_subleading_eta),mean(PRI_jet_subleading_phi)) #missing4 group


#Mean Value Imputation
for (name in colnames(mean_train)) {
  print (name)
  print (mean(test_train[name][1:250000,], na.rm=TRUE))
  mean_train[name][is.na(mean_train[name])] = mean(mean_train[name][1:250000,], na.rm=TRUE)
}

#Simple Random Imputation
set.seed(0)
for (name in colnames(mean_train)) {
  print (name)
  rand_train[name] = impute(rand_train[name], "random") #Simple random imputation using the impute() function from the Hmisc package.
}


#K-Nearest Neighbors using square root of length
neighbors = sqrt(nrow(train))

for (name in colnames(test_train)) {
  print (name)
  test_train = kNN(test_train, k = neighbors)
}

df_0 = filter(train, PRI_jet_num == 0)
df_1 = filter(train, PRI_jet_num == 1)
df_2 = filter(train, PRI_jet_num == 2)
df_3 = filter(train, PRI_jet_num == 3)

df_set = c(df_0$DER_mass_MMC, df_1$DER_mass_MMC, df_2$DER_mass_MMC, df_3$DER_mass_MMC)
for (set in df_set) {
  df_0['DER_mass_MMC'] = impute(set, "random")
}









