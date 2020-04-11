#Servidores

servidores_ativos <- read.csv2("DesafioSeducSP/BASE_SERVIDORES_ATIVOS_1118.csv")
servidores_ativos["Exp_Total"] <- difftime(as.Date("2018-12-31"), as.Date(servidores_ativos$DT_INICIO_EXERCICIO_C,format = "%d/%m/%Y"), units = "days")
servidores_ativos["Exp_Cargo"] <- difftime(as.Date("2018-12-31"), as.Date(servidores_ativos$DATA_INICIO_EXERCICIO_E,format = "%d/%m/%Y"), units = "days")
servidores_ativos["Jornada"] <- 40*(servidores_ativos$JORNADA=="C")+30*(servidores_ativos$JORNADA=="B") + 24*(servidores_ativos$JORNADA=="I") + 12*(servidores_ativos$JORNADA=="R")
servidores_ativos["Cargo"] <-  servidores_ativos$NOMECAR_E
servidores_ativos <- servidores_ativos[,c("id_interno","Exp_Total","Exp_Cargo","Jornada", "Cargo")]

formacao <- read.csv2("DesafioSeducSP/BASE_FORMACAO_1118.csv")
formacao["Formacao"] <- -1*grepl("ENSINO MÌäDIO", formacao$FORMACAO, fixed=TRUE) + grepl("ESPECIALIZAÌàÌÄO", formacao$FORMACAO, fixed=TRUE)+grepl("MESTRADO", formacao$FORMACAO, fixed=TRUE)+grepl("DOUTORADO", formacao$FORMACAO, fixed=TRUE)
formacao <- formacao[,c("id_interno","Formacao")]

ausencias <- read.csv2("DesafioSeducSP/BASE_AUSENCIAS_1118.csv")
ausencias["FaltaJust"] <-  ausencias$TT_DIAS_FALTA_JUST
ausencias["FaltaNaoJust"] <-  ausencias$TT_DIAS_FALTA_INJUST
ausencias["FaltaDireito"] <-  ausencias$TT_DIAS_FALTA_MEDICA + ausencias$TT_DIAS_LIC_PREMIO +  ausencias$TT_DIAS_LIC_GESTANTE +  ausencias$TT_DIAS_LIC_ACID_TRAB +  ausencias$TT_DIAS_LIC_INTER_PARTIC
ausencias <- ausencias[,c("id_interno", "FaltaJust", "FaltaNaoJust", "FaltaDireito")]

servidores = merge(ausencias, formacao, all=TRUE)
servidores = merge(servidores, servidores_ativos, all=TRUE)

#Gestores
diretor = read.csv2("DesafioSeducSP/DIRETORES DE ESCOLA.csv")
diretor <- diretor[diretor$X2018=="SIM",]
diretor <- diretor[,c("CD_ESCOLA", "id_interno")]
diretor <- merge(diretor, servidores, x.all=TRUE)
outra_atividade <- diretor[diretor$Cargo != "DIRETOR DE ESCOLA",]
outra_atividade <- aggregate(outra_atividade$Exp_Total, list(outra_atividade$CD_ESCOLA), mean)
names(outra_atividade)[names(outra_atividade) == "Group.1"] <- "CD_ESCOLA"
names(outra_atividade)[names(outra_atividade) == "x"] <- "Dir_Exp_Mag"
diretor <- diretor[diretor$Cargo == "DIRETOR DE ESCOLA",]
diretor <- aggregate(x = diretor[c("Formacao", "Exp_Cargo")], by = diretor[c("CD_ESCOLA")], FUN = mean)
diretor =  merge(diretor, outra_atividade, all=TRUE)
names(diretor)[names(diretor) == "Formacao"] <- "Dir_Formacao"
names(diretor)[names(diretor) == "Exp_Cargo"] <- "Dir_Exp_Cargo"

vdiretor = read.csv2("DesafioSeducSP/VICE_DIRETOR.csv")
vdiretor <- vdiretor[vdiretor$X2018=="SIM",]
vdiretor <- vdiretor[,c("CD_ESCOLA", "id_interno")]
vdiretor <- merge(vdiretor, servidores, x.all = TRUE)
#outra_atividade <- vdiretor[vdiretor$Cargo != "DIRETOR DE ESCOLA",]
#outra_atividade <- aggregate(outra_atividade$Exp_Total, list(outra_atividade$CD_ESCOLA), mean)
#names(outra_atividade)[names(outra_atividade) == "Group.1"] <- "CD_ESCOLA"
#names(outra_atividade)[names(outra_atividade) == "x"] <- "VDir_Exp_Mag"
#vdiretor <- vdiretor[vdiretor$Cargo == "DIRETOR DE ESCOLA",]
#vdiretor <- aggregate(x = vdiretor[c("Formacao", "Exp_Total", "Exp_Cargo")], by = vdiretor[c("CD_ESCOLA")], FUN = mean)
vdiretor <- aggregate(x = vdiretor[c("Formacao", "Exp_Total", "Exp_Cargo")], by = vdiretor[c("CD_ESCOLA")], FUN = mean)
vdiretor =  merge(vdiretor, outra_atividade, all=TRUE)
names(vdiretor)[names(vdiretor) == "Formacao"] <- "VDir_Formacao"
names(vdiretor)[names(vdiretor) == "Exp_Cargo"] <- "VDir_Exp_Cargo"
names(vdiretor)[names(vdiretor) == "Exp_Total"] <- "VDir_Exp_Mag"

pc = read.csv2("DesafioSeducSP/PROFESSOR_COORDENADOR.csv")
pc <- pc[pc$X2018=="SIM",]
pc <- pc[,c("CD_ESCOLA", "id_interno")]
pc <- merge(pc, servidores, x.all = TRUE)
pc <- pc[pc$Cargo != "DIRETOR DE ESCOLA",]
pc <- pc[pc$Cargo != "DIRETOR TECNICO I",]
pc <- pc[pc$Cargo != "DIRETOR TECNICO II",]
pc <- pc[pc$Cargo != "SUPERVISOR DE ENSINO",]

pc <- aggregate(x = pc[c("Formacao", "Exp_Cargo")], by = pc[c("CD_ESCOLA")], FUN = mean)
names(pc)[names(pc) == "Formacao"] <- "PC_Formacao"
names(pc)[names(pc) == "Exp_Cargo"] <- "PC_Exp_Cargo"

gestores = merge(diretor, vdiretor, all=TRUE)
gestores = merge(gestores, pc, all=TRUE)

#Professores 
cargahoraria = read.csv2("DesafioSeducSP/BASE_CARGA_HOR_SALA_AULA_0419.csv")
cargahoraria <- cargahoraria[,c("CODESC","TOT_AULA_LIVRE", "TOT_AULA_SUBST", "TOT_AULA_LIVRE_NOTURNO", "TOT_AULA_SUBST_NOTURNO", "JORNADA", "id_interno")]
names(cargahoraria)[names(cargahoraria) == "CODESC"] <- "CD_ESCOLA"
prof <- merge(servidores, cargahoraria, all=TRUE)
prof <- prof[prof$Cargo != "AGENTE DE ORGANIZACAO ESCOLAR",]
prof <- prof[prof$Cargo != "DIRETOR DE ESCOLA",]

prof$Jornada <- 40*(prof$JORNADA=="C")+30*(prof$JORNADA=="B") + 24*(prof$JORNADA=="I") + 12*(prof$JORNADA=="R")
prof <- aggregate(x = prof[c("FaltaJust", "FaltaNaoJust", "FaltaDireito", "Formacao", "Exp_Total", "TOT_AULA_LIVRE", "TOT_AULA_SUBST", "TOT_AULA_LIVRE_NOTURNO", "TOT_AULA_SUBST_NOTURNO")], by = prof[c("CD_ESCOLA")], FUN = mean, na.rm=TRUE)
names(prof)[names(prof) == "FaltaJust"] <- "Prof_FaltaJust"
names(prof)[names(prof) == "FaltaNaoJust"] <- "Prof_FaltaNaoJust"
names(prof)[names(prof) == "FaltaDireito"] <- "Prof_Direito"
names(prof)[names(prof) == "Formacao"] <- "Prof_Formacao"
names(prof)[names(prof) == "Exp_Total"] <- "Prof_Exp_Mag"
names(prof)[names(prof) == "TOT_AULA_LIVRE"] <- "Prof_TOT_AULA_LIVRE"
names(prof)[names(prof) == "TOT_AULA_SUBST"] <- "Prof_TOT_AULA_SUBST"
names(prof)[names(prof) == "TOT_AULA_LIVRE_NOTURNO"] <- "Prof_TOT_AULA_LIVRE_NOTURNO"
names(prof)[names(prof) == "TOT_AULA_SUBST_NOTURNO"] <- "Prof_TOT_AULA_SUBST_NOTURNO"

#escolas

pei = read.csv("DesafioSeducSP/pei.csv")
pei["T_PEI"] <- c(2018-pei$ano)
pei <- pei[,-c(1,3,4)]
names(pei)[names(pei) == "COD_ESC"] <- "CODESC"

eti = read.csv("DesafioSeducSP/ETI.csv")
eti["T_ETI"] <- c(2018-eti$ano)
eti <- eti[,-c(1,3,4)]
names(eti)[names(eti) == "COD_ESC"] <- "CODESC"

inse = read.csv2("DesafioSeducSP/INSE_Geral 2018_1.csv")
inse <- inse[,c(1,5)]
names(inse)[names(inse) == "NIVEL.SOCIOECONOMICO.DOS.ALUNOS"] <- "INSE"

cls = read.csv2("DesafioSeducSP/12_MMR_Clusters.csv")
cls = cls[,c(3,4,5)]
names(cls)[names(cls) == "CD_ESCOLA"] <- "CODESC"

idesp = read.csv2("DesafioSeducSP/IDESP por Escola - 2018.csv",  stringsAsFactors = FALSE)
idesp = idesp[c("CODIGO_CIE","ANOS_INICIAIS","ANOS_FINAIS","ENSINO_MEDIO")]
names(idesp)[names(idesp) == "CODIGO_CIE"] <- "CODESC"

meta = read.csv2("DesafioSeducSP/IDESP Metas por Escola - 2018.csv")
meta = meta[c("ï..CÃ.DIGO.CIE","ANOS.INICIAIS","ANOS.FINAIS","ENSINO.MÃ.DIO")]
names(meta)[names(meta) == "ï..CÃ.DIGO.CIE"] <- "CODESC"

meta=merge(meta, idesp, all=TRUE)

meta$ANOS_INICIAIS2 <- as.double(meta$ANOS_INICIAIS)
meta$ANOS_FINAIS2 <- as.double(meta$ANOS_FINAIS)
meta$ENSINO_MEDIO2 <- as.double(meta$ENSINO_MEDIO)

meta["Dif_Meta_Idesp_AI"] <- meta$ANOS_INICIAIS2 - meta$ANOS.INICIAIS
meta["Dif_Meta_Idesp_AF"] <- meta$ANOS_FINAIS2 - meta$ANOS.FINAIS
meta["Dif_Meta_Idesp_EM"] <- meta$ENSINO_MEDIO2 - meta$ENSINO.MÃ.DIO
meta[["Dif_Meta_Idesp_AI"]][is.na(meta[["Dif_Meta_Idesp_AI"]])] <- 0
meta[["Dif_Meta_Idesp_AF"]][is.na(meta[["Dif_Meta_Idesp_AF"]])] <- 0
meta[["Dif_Meta_Idesp_EM"]][is.na(meta[["Dif_Meta_Idesp_EM"]])] <- 0
meta["Dif_Meta_Idesp_Geral"] <- meta$Dif_Meta_Idesp_AI + meta$Dif_Meta_Idesp_AF + meta$Dif_Meta_Idesp_EM
meta$Dif_Meta_Idesp_AI <- meta$ANOS_INICIAIS2 - meta$ANOS.INICIAIS
meta$Dif_Meta_Idesp_AF <- meta$ANOS_FINAIS2 - meta$ANOS.FINAIS
meta$Dif_Meta_Idesp_EM <- meta$ENSINO_MEDIO2 - meta$ENSINO.MÃ.DIO


meta <- meta[c("CODESC","Dif_Meta_Idesp_AI","Dif_Meta_Idesp_AF","Dif_Meta_Idesp_EM", "Dif_Meta_Idesp_Geral", "ANOS.INICIAIS","ANOS.FINAIS","ENSINO.MÃ.DIO")]

total = read.csv("DesafioSeducSP/total.csv")
names(total)[names(total) == "ï..CD_ESCOLA"] <- "CODESC"
total <- total[total$MES_VIG == "2", ]
total <- aggregate(x = total[c("NR_HORA_AULA_SEMANA")], by = total[c("CODESC")], FUN = sum)
names(total)[names(total) == "NR_HORA_AULA_SEMANA"] <- "Horas_Sem_Aula"

infra = read.csv2("DesafioSeducSP/06_Escolas_Dependencias.csv")
infra["Infra"] <- infra$TOT_SALAS_AULA + infra$TOT_SALA_LEITURA + infra$TOT_LAB_CIENCIA + infra$TOT_QUADRA + infra$SANITARIO_AL_MASC + infra$SANITARIO_AL_FEM
infra = infra[c("CODESC", "Infra")]

qalunos <- read.csv2("DesafioSeducSP/10_Escolas_Classes_Qtde_Alunos.csv")
names(qalunos)[names(qalunos) == "COD_ESC"] <- "CODESC"
qalunos <- qalunos[qalunos$ï..ANO == 2018,]
qalunos <- qalunos[c("CODESC","TipoEnsino","SERIE","QTDE_ALUNOS")]

escola <- merge(eti, pei, all=TRUE)
escola <- merge(escola, inse, all=TRUE)
escola <- merge(escola, meta, all=TRUE)
escola <- merge(escola, total, all=TRUE)
escola <- merge(escola, infra, all=TRUE)
escola <- merge(escola, cls, all=TRUE)
escola <- merge(escola, qalunos, all=TRUE)

vexp <- merge(prof, gestores, all=TRUE)
names(vexp)[names(vexp) == "CD_ESCOLA"] <- "CODESC"
vexp <- merge(vexp, escola, all=TRUE)

#variaveis dependentes
saresp <- read.csv2("DesafioSeducSP/SARESP_escolas_2018.csv")
saresp <- saresp[c("CODESC","SERIE_ANO","ds_comp","medprof")]


fluxo <- read.csv2("DesafioSeducSP/Fluxo Escolar 2018 - por escola_0.csv")
names(fluxo)[names(fluxo) == "CD_ESCOLA"] <- "CODESC"
fluxo <- fluxo[fluxo$ï..ANO == 2018,]
fluxo <- fluxo[c("CODESC","APR_1","APR_2","APR_3")]

vdep <- merge(saresp, fluxo, all=TRUE)

#Regressões

#por escola
base <- merge(vdep, vexp)
base[["T_PEI"]][is.na(base[["T_PEI"]])] <- 0
base[["T_ETI"]][is.na(base[["T_ETI"]])] <- 0
vdep_escola <- aggregate(x = vdep[c("medprof", "APR_1", "APR_2", "APR_3")], by = vdep[c("CODESC")], FUN = mean)
vexp_escola <- aggregate(x = vexp[c("Prof_FaltaJust", "Prof_FaltaNaoJust", "Prof_Direito", "Prof_Formacao", "Prof_Exp_Mag", "Prof_TOT_AULA_LIVRE", "Prof_TOT_AULA_SUBST", "Prof_TOT_AULA_LIVRE_NOTURNO", "Prof_TOT_AULA_SUBST_NOTURNO", "Dir_Formacao", "Dir_Exp_Cargo", "Dir_Exp_Mag", "VDir_Formacao", "VDir_Exp_Cargo", "VDir_Exp_Mag", "PC_Formacao", "PC_Exp_Cargo", "T_ETI", "T_PEI", "INSE", "Dif_Meta_Idesp_AI", "Dif_Meta_Idesp_AF", "Dif_Meta_Idesp_EM", "Dif_Meta_Idesp_Geral", "ANOS.INICIAIS","ANOS.FINAIS","ENSINO.MÃ.DIO", "Horas_Sem_Aula", "Infra", "QTDE_ALUNOS")], by = vexp[c("CODESC")], FUN = mean)
base <- merge(vdep_escola, vexp_escola)
base[["T_PEI"]][is.na(base[["T_PEI"]])] <- 0
base[["T_ETI"]][is.na(base[["T_ETI"]])] <- 0
re_escola <- lm(medprof ~ Dir_Formacao + Dir_Exp_Cargo + Dir_Exp_Mag + VDir_Formacao + VDir_Exp_Mag + VDir_Exp_Cargo + PC_Formacao + PC_Exp_Cargo+ Prof_Exp_Mag + Prof_Formacao + Prof_FaltaJust + Prof_Direito +  Prof_FaltaNaoJust + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + T_PEI + T_ETI + Prof_FaltaNaoJust + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO+ Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Dif_Meta_Idesp_Geral + Dir_Formacao + Dir_Exp_Cargo + Dir_Exp_Mag + VDir_Exp_Mag + PC_Formacao + PC_Exp_Cargo + Horas_Sem_Aula + Infra + INSE + QTDE_ALUNOS, data=base)

#Ensino Médio
vdep_em <- vdep[vdep$SERIE_ANO == "EM-3ª série",]
vdep_em <- aggregate(x = vdep_em[c("medprof", "APR_3")], by = vdep_em[c("CODESC")], FUN = mean)
vexp_em <- vexp[vexp$TipoEnsino == "ENSINO MEDIO",]
vexp_em <- aggregate(x = vexp_em[c("Prof_FaltaJust", "Prof_FaltaNaoJust", "Prof_Direito", "Prof_Formacao", "Prof_Exp_Mag", "Prof_TOT_AULA_LIVRE", "Prof_TOT_AULA_SUBST", "Prof_TOT_AULA_LIVRE_NOTURNO", "Prof_TOT_AULA_SUBST_NOTURNO", "Dir_Formacao", "Dir_Exp_Cargo", "Dir_Exp_Mag", "VDir_Formacao", "VDir_Exp_Cargo", "VDir_Exp_Mag", "PC_Formacao", "PC_Exp_Cargo", "T_ETI", "T_PEI", "INSE", "Dif_Meta_Idesp_EM", "Horas_Sem_Aula", "Infra", "QTDE_ALUNOS")], by = vexp_em[c("CODESC")], FUN = mean)
base_em <- merge(vdep_em, vexp_em)
base_em[["T_PEI"]][is.na(base_em[["T_PEI"]])] <- 0
base_em[["T_ETI"]][is.na(base_em[["T_ETI"]])] <- 0
re_em <- lm(medprof ~ Dir_Formacao + Dir_Exp_Cargo + Dir_Exp_Mag + VDir_Formacao + VDir_Exp_Mag + VDir_Exp_Cargo + PC_Formacao + PC_Exp_Cargo+ Prof_Exp_Mag + Prof_Formacao + Prof_FaltaJust + Prof_Direito + Prof_FaltaNaoJust + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + T_PEI + T_ETI  + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Dif_Meta_Idesp_EM + Horas_Sem_Aula + Infra + INSE + QTDE_ALUNOS, data=base_em)

#Anos Finais 
vdep_af <- vdep[vdep$SERIE_ANO == "9º Ano EF",]
vdep_af <- aggregate(x = vdep_af[c("medprof", "APR_2")], by = vdep_af[c("CODESC")], FUN = mean)
vexp_af <- vexp[vexp$TipoEnsino == "ENSINO FUNDAMENTAL DE 9 ANOS",]
vexp_af <- vexp_af[vexp_af$SERIE >= "6",]
vexp_af <- aggregate(x = vexp_af[c("Prof_FaltaJust", "Prof_FaltaNaoJust", "Prof_Direito", "Prof_Formacao", "Prof_Exp_Mag", "Prof_TOT_AULA_LIVRE", "Prof_TOT_AULA_SUBST", "Prof_TOT_AULA_LIVRE_NOTURNO", "Prof_TOT_AULA_SUBST_NOTURNO", "Dir_Formacao", "Dir_Exp_Cargo", "Dir_Exp_Mag", "VDir_Formacao", "VDir_Exp_Cargo", "VDir_Exp_Mag", "PC_Formacao", "PC_Exp_Cargo", "T_ETI", "T_PEI", "INSE", "Dif_Meta_Idesp_AF", "Horas_Sem_Aula", "Infra", "QTDE_ALUNOS")], by = vexp_af[c("CODESC")], FUN = mean)
base_af <- merge(vdep_af, vexp_af)
base_af[["T_PEI"]][is.na(base_af[["T_PEI"]])] <- 0
base_af[["T_ETI"]][is.na(base_af[["T_ETI"]])] <- 0
re_af <- lm(medprof ~ Dir_Formacao + Dir_Exp_Cargo + Dir_Exp_Mag + VDir_Formacao + VDir_Exp_Mag + VDir_Exp_Cargo + PC_Formacao + PC_Exp_Cargo+ Prof_Exp_Mag + Prof_Formacao + Prof_FaltaJust + Prof_Direito + Prof_FaltaNaoJust + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + T_PEI + T_ETI  + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Dif_Meta_Idesp_AF  + Horas_Sem_Aula + Infra + INSE + QTDE_ALUNOS, data=base_af)

#Anos Iniciais 
vdep_ai <- vdep[vdep$SERIE_ANO == "5º Ano EF",]
vdep_ai <- aggregate(x = vdep_ai[c("medprof", "APR_1")], by = vdep_ai[c("CODESC")], FUN = mean)
vexp_ai <- vexp[vexp$TipoEnsino == "ENSINO FUNDAMENTAL DE 9 ANOS",]
vexp_ai <- vexp_ai[vexp_ai$SERIE <= "5",]
vexp_ai <- aggregate(x = vexp_ai[c("Prof_FaltaJust", "Prof_FaltaNaoJust", "Prof_Direito", "Prof_Formacao", "Prof_Exp_Mag", "Prof_TOT_AULA_LIVRE", "Prof_TOT_AULA_SUBST", "Prof_TOT_AULA_LIVRE_NOTURNO", "Prof_TOT_AULA_SUBST_NOTURNO", "Dir_Formacao", "Dir_Exp_Cargo", "Dir_Exp_Mag", "VDir_Formacao", "VDir_Exp_Cargo", "VDir_Exp_Mag", "PC_Formacao", "PC_Exp_Cargo", "T_ETI", "T_PEI", "INSE", "Dif_Meta_Idesp_AI", "Horas_Sem_Aula", "Infra", "QTDE_ALUNOS")], by = vexp_ai[c("CODESC")], FUN = mean)
base_ai <- merge(vdep_ai, vexp_ai)
base_ai[["T_PEI"]][is.na(base_ai[["T_PEI"]])] <- 0
base_ai[["T_ETI"]][is.na(base_ai[["T_ETI"]])] <- 0
re_ai <- lm(medprof ~ Dir_Formacao + Dir_Exp_Cargo + Dir_Exp_Mag + VDir_Formacao + VDir_Exp_Mag + VDir_Exp_Cargo + PC_Formacao + PC_Exp_Cargo+ Prof_Exp_Mag + Prof_Formacao + Prof_FaltaJust + Prof_Direito + Prof_FaltaNaoJust + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + T_PEI + T_ETI  + Prof_TOT_AULA_LIVRE_NOTURNO + Prof_TOT_AULA_SUBST_NOTURNO + Prof_TOT_AULA_LIVRE + Prof_TOT_AULA_SUBST + Dif_Meta_Idesp_AI + Horas_Sem_Aula + Infra + INSE + QTDE_ALUNOS, data=base_ai)


#Características dos gestores
base$Prof_Exp_Mag <- as.numeric(base$Prof_Exp_Mag)
base$Dir_Exp_Cargo <- as.numeric(base$Dir_Exp_Cargo)
base$Dir_Exp_Mag <- as.numeric(base$Dir_Exp_Mag)
base$VDir_Exp_Cargo <- as.numeric(base$VDir_Exp_Cargo)
base$VDir_Exp_Mag <- as.numeric(base$VDir_Exp_Mag)
base$PC_Exp_Cargo <- as.numeric(base$PC_Exp_Cargo) 
quantile(base$medprof, c(.15, 0.5, .85), na.rm = TRUE)
#15% de pior desempenho
base_15 <- base[base$medprof <= 223.9175,]
#15% de melhor desempenho
base_85 <- base[base$medprof <= 279.0500,]
t.test(base_85$Dir_Formacao, base_15$Dir_Formacao)
t.test(base_85$Dir_Exp_Cargo, base_15$Dir_Exp_Cargo)
t.test(base_85$Dir_Exp_Mag, base_15$Dir_Exp_Mag)
t.test(base_85$VDir_Formacao, base_15$VDir_Formacao)
t.test(base_85$VDir_Exp_Cargo, base_15$VDir_Exp_Cargo)
t.test(base_85$VDir_Exp_Mag, base_15$VDir_Exp_Mag)
t.test(base_85$PC_Formacao, base_15$PC_Formacao)
t.test(base_85$PC_Exp_Cargo, base_15$PC_Exp_Cargo)
t.test(base_85$Dif_Meta_Idesp_Geral, base_15$Dif_Meta_Idesp_Geral)







