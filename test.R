round(prop.table(table(dat$hypothyroid)),4)
table(dat[,c("hypothyroid","TSH_measured")])
round(prop.table(table(dat[,c("hypothyroid","TSH_measured")])),4)

table(dat[,c("hypothyroid","T3_measured")])
round(prop.table(table(dat[,c("hypothyroid","T3_measured")])),4)

table(dat[,c("hypothyroid","TT4_measured")])
round(prop.table(table(dat[,c("hypothyroid","TT4_measured")])),4)

table(dat[,c("hypothyroid","T4U_measured")])
round(prop.table(table(dat[,c("hypothyroid","T4U_measured")])),4)

table(dat[,c("hypothyroid","FTI_measured")])
round(prop.table(table(dat[,c("hypothyroid","FTI_measured")])),4)

table(dat[,c("hypothyroid","TBG_measured")])
round(prop.table(table(dat[,c("hypothyroid","TBG_measured")])),4)

table(dat[,c("hypothyroid","TSH_measured","T3_measured","TT4_measured","T4U_measured","FTI_measured")])
