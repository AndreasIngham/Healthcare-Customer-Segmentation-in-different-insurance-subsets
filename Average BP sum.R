
bp <- read.csv("examination.csv")

BP1 <- bp$BPXSY1[bp$PEASCST1==1] + bp$BPXDI1[bp$PEASCST1==1]
BP2 <- bp$BPXSY2[bp$PEASCST1==1] + bp$BPXDI2[bp$PEASCST1==1]
BP3 <- bp$BPXSY3[bp$PEASCST1==1] + bp$BPXDI3[bp$PEASCST1==1]

bps <- data.frame(completed_exam, BP1, BP2, BP3)
head(bps, 10)
bps2 <- na.omit(bps)
tail(bps2, 10)

bps2$mean <- rowMeans(subset(bps2, select = c(BP1, BP2, BP3)))
head(bps2)








###PRESENTATION FLOW
#techniques
#strategy
#recommendation
#challenges limitattions
#impact/value add



