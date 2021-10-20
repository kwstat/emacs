
ncol=6
s0 <- round( (0:(ncol-1))/(ncol-1) * 99 +1,0 )

s1 <- (1.1)^(0:(ncol-1))
s1 <- s1-1
s1 <- s1/max(s1)
s1 <- round(s1*99,0)+1
s1

s3 <- (1.3)^(0:(ncol-1))
s3 <- s3-1
s3 <- s3/max(s3)
s3 <- round(s3*99,0)+1
s3

s4 <- (1.4)^(0:(ncol-1))
s4 <- s4-1
s4 <- s4/max(s4)
s4 <- round(s4*99,0)+1
s4

s5 <- (1.5)^(0:(ncol-1))
s5 <- s5-1
s5 <- s5/max(s5)
s5 <- round(s5*99,0)+1
s5


libs(pals)
reds <- colorRampPalette(c("#fae6e6","#e1bebe","#c89797","#b06f6f","#974848","#7e2020"))(100)
ong <- colorRampPalette(c("#ffece0","#f0cab3","#e1a886","#d1855a","#c2632d","#b34100"))(100)
yels <- colorRampPalette(c("#fdfde7","#d5d5b9","#c1ac8b","#a2845c","#845b2e","#663300"))(100)
grns <- colorRampPalette(c("#f0fff0","#cedfce","#abbfab","#899f89","#677f67","#455f45"))(100)
blus <- colorRampPalette(c("#d9e9ff","#b4cae6","#8faccc","#6a8db3","#456f99","#205080"))(100)
purs <- colorRampPalette(c("#edebff","#c6c2e5","#9e9acb","#7771b2","#4f499b","#28207e"))(100)
gras <- colorRampPalette(c("#f4f4f4","#d1d1d1","#aeaeae","#8b8b8b","#696969","#464646"))(100)
  
mycols <- reds
mycols <- ong
mycols <- yels
mycols <- grns
mycols <- blus
mycols <- purs
mycols <- gras
pal.bands(mycols[s0],
          mycols[s1],
          mycols[s3],
          mycols[s4],
          mycols[s5],
          labels=c("Lin","10%","30%","40%","50%"))
mycols[s4]



all <- c("#fae6e6","#eed3d3","#c89797","#b06f6f","#974848","#7e2020","#3f1010","#000000","#ffece0","#f8dccb","#e1a886","#d1855a","#c2632d","#b34100","#5a2100","#000000","#fdfde7","#fbf7c4","#c1ac8b","#a2845c","#845b2e","#663300","#331a00","#000000","#f0fff0","#e0f0e0","#abbfab","#899f89","#677f67","#455f45","#223f22","#000000","#d9e9ff","#c8daf3","#8faccc","#6a8db3","#456f99","#205080","#1f292e","#000000","#edebff","#dbd8f3","#9e9acb","#7771b2","#4f499b","#28207e","#201f2e","#000000","#f4f4f4","#e4e4e4","#aeaeae","#8b8b8b","#696969","#464646","#232323","#000000")
pal.cube(all)
pal.cube(all, type="LUV")
