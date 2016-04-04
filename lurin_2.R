library(GISTools); library(rgdal)
seda1 <- '/Volumes/KYARA/PFC\ CROUSSE/Lima\ GIS\ Layers/'
seda2 <- 'I.DIAGNOSTICO/7_SERVICIOS_PUBLICOS/SHP/'
seda3 <- 'Acceso\ al\ agua\ de\ la\ poblacio\314\201n/shp/'
seda <- readOGR(dsn = paste0(seda1, seda2, seda3, sep = ''), 
                layer = 'Agua_INEI_mz')
lim.dist1 <- '/Volumes/KYARA/PFC\ CROUSSE/Lima\ GIS\ Layers'
lim.dist2 <- '/1_DIAGNOSTICO_TERRITORIAL/13_Cartografia_General/Capas/'
lim.dist <- readOGR(dsn = paste(lim.dist1, lim.dist2, sep = ''),
                    layer = 'Limite_Distrital')
manz <- readOGR(dsn = paste(lim.dist1, lim.dist2, sep = ''),
                layer = 'Manzanas')
head(data.frame(manz.match))
head(data.frame(seda.match))

par(mar = c(0, 0, 0, 0))
manz.match <- manz[which(grepl(pattern = '016|042', x = manz$COD_DIST)),]
seda.match <- seda[which(grepl(pattern = 'LURIN|VILLA EL SALVADOR', x = seda$NOMCCPP)),]
shades.dias <- auto.shading(x = seda.match$NUM_DIAS, n = 7,
                            cols = brewer.pal(7, 'Blues'), cutter = rangeCuts)
plot(manz.match, lwd = 0.15)
plot(lim.dist, add = TRUE, lwd = .9, lty = 3)
choropleth(sp = seda.match, v = seda.match$NUM_DIAS, shading = shades.dias, add = TRUE,
           lwd = 0.1)
locator()
choro.legend(px = 283000, py = 8643000, sh = shades.dias, cex = 0.7)


# 24/7
shades.horas <- auto.shading(x = seda.match$NUM_HORAS, n = 5,
                             cols = brewer.pal(5, 'Greens'), cutter = rangeCuts)
plot(manz.match, lwd = 0.15)
plot(lim.dist, add = TRUE, lwd = .9, lty = 3)
choropleth(sp = seda.match, v = seda.match$NUM_HORAS, shading = shades.horas, add = TRUE,
           lwd = 0.1)
choro.legend(px = 283000, py = 8643000, sh = shades.horas, cex = 0.7)


# cisternas
shades.ciste <- auto.shading(x = seda.match$CISTERNA, n = 5,
                             cols = brewer.pal(5, 'Oranges'), cutter = rangeCuts)
plot(manz.match, lwd = 0.15)
plot(lim.dist, add = TRUE, lwd = .9, lty = 3)
choropleth(sp = seda.match, v = seda.match$CISTERNA, shading = shades.ciste, add = TRUE,
           lwd = 0.1)
choro.legend(px = 283000, py = 8643000, sh = shades.ciste, cex = 0.7)


# red de agua y pozos
pozo1 <- '/Volumes/KYARA/PFC\ CROUSSE/Lima\ GIS\ Layers/'
pozo2 <- 'I.DIAGNOSTICO/2_MEDIO_AMBIENTE/SHP/pozos\ sedapal/'
pozo <- readOGR(dsn = paste(pozo1, pozo2, sep = ''), 
                layer = 'Pozos_SEDAPAL')
class(pozo)
plot(manz.match, lwd = 0.15)
plot(lim.dist, add = TRUE, lwd = .9, lty = 3)
points(pozo)
shades.red <- auto.shading(x = seda.match$AGUA_RED, n = 5,
                             cols = brewer.pal(5, 'Purples'), cutter = rangeCuts)
plot(manz.match, lwd = 0.15)
plot(lim.dist, add = TRUE, lwd = .9, lty = 3)
choropleth(sp = seda.match, v = seda.match$AGUA_RED, shading = shades.red, add = TRUE,
           lwd = 0.1)
choro.legend(px = 283000, py = 8643000, sh = shades.red, cex = 0.7)
points(pozo, col = 'red', pch = 16)


