library(rgdal); library(ggmap); library(rgeos); library(maptools); library(svglite)
library(readxl); library(dplyr); library(tidyr); library(stringr); library(magrittr)

x <- readOGR(dsn = path.expand('~/Dropbox/shapes/ECONOMIA/SHP/Actividades_Economicas_INEI/'),
             layer = 'Actividades_Economicas_ZO')
x <- spTransform(x, CRS('+proj=longlat +datum=WGS84'))
lurin <- x[which(x$NOMCCPP %in% c('LURIN', 'PACHACAMAC', 'VILLA EL SALVADOR',
                                  'VILLA MARIA DEL TRIUNFO', 'CIENEGUILLA')),]

# Para poner metadata en el data frame de las coordinadas es necesario que haya una 
# columna de valores unicos en el data frame de la meta data que se repita en el data
# frame de las coordenadas. Los juntamos usando merge()

data <- fortify(lurin)

# frecuencias relativas
df <- lurin@data
df <- df[, 13:32]
id <- row.names(df)
df %<>% 
  mutate_each(funs(./sum(.)))
df$id <- id
df$O_OET <- 0

#frecuencias absolutas
df <- lurin@data
df <- df[, 13:32]
df$id <- row.names(df)

lurin.man <- merge(data, df, by.x = 'id', by.y = 'id')
lurin.man %<>%
  gather(var, value, 8:ncol(lurin.man))

coord <- data.frame(lon = -76.90, lat = -12.20)

map <- get_map(location = coord, source = 'google', maptype = 'terrain', zoom = 12,
               color = 'bw', scale = 1)

ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = value), 
               data = lurin.man, color = 'white', alpha = .4, size = .2) +
  scale_fill_gradient(low = 'dark grey', high = 'black') +
  facet_wrap(~var)

=============================================

dest <- '~/Dropbox/shapes/ECONOMIA/SHP/Establecimientos por actividad economica/'
y <- readOGR(dsn = path.expand(dest),
             layer = 'ESTAB_ACT_ECONOMICA')
y <- spTransform(y, CRS('+proj=longlat +datum=WGS84'))
lurin <- y[which(y$DISTNOMB == 'LURIN'),]

data.y <- fortify(lurin)
df.y <- lurin@data
df.y <- df.y[,11:ncol(df.y)]
df.y$id <- row.names(df.y)
df %<>% 
  mutate_each(funs(./sum(.)))
df$id <- id
df$O_OET <- 0

lurin.y.man <- merge(data.y, df.y, by.x = 'id', by.y = 'id')
?get_map

svglite('~/Documents/kikivalle.svg', width = 33.11, height = 23.39)
ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = value), 
               data = lurin.man, 
               color = 'grey', alpha = 1, size = .05) + 
  scale_fill_gradient(low = 'white', high = 'dark blue', guide = 'colourbar') +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~var)
dev.off()

=============================================

densidad <- readOGR(dsn = path.expand('~/Documents/Densidad/'), 
                    layer = 'Densidad_zona_censal_07')
densidad <- spTransform(densidad, CRS('+proj=longlat +datum=WGS84'))
valle <- densidad[which(densidad$NOMCCPP %in% c('LURIN', 'PACHACAMAC', 
                                                'VILLA EL SALVADOR',
                                  'VILLA MARIA DEL TRIUNFO', 'CIENEGUILLA')),]

data.d <- fortify(valle)
df.d <- valle@data
df.d <- df.d[, 11:ncol(df.d)]
df.d$id <- row.names(df.d)

valle.man <- merge(data.d, df.d, by.x = 'id', by.y = 'id')
valle.man %<>%
  gather(var, value, 8:ncol(valle.man))

coord <- data.frame(lon = -76.90, lat = -12.20)

map <- get_map(location = coord, source = 'google', maptype = 'terrain', zoom = 12,
               color = 'bw', scale = 1)
densidad.man[which(densidad.man$var == 'DENSIDAD'),]

y <- valle.man[which(valle.man$var == 'DENSIDAD'),]
y$value <- as.numeric(y$value)

svglite('~/Documents/kikidensidad.svg', width = 33.11, height = 23.39)  
ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = value), 
               data = y, 
               color = 'grey', alpha = 1, size = 0) + 
  scale_fill_gradient(low = 'white', high = '#F4D100', guide = 'colourbar') +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) 
dev.off()

=============================================

vivienda <- readOGR(dsn = path.expand('~/Documents/Tipo de Vivienda/'), 
                    layer = 'Tipo_vivienda')
vivienda <- spTransform(vivienda, CRS('+proj=longlat +datum=WGS84'))
valle.v <- vivienda[which(vivienda$NOMB_DIST %in% c('LURIN', 'PACHACAMAC', 
                                                    'VILLA EL SALVADOR', 'CIENEGUILLA',
                                                    'VILLA MARIA DEL TRIUNFO')),]
data.v <- fortify(valle.v)
df.v <- valle.v@data
df.v <- df.v[, (ncol(df.v)-8):(ncol(df.v)-3)]
df.v$id <- row.names(df.v)
df.v <- as.data.frame(sapply(df.v, as.numeric))

valle.v.man <- merge(data.v, df.v, by.x = 'id', by.y = 'id')
valle.v.man %<>%
  gather(var, value, 8:ncol(valle.v.man))

coord <- data.frame(lon = -76.90, lat = -12.20)

map <- get_map(location = coord, source = 'google', maptype = 'terrain', zoom = 12,
               color = 'bw', scale = 1)

svglite('~/Documents/kikicasitas.svg', width = 33.11, height = 23.39)  
ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = value), 
               data = valle.v.man) + 
  scale_fill_gradient(low = 'white', high = 'dark red', guide = 'colourbar') +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~var)
dev.off()

=============================================

material <- readOGR(dsn = path.expand('~/Documents/Material de la vivienda/'), 
                    layer = 'Material_Vivienda')
material <- spTransform(material, CRS('+proj=longlat +datum=WGS84'))
valle.m <- material[which(material$NOMB_DIST %in% c('LURIN', 'PACHACAMAC', 
                                                    'VILLA EL SALVADOR', 'CIENEGUILLA',
                                                    'VILLA MARIA DEL TRIUNFO')),]
data.m <- fortify(valle.m)
df.m <- valle.m@data
df.m <- df.m[, (ncol(df.m)-10):(ncol(df.m)-3)]
names(df.m) <- c('Ladrillo o bloque de cemento', 'Adobe o tapia', 'Madera', 'Quincha',
                 'Estera', 'Piedra con barro', 'Piedra, sillar con cal o cemento',
                 'Otro material predominante en paredes')
df.m$id <- row.names(df.m)
df.v <- as.data.frame(sapply(df.v, as.numeric))

valle.m.man <- merge(data.m, df.m, by.x = 'id', by.y = 'id')
valle.m.man %<>%
  gather(var, value, 8:ncol(valle.m.man))

coord <- data.frame(lon = -76.90, lat = -12.20)

map <- get_map(location = coord, source = 'google', maptype = 'terrain', zoom = 12,
               color = 'bw', scale = 1)

svglite('~/Documents/kikimateriales.svg', width = 33.11, height = 23.39)  
ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = value), 
               data = valle.m.man) + 
  scale_fill_gradient(low = 'white', high = 'dark red', guide = 'colourbar') +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~var)
dev.off()

=============================================

cobertura <- readOGR(dsn = path.expand('~/Documents/Cobertura de Agua SEDAPAL/'), 
                    layer = 'Cobertura_agua_SEDAPAL')
cobertura <- spTransform(cobertura, CRS('+proj=longlat +datum=WGS84'))
sedapal.c <- cobertura[which(cobertura$DISTRITO %in% c('LURIN', 'PACHACAMAC', 
                                                    'VILLA EL SALVADOR', 'CIENEGUILLA',
                                                    'VILLA MARIA DEL TRIUNFO')),]
str(cobertura@data)
data.s <- fortify(sedapal.c)
df.s
df.s <- sedapal.c@data
df.s <- df.s[, (ncol(df.s)-10):(ncol(df.s)-3)]
names(df.m) <- c('Ladrillo o bloque de cemento', 'Adobe o tapia', 'Madera', 'Quincha',
                 'Estera', 'Piedra con barro', 'Piedra, sillar con cal o cemento',
                 'Otro material predominante en paredes')
df.s$id <- row.names(df.s)

valle.s.man <- merge(data.s, df.s, by.x = 'id', by.y = 'id')

coord <- data.frame(lon = -76.90, lat = -12.20)

map <- get_map(location = coord, source = 'google', maptype = 'terrain', zoom = 12,
               color = 'bw', scale = 1)

svglite('~/Documents/kikisedapal.svg', width = 33.11, height = 23.39)  
ggmap(map) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = TIPO_ABAS), 
               data = valle.s.man) + 
  scale_fill_brewer(type = "seq", palette = 3, direction = -1) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank())
dev.off()

=============================================

colegio <- readOGR(dsn = path.expand('~/Documents/Instituciones Educativas/shp/'), 
                     layer = 'IE_P_LIMA2012_project')
colegio <- spTransform(colegio, CRS('+proj=longlat +datum=WGS84'))
colegio.v <- colegio[which(colegio$DIST %in% c('LURIN', 'PACHACAMAC', 
                                                       'VILLA EL SALVADOR', 'CIENEGUILLA',
                                                       'VILLA MARIA DEL TRIUNFO')),]
limite <- readOGR(dsn = path.expand('~/Documents/Capas/'), 
                  layer = 'Limite_Distrital')
limite <- spTransform(limite, CRS('+proj=longlat +datum=WGS84'))
str(limite)
limite.v <- limite[which(limite$DISTNOMB %in% c('LURIN', 'PACHACAMAC', 
                                               'VILLA EL SALVADOR', 'CIENEGUILLA',
                                               'VILLA MARIA DEL TRIUNFO')),]
data.v <- fortify(limite.v)


# Acá el procedimiento es distinto porque estamos trabajando con puntos, no con poligonos
# Cogemos el archivo@coords y juntamos usando cbind() con el archivo@data. Luego hacemos
# los filtros y usamos ese data.frame en ggplot usando el geom_point()

data.c <- colegio.v@coords
df.c <- colegio.v@data
valle.col <- cbind(df.c, data.c)
valle.col <- valle.col[which(valle.col$NIVEL %in% c('I', 'P', 'S', 'IP', 'PS', 'IPS')),]

coord <- data.frame(lon = -76.90, lat = -12.20)

map <- get_map(location = coord, source = 'google', maptype = 'terrain', zoom = 12,
               color = 'bw', scale = 1)

# Si corremos geom_path() para los límites distritales sin usar coord_map() corremos 
# el riesgo de que se trunquen los mapas y salgan cosas raras. Por esto se tiene que
# poner un par de variables adicionales en ggmap(maprange = FALSE, extent = 'normal) y
# para que el mapa final esté a la medida que queremos se debe llamar a la función 
# coord_map() con los atributos coord_map(projection = 'mercator', xlim = c(attr(map, 
# 'bb')$ll.lon, attr(map, 'bb')$ur.lon), ylim = c(attr(map, 'bb')$ll.lat, attr(map, 
# 'bb')$ur.lat))

svglite('~/Documents/kikicolegios.svg', width = 33.11, height = 23.39)  
ggmap(map, maprange = FALSE, extent = 'normal') +
  geom_path(aes(x = long, y = lat, group = group), data = data.v) +
  geom_point(aes(x = coords.x1, y = coords.x2, group = coords.x3, shape = NIVEL, 
                 color = NIVEL), data = valle.col, size = 8) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(projection = 'mercator', 
            xlim = c(attr(map, 'bb')$ll.lon, attr(map, 'bb')$ur.lon),
            ylim = c(attr(map, 'bb')$ll.lat, attr(map, 'bb')$ur.lat))
dev.off()

=============================================

cultura <- readOGR(dsn = path.expand('~/Documents/Cultural/'), 
                   layer = 'Centros_culturales')
cultura <- spTransform(cultura, CRS('+proj=longlat +datum=WGS84'))
str(cultura)
unique(cultura$ZONAS)
cultura.v <- cultura[which(cultura$ZONAS == 'LIMA SUR'),]


deporte <- readOGR(dsn = path.expand('~/Documents/Equipamiento_Deportivo MML/'), 
                   layer = 'Lozas_Deportivas')
deporte <- spTransform(deporte, CRS('+proj=longlat +datum=WGS84'))
str(deporte@data)
unique(deporte$DISTRITO)
deporte.v <- deporte[which(deporte$DISTRITO %in% c('Lurin', 'Pachacamac', 
                                                    'Villa El Salvador', 'Cieneguilla',
                                                    'Villa Maria del Triunfo')),]

serpar <- readOGR(dsn = path.expand('~/Documents/Areas Verdes SERPAR 2013/'), 
                             layer = '2_Areas_verdes_SERPAR')
serpar <- spTransform(serpar, CRS('+proj=longlat +datum=WGS84'))
str(serpar$COD_DIST)
unique(serpar$COD_DIST)
serpar.v <- serpar[which(serpar$COD_DIST %in% c('016', '019', '035', '040', '042')),]

parques <- readOGR(dsn = path.expand('~/Documents/Areas Verdes Parques/'), 
                             layer = 'Areas_verdes_parques')
parques <- spTransform(parques, CRS('+proj=longlat +datum=WGS84'))
str(parques@data)
parques.v <- parques[which(parques$COD_DIST %in% c('016', '019', '035', '040', '042')),]

# Acá el procedimiento es distinto porque estamos trabajando con puntos, no con poligonos
# Cogemos el archivo@coords y juntamos usando cbind() con el archivo@data. Luego hacemos
# los filtros y usamos ese data.frame en ggplot usando el geom_point()

valle.cul <- cbind(cultura.v@data, cultura.v@coords)
data.dep <- fortify(deporte.v)
df.dep <- deporte.v@data
df.dep$id <- rownames(df.dep)
data.ser <- fortify(serpar.v)
data.parque <- fortify(parques.v)

# Si corremos geom_path() para los límites distritales sin usar coord_map() corremos 
# el riesgo de que se trunquen los mapas y salgan cosas raras. Por esto se tiene que
# poner un par de variables adicionales en ggmap(maprange = FALSE, extent = 'normal) y
# para que el mapa final esté a la medida que queremos se debe llamar a la función 
# coord_map() con los atributos coord_map(projection = 'mercator', xlim = c(attr(map, 
# 'bb')$ll.lon, attr(map, 'bb')$ur.lon), ylim = c(attr(map, 'bb')$ll.lat, attr(map, 
# 'bb')$ur.lat))

svglite('~/Documents/kikiparques.svg', width = 33.11, height = 23.39)  
ggmap(map, maprange = FALSE, extent = 'normal') +
  geom_path(aes(x = long, y = lat, group = group), data = data.v) +
  geom_point(aes(x = coords.x1, y = coords.x2), data = valle.cul, size = 2, 
             color = 'red') +
  geom_polygon(aes(x = long, y = lat, group = group), data = data.dep, fill = 'blue') +
  geom_polygon(aes(x = long, y = lat, group = group), data = data.ser, 
               fill = '#D4D46A') +
  geom_polygon(aes(x = long, y = lat, group = group), data = data.parque, 
               fill = '#555500') +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(projection = 'mercator', 
            xlim = c(attr(map, 'bb')$ll.lon, attr(map, 'bb')$ur.lon),
            ylim = c(attr(map, 'bb')$ll.lat, attr(map, 'bb')$ur.lat))
dev.off()