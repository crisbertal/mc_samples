library(raster)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(geojsonsf)
library(stringr)

reclass <- function(path) { 
  roi <- raster(path)
  # Los rangos tienen que ser continuos para que funcione bien
  reclass_df <- c(0, 4, 1,
                  4, 10, 2,
                  10, 14, 3,
                  14, 21, 4,
                  100, 118, 4,
                  200, 219, 1)
  reclass_m <- matrix(reclass_df, ncol = 3, byrow = TRUE)
  roi_classified <- reclassify(roi, reclass_m)
  # plot(roi_classified)
  
  # Se considera filtro de mayoria al aplicar la funcion modal
  roi_filtered <- focal(roi_classified, w = matrix(1, 5, 5), fun = modal)
  # plot(roi_filtered, col = c("red", "green", "cyan", "blue"))
  return(roi_filtered)
} 

extract_samples <- function(roi_filtered) {
  # Pasar a vector
  roi_filtered_vector <-
    rasterToPolygons(roi_filtered, dissolve = TRUE)
  
  # Si el raster esta vacio que termine 
  roi_filtered_vector
  if(is.null(roi_filtered_vector)) {
    print("Mamauevaso")
    return(NULL)
  }
  
  # para utilizar el metodo buffer hay que pasar a objecto S3 con sf
  roi_filtered_vector_sf <- st_as_sf(roi_filtered_vector)
  # tamaño del buffer para que los puntos no caigan en la frontera
  buffer_size <- (-60)
  # aplicar el buffer y limpiar aquellos valores que se hayan quedado fuera
  roi_filtered_vector_buffer <-
    st_buffer(roi_filtered_vector_sf, buffer_size)
  roi_remaining <-
    roi_filtered_vector_buffer[!st_is_empty(roi_filtered_vector_buffer), ]
  # de vuelta a la clase original
  roi_sp_buffered <- as(roi_remaining, Class = "Spatial")
  
  # Samples de forma regular por poligonos
  # Cantidad de samples por poligono aproximadamente
  n_samples <- 50
  samples <- NULL
  for (i in 1:length(roi_sp_buffered@polygons)) {
    samples_i <-
      spsample(roi_sp_buffered@polygons[[i]], n = n_samples, "regular")
    if (is.null(samples)) {
      samples <- samples_i
    } else {
      samples <- rbind(samples, samples_i)
    }
  }
  
  crs(samples) <- crs(roi_filtered)
  
  # visualizacion
  # plot(roi_filtered, col = c("red", "green", "cyan", "blue"))
  # points(samples_1)
  # points(samples_2)
  # points(samples_3)
  # points(samples_4)
  # points(samples)
  samples <- st_as_sf(samples)
  samples$GVAL <- extract(roi_filtered, samples)
  
  return(samples)
}

in_path <-
  "~/Universidad de Córdoba/Proyectos - General/tfm_mc/mc_valencia_samples/roi/${file}.tif"
out_path <-
  "~/Universidad de Córdoba/Proyectos - General/tfm_mc/mc_valencia_samples/samples/${file}.shp"

for (i in 1:10) {
  print(paste("Imagen", i))
  roi_reclass <- reclass(str_interp(in_path, list(file = i)))
  samples <- extract_samples(roi_reclass)
  
  if (is.null(samples)) next
  
  print(paste("Sample para la imagen", i, "creado"))
  st_write(samples, str_interp(out_path, list(file = i)))
  print(paste("Samples para", i, "guardados"))
}

shapefin <- NULL
for (i in 1:10) {
  if (file.exists(str_interp(out_path, list(file = i)))) {
    back <- readOGR(str_interp(out_path, list(file = i)))
    if (is.null(shapefin)) {
      shapefin <- back 
    } else {
      shapefin <- bind(shapefin, back)
    }
  }
}
shapefin <- st_as_sf(shapefin)
st_write(shapefin, str_interp(out_path, list(file = "shapefinal.shp")))

# GlobalMapper


vector <-
  rasterToPolygons(raster, dissolve = TRUE)
merge_polygons <- function(raster) {
  # Pasar a vector
}
