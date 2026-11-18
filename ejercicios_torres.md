# Verificar entorno de trabajo

    ### Verificar versión de R
    R.version.string

    ## [1] "R version 4.5.1 (2025-06-13 ucrt)"

    ### Sistema operativo
    Sys.info()[["sysname"]]

    ## [1] "Windows"

    ### Verificar si las librerías están instaladas 
    library(ggplot2)

    ### Y las versiones de los paquetes
    packageVersion("dplyr")

    ## [1] '1.2.1'

# Ejercicios 3

## Ejercicio 3.1. Vectores y selección

    # Generar vectores 
    ids <- c("g1", "g2", "g3", "g4", "g5")
    tam <- c(90, 450, 310, 120, 600)

    # Selecciona el tercer identificador.
    ids[3]

    ## [1] "g3"

    # Recupera los identificadores de genes con longitud de al menos 300.
    ids[tam >= 300]

    ## [1] "g2" "g3" "g5"

    # Cuenta cuántos cumplen esa condición sin usar un ciclo.
    sum(tam >= 300)

    ## [1] 3

## Ejercicio 3.2

    # Generar vector 
    niveles <- c("A", "M", "A", "B", "M", "M", "B", "A", "B", "B")

    # A significa Alto, B bajo y M medio, convertir las entradas del vector a factores
    niveles <- factor(niveles,
                      levels = c("B", "M", "A"),
                      labels = c("Bajo", "Medio", "Alto"),
                      ordered = TRUE)
    niveles

    ##  [1] Alto  Medio Alto  Bajo  Medio Medio Bajo  Alto  Bajo  Bajo 
    ## Levels: Bajo < Medio < Alto

    levels(niveles)

    ## [1] "Bajo"  "Medio" "Alto"

## Ejercicio 3.3

    # Construye un data.frame con tres genes, sus longitudes y una etiqueta funcional.
    tabla <- data.frame(
      gen = c("g1", "g2", "g3"),
      longitud = c(123, 234, 345),
      funcion = c("TP53", "BRCA1", "EGFR")
    )

    # Recupera únicamente las filas con longitud mayor que 200.
    tabla[tabla$longitud > 200, ]

    ##   gen longitud funcion
    ## 2  g2      234   BRCA1
    ## 3  g3      345    EGFR

    # Crea una lista que contenga la tabla y el identificador de una muestra.
    resultado <- list(muestra = "M01", genes = tabla)
    resultado$genes

    ##   gen longitud funcion
    ## 1  g1      123    TP53
    ## 2  g2      234   BRCA1
    ## 3  g3      345    EGFR

    ## Explica por qué una matriz de caracteres no sería una buena sustitución para la 
    ## tabla cuando queremos conservar longitudes numéricas.
    # Una matriz no sería la mejor opción porque todos sus elementos deben ser del 
    # mismo tipo. Como tenemos nombres y longitudes numéricas, R convertiría todo a 
    # texto y ya no podríamos trabajar directamente con las longitudes como números.

# Ejercicios 4

## Ejercicio 4.1

    # Define una variable conteo que represente el número de lecturas asignadas a un gen. 
    # Escribe un condicional que imprima:
    #   "Dato faltante" si el valor es NA.
    #   "Conteo inválido" si es negativo.
    #   "Sin lecturas" si es cero.
    #   "Con lecturas" si es positivo.

    # Para NA
    conteo <- NA
    if (is.na(conteo)) {
      print("Dato faltante")
    } else if (conteo < 0) {
      print("Conteo inválido")
    } else if (conteo == 0) {
      print("Sin lecturas")
    } else {
      print("Con lecturas")
    }

    ## [1] "Dato faltante"

    # Para -2
    conteo <- -2
    if (is.na(conteo)) {
      print("Dato faltante")
    } else if (conteo < 0) {
      print("Conteo inválido")
    } else if (conteo == 0) {
      print("Sin lecturas")
    } else {
      print("Con lecturas")
    }

    ## [1] "Conteo inválido"

    # Para 0 
    conteo <- 0 
    if (is.na(conteo)) {
      print("Dato faltante")
    } else if (conteo < 0) {
      print("Conteo inválido")
    } else if (conteo == 0) {
      print("Sin lecturas")
    } else {
      print("Con lecturas")
    }

    ## [1] "Sin lecturas"

    # Para 15 
    conteo <- 15 
    if (is.na(conteo)) {
      print("Dato faltante")
    } else if (conteo < 0) {
      print("Conteo inválido")
    } else if (conteo == 0) {
      print("Sin lecturas")
    } else {
      print("Con lecturas")
    }

    ## [1] "Con lecturas"

## Ejercicio 4.2

    # Vector
    conteos <- c(8, 0, 13, 5, 0, 21)

    # Verificar si todos los conteos son no negativos
    all(conteos >= 0)

    ## [1] TRUE

    # Verificar si existe algún conteo mayor que 20
    any(conteos > 20)

    ## [1] TRUE

    # Obtener las posiciones de los conteos iguales a cero
    which(conteos == 0)

    ## [1] 2 5

    # Clasificar los conteos como Presente o Ausente
    estado <- ifelse(conteos > 0, "Presente", "Ausente")
    estado

    ## [1] "Presente" "Ausente"  "Presente" "Presente" "Ausente"  "Presente"

## Ejercicio 4.3

    # Vector
    secuencias <- c("ATG", "ATGCGT", "AATTCCGG", "CG")

    # Guardar las longitudes usando un ciclo for
    longitudes <- numeric(length(secuencias))

    for (i in seq_along(secuencias)) {
      longitudes[i] <- nchar(secuencias[i])
    }

    # Clasificar las secuencias según su longitud
    clasificacion <- ifelse(longitudes < 5, "Corta", "Larga")

    # Mostrar ambos vectores
    longitudes

    ## [1] 3 6 8 2

    clasificacion

    ## [1] "Corta" "Larga" "Larga" "Corta"

## Ejercicio 4.4

    # Crear una secuencia vacía
    secuencia <- ""

    # Agregar una "A" hasta alcanzar 8 nucleótidos
    while (nchar(secuencia) < 8) {
      secuencia <- paste0(secuencia, "A")
    }

    # Mostrar la secuencia final
    secuencia

    ## [1] "AAAAAAAA"

# Ejercicios 5

## Ejercicio 5.1

    # Crear la función
    empieza_con <- function(secuencia, prefijo) {
      inicio <- substr(secuencia, 1, nchar(prefijo))
      return(inicio == prefijo)
    }

    # Probar con el prefijo "ATG"
    empieza_con("ATGCGT", "ATG")

    ## [1] TRUE

    # Probar con el prefijo "TGA"
    empieza_con("ATGCGT", "TGA")

    ## [1] FALSE

## Ejercicio 5.2

    # Modificar la función resumir_adn
    resumir_adn <- function(id, secuencia) {
      secuencia <- toupper(secuencia)
      bases <- strsplit(secuencia, split = "", fixed = TRUE)[[1]]
      
      list(
        id = id,
        longitud = nchar(secuencia),
        conteo_gc = sum(bases %in% c("G", "C")),
        proporcion_gc = sum(bases %in% c("G", "C")) / nchar(secuencia)
      )
    }

    # Probar la función con una secuencia no vacía
    resultado <- resumir_adn("GEN_01", "ATGCGTAA")

    # Mostrar los resultados
    resultado

    ## $id
    ## [1] "GEN_01"
    ## 
    ## $longitud
    ## [1] 8
    ## 
    ## $conteo_gc
    ## [1] 3
    ## 
    ## $proporcion_gc
    ## [1] 0.375

## Ejercico 5.3

    # Crear la función para revisar las secuencias
    revisar_secuencias <- function(secuencias) {
      resultado <- logical(length(secuencias))
      
      for (i in seq_along(secuencias)) {
        secuencia <- secuencias[i]
        
        if (is.na(secuencia)) {
          resultado[i] <- FALSE
        } else {
          resultado[i] <- grepl("^[ACGT]+$", toupper(secuencia))
        }
      }
      
      return(resultado)
    }

    # Crear la función para clasificar las secuencias
    clasificar_secuencias <- function(secuencias) {
      resultado <- revisar_secuencias(secuencias)
      
      list(
        validas = secuencias[resultado],
        invalidas = secuencias[!resultado]
      )
    }

    # Probar la función
    secuencias <- c("ATGCGT", "ACNTGA", "ttacga", NA)

    # Mostrar los resultados
    clasificar_secuencias(secuencias)

    ## $validas
    ## [1] "ATGCGT" "ttacga"
    ## 
    ## $invalidas
    ## [1] "ACNTGA" NA

## Ejercicio 5.4

    # Crear los vectores
    ids <- c("seq_01", "seq_02", "seq_03", "seq_04")
    secuencias <- c("ATGCGT", "ACNTGA", "GGCCAA", "TTTAAA")

    # Crear la función para revisar las secuencias
    revisar_secuencias <- function(secuencias) {
      resultado <- logical(length(secuencias))
      
      for (i in seq_along(secuencias)) {
        secuencia <- secuencias[i]
        
        if (is.na(secuencia)) {
          resultado[i] <- FALSE
        } else {
          resultado[i] <- grepl("^[ACGT]+$", toupper(secuencia))
        }
      }
      
      return(resultado)
    }

    # Crear la función para generar el reporte
    crear_reporte_secuencias <- function(ids, secuencias) {
      
      # Comprobar que los argumentos sean correctos
      if (!is.character(ids) || !is.character(secuencias) ||
          length(ids) != length(secuencias)) {
        stop("Los argumentos deben ser vectores de caracteres de igual longitud")
      }
      
      # Identificar las secuencias válidas
      valida <- revisar_secuencias(secuencias)
      
      # Calcular las longitudes
      longitud <- nchar(secuencias)
      
      # Asignar NA a las longitudes inválidas
      longitud[!valida] <- NA
      
      # Hacer el data frame
      reporte <- data.frame(
        id = ids,
        secuencia = secuencias,
        longitud = longitud,
        valida = valida
      )
      
      return(reporte)
    }

    # Probar la función
    resultado <- crear_reporte_secuencias(ids, secuencias)

    # Mostrar el resultado
    resultado

    ##       id secuencia longitud valida
    ## 1 seq_01    ATGCGT        6   TRUE
    ## 2 seq_02    ACNTGA       NA  FALSE
    ## 3 seq_03    GGCCAA        6   TRUE
    ## 4 seq_04    TTTAAA        6   TRUE

# Ejercicios 6

## Ejercicio 6.1

    # Definir los datos de entrada
    ids <- c("seq_01", "seq_02", "seq_03", "seq_04")
    secuencias <- c("ATGCGT", "ACNTGA", "GGCCAA", "TTTAAA")

    # Verificar que ambos vectores tengan la misma longitud
    stopifnot(length(ids) == length(secuencias))

    # Calcular las longitudes de las secuencias
    longitudes <- nchar(secuencias)

    # Crear la tabla de resultados
    resultado <- data.frame(
      id = ids,
      longitud = longitudes
    )

    # Mostrar el resultado
    print(resultado)

    ##       id longitud
    ## 1 seq_01        6
    ## 2 seq_02        6
    ## 3 seq_03        6
    ## 4 seq_04        6

# Ejercicios 7

## Ejercicio 7.1

    # Definir la ruta del archivo
    archivo <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/blast_separador_incorrecto.csv"

    # Comprobar que el archivo existe
    file.exists(archivo)

    ## [1] TRUE

    # Mostrar las primeras tres líneas
    readLines(archivo, n = 3)

    ## [1] "query_id\tsubject_id\tpident\tlength\tevalue\tbitscore"
    ## [2] "seq01\tref_001\t87.9\t216\t1.00e-40\t118.7"            
    ## [3] "seq01\tref_002\t68.2\t245\t1.00e-36\t184.6"

    # Comprobar el separador real
    datos <- read.delim(archivo)

    # Mostrar los nombres de las columnas
    names(datos)

    ## [1] "query_id"   "subject_id" "pident"     "length"     "evalue"    
    ## [6] "bitscore"

    # El archivo utiliza tabulaciones (\t) como separador, aunque tenga extensión
    #.csv. Por ejemplo "query_id\tsubject_id\tpident\tlength\tevalue\tbitscore"

## Ejercicio 7.2

    # Cargar la librería readr
    library(readr)

    ## Warning: package 'readr' was built under R version 4.5.3

    # Definir las rutas de los archivos
    archivo_metadata <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/metadata_secuencias.csv"
    archivo_blast <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/blast_comentarios.tsv"

    # Importar metadata conservando codigo_muestra como texto
    metadata <- readr::read_csv(
      archivo_metadata,
      col_types = readr::cols(codigo_muestra = readr::col_character()),
      show_col_types = FALSE
    )

    # Importar BLAST ignorando las líneas de comentarios
    blast <- readr::read_tsv(
      archivo_blast,
      comment = "#",
      show_col_types = FALSE
    )

    # Comprobar la clase de cada objeto
    class(metadata)

    ## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

    class(blast)

    ## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"

    # Mostrar las primeras filas de cada tabla
    head(metadata)

    ## # A tibble: 6 × 4
    ##   query_id organismo  origen codigo_muestra
    ##   <chr>    <chr>      <chr>  <chr>         
    ## 1 seq01    Bacteria_A suelo  001           
    ## 2 seq02    Bacteria_B agua   002           
    ## 3 seq03    Bacteria_C suelo  003           
    ## 4 seq04    Bacteria_A agua   004           
    ## 5 seq05    Bacteria_B suelo  005           
    ## 6 seq06    Bacteria_C agua   006

    head(blast)

    ## # A tibble: 6 × 6
    ##   query_id subject_id pident length evalue bitscore
    ##   <chr>    <chr>       <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 seq01    ref_001      87.9    216  1e-40     119.
    ## 2 seq01    ref_002      68.2    245  1e-36     185.
    ## 3 seq01    ref_003      93.5    168  1e-35     386.
    ## 4 seq02    ref_004      82.6    111  1e-85     260.
    ## 5 seq02    ref_005      68.7    292  1e-57     323.
    ## 6 seq03    ref_007      88.2    254  1e- 5     362.

## Ejercicio 7.3

    # Importar el archivo de resultados BLAST
    library(readr)

    blast <- readr::read_tsv(
      "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/resultados_blast.tsv",
      show_col_types = FALSE
    )

    # ¿Cuántos registros y columnas tiene la tabla?
    dim(blast)

    ## [1] 20  6

    # La tabla tiene 20 registros y 6 columnas.

    # ¿Cuántos query_id diferentes aparecen?
    length(unique(blast$query_id))

    ## [1] 8

    # Aparecen 8 identificadores diferentes.

    # ¿Cuántos resultados tiene cada consulta?
    table(blast$query_id)

    ## 
    ## seq01 seq02 seq03 seq04 seq05 seq06 seq07 seq08 
    ##     3     2     4     1     3     2     3     2

    # seq01: 3, seq02: 2, seq03: 4, seq04: 1, seq05: 3, seq06: 2, seq07: 3 y seq08: 2.

    # ¿Qué filas tienen pident mayor o igual a 95?
    blast[blast$pident >= 95, ]

    ## # A tibble: 3 × 6
    ##   query_id subject_id pident length evalue bitscore
    ##   <chr>    <chr>       <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 seq03    ref_009      97.8    191  1e-71     97.9
    ## 2 seq03    ref_010      99      229  1e- 8    336. 
    ## 3 seq07    ref_019      95.7    106  1e-70    138.

    # Son 3 filas: dos de seq03 (97.8% y 99%) y una de seq07 (95.7%).

    # ¿Hay valores faltantes?
    anyNA(blast)

    ## [1] FALSE

    # No

    # Mostrar cuántos valores faltantes hay por columna
    colSums(is.na(blast))

    ##   query_id subject_id     pident     length     evalue   bitscore 
    ##          0          0          0          0          0          0

    # Ninguno

## Ejercicio 7.4

    # Cargar la librería readr
    library(readr)

    # Definir la ruta del archivo
    archivo <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/blast_separador_incorrecto.csv"

    # Importar incorrectamente con read_csv()
    mal <- readr::read_csv(archivo, show_col_types = FALSE)

    # Comprobar los nombres de las columnas
    names(mal)

    ## [1] "query_id\tsubject_id\tpident\tlength\tevalue\tbitscore"

    # Revisar las primeras líneas del archivo
    readLines(archivo, n = 3)

    ## [1] "query_id\tsubject_id\tpident\tlength\tevalue\tbitscore"
    ## [2] "seq01\tref_001\t87.9\t216\t1.00e-40\t118.7"            
    ## [3] "seq01\tref_002\t68.2\t245\t1.00e-36\t184.6"

    # Corregir la importación usando read_tsv()
    bien <- readr::read_tsv(archivo, show_col_types = FALSE)

    # Comprobar las columnas correctamente importadas
    names(bien)

    ## [1] "query_id"   "subject_id" "pident"     "length"     "evalue"    
    ## [6] "bitscore"

    # Mostrar las primeras filas
    head(bien)

    ## # A tibble: 6 × 6
    ##   query_id subject_id pident length evalue bitscore
    ##   <chr>    <chr>       <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 seq01    ref_001      87.9    216  1e-40     119.
    ## 2 seq01    ref_002      68.2    245  1e-36     185.
    ## 3 seq01    ref_003      93.5    168  1e-35     386.
    ## 4 seq02    ref_004      82.6    111  1e-85     260.
    ## 5 seq02    ref_005      68.7    292  1e-57     323.
    ## 6 seq03    ref_007      88.2    254  1e- 5     362.

    # ¿Significa que el archivo contiene una única variable? Explica cómo comprobarlo
    # y corrige la importación

    # No significa que el archivo tenga una sola variable, sino que se utilizó un separador 
    # incorrecto. Podemos comprobarlo con readLines(), donde se observa que los datos están 
    # separados por tabulaciones. Para corregirlo usamos read_tsv() en lugar de 
    # read_csv(), y así obtenemos las seis columnas originales.

## Ejercicio 7.5

    # Definir la ruta del archivo FASTA
    archivo <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/secuencias.fasta"

    # Leer las líneas del archivo
    lineas <- readLines(archivo)

    # Identificar únicamente los encabezados
    encabezados <- grep("^>", lineas, value = TRUE)

    # Contar cuántas entradas FASTA contiene
    length(encabezados)

    ## [1] 8

    # Mostrar únicamente los encabezados
    encabezados

    ## [1] ">seq01 secuencia_sintetica" ">seq02 secuencia_sintetica"
    ## [3] ">seq03 secuencia_sintetica" ">seq04 secuencia_sintetica"
    ## [5] ">seq05 secuencia_sintetica" ">seq06 secuencia_sintetica"
    ## [7] ">seq07 secuencia_sintetica" ">seq08 secuencia_sintetica"

    # El archivo contiene 8 entradas FASTA

## Ejercicio 7.6

    # Exploración de datos bioinformáticos

    # Cargar la librería
    library(readr)

    # Definir las rutas de los archivos
    archivo_blast <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/resultados_blast.tsv"
    archivo_metadata <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/metadata_secuencias.csv"
    archivo_fasta <- "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/secuencias.fasta"

    # Comprobar que los tres archivos existen
    file.exists(archivo_blast)

    ## [1] TRUE

    file.exists(archivo_metadata)

    ## [1] TRUE

    file.exists(archivo_fasta)

    ## [1] TRUE

    # Importar las tablas con readr
    blast <- readr::read_tsv(
      archivo_blast,
      show_col_types = FALSE
    )

    metadata <- readr::read_csv(
      archivo_metadata,
      col_types = readr::cols(codigo_muestra = readr::col_character()),
      show_col_types = FALSE
    )

    # Mostrar dimensiones, nombres y tipos de columnas
    dim(blast)

    ## [1] 20  6

    dim(metadata)

    ## [1] 8 4

    names(blast)

    ## [1] "query_id"   "subject_id" "pident"     "length"     "evalue"    
    ## [6] "bitscore"

    names(metadata)

    ## [1] "query_id"       "organismo"      "origen"         "codigo_muestra"

    str(blast)

    ## spc_tbl_ [20 × 6] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ query_id  : chr [1:20] "seq01" "seq01" "seq01" "seq02" ...
    ##  $ subject_id: chr [1:20] "ref_001" "ref_002" "ref_003" "ref_004" ...
    ##  $ pident    : num [1:20] 87.9 68.2 93.5 82.6 68.7 88.2 93.6 97.8 99 83.9 ...
    ##  $ length    : num [1:20] 216 245 168 111 292 254 319 191 229 162 ...
    ##  $ evalue    : num [1:20] 1e-40 1e-36 1e-35 1e-85 1e-57 ...
    ##  $ bitscore  : num [1:20] 119 185 386 260 323 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   query_id = col_character(),
    ##   ..   subject_id = col_character(),
    ##   ..   pident = col_double(),
    ##   ..   length = col_double(),
    ##   ..   evalue = col_double(),
    ##   ..   bitscore = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

    str(metadata)

    ## spc_tbl_ [8 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ query_id      : chr [1:8] "seq01" "seq02" "seq03" "seq04" ...
    ##  $ organismo     : chr [1:8] "Bacteria_A" "Bacteria_B" "Bacteria_C" "Bacteria_A" ...
    ##  $ origen        : chr [1:8] "suelo" "agua" "suelo" "agua" ...
    ##  $ codigo_muestra: chr [1:8] "001" "002" "003" "004" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   query_id = col_character(),
    ##   ..   organismo = col_character(),
    ##   ..   origen = col_character(),
    ##   ..   codigo_muestra = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

    # Identificar los query_id diferentes
    unique(blast$query_id)

    ## [1] "seq01" "seq02" "seq03" "seq04" "seq05" "seq06" "seq07" "seq08"

    # Contar cuántos query_id distintos hay
    length(unique(blast$query_id))

    ## [1] 8

    # Contar los resultados por consulta
    table(blast$query_id)

    ## 
    ## seq01 seq02 seq03 seq04 seq05 seq06 seq07 seq08 
    ##     3     2     4     1     3     2     3     2

    # Mostrar las filas con pident mayor o igual a 90
    blast[blast$pident >= 90, ]

    ## # A tibble: 5 × 6
    ##   query_id subject_id pident length evalue bitscore
    ##   <chr>    <chr>       <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 seq01    ref_003      93.5    168  1e-35    386. 
    ## 2 seq03    ref_008      93.6    319  1e-36     96.6
    ## 3 seq03    ref_009      97.8    191  1e-71     97.9
    ## 4 seq03    ref_010      99      229  1e- 8    336. 
    ## 5 seq07    ref_019      95.7    106  1e-70    138.

    # Este filtro muestra los alineamientos que tienen
    # un porcentaje de identidad de al menos 90%.

    # Leer el archivo FASTA
    lineas <- readLines(archivo_fasta)

    # Identificar los encabezados
    encabezados <- grep("^>", lineas, value = TRUE)

    # Contar los encabezados
    length(encabezados)

    ## [1] 8

    # Mostrar los encabezados
    encabezados

    ## [1] ">seq01 secuencia_sintetica" ">seq02 secuencia_sintetica"
    ## [3] ">seq03 secuencia_sintetica" ">seq04 secuencia_sintetica"
    ## [5] ">seq05 secuencia_sintetica" ">seq06 secuencia_sintetica"
    ## [7] ">seq07 secuencia_sintetica" ">seq08 secuencia_sintetica"

    # Comprobar si hay valores faltantes
    anyNA(blast)

    ## [1] FALSE

    anyNA(metadata)

    ## [1] FALSE

    # Contar los valores faltantes por columna
    colSums(is.na(blast))

    ##   query_id subject_id     pident     length     evalue   bitscore 
    ##          0          0          0          0          0          0

    colSums(is.na(metadata))

    ##       query_id      organismo         origen codigo_muestra 
    ##              0              0              0              0

    ### Tres observaciones sobre los datos ###

    # 1. La tabla BLAST tiene 20 registros y 6 columnas, contiene 8 identificadores 
    # query_id diferentes.

    # 2. No todas las consultas tienen la misma cantidad de resultados. Algunas 
    # aparecen más veces que otras.

    # 3. El archivo FASTA contiene 8 encabezados y la tabla BLAST no presenta 
    # valores faltantes.

# Ejercicios 8

## Ejercicio 8.1

    library(readr)
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 4.5.3

    ## 
    ## Adjuntando el paquete: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    datos <- read_csv(
      "C:/Users/torre/OneDrive/Documents/Rpracticas/datos_sesion_02_blast/expresion_genica_estres.csv",
      show_col_types = FALSE
    )

    glimpse(datos)

    ## Rows: 240
    ## Columns: 8
    ## $ sample_id     <chr> "CTL_01", "CTL_02", "CTL_03", "EST_01", "EST_02", "EST_0…
    ## $ condicion     <chr> "Control", "Control", "Control", "Estres", "Estres", "Es…
    ## $ replica       <dbl> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1,…
    ## $ gene_id       <chr> "gene_001", "gene_001", "gene_001", "gene_001", "gene_00…
    ## $ funcion       <chr> "Función desconocida", "Función desconocida", "Función d…
    ## $ longitud_pb   <dbl> 1803, 1803, 1803, 1803, 1803, 1803, 891, 891, 891, 891, …
    ## $ gc_porcentaje <dbl> 58.1, 58.1, 58.1, 58.1, 58.1, 58.1, 43.2, 43.2, 43.2, 43…
    ## $ conteo        <dbl> 1302, 1263, 1368, 892, 953, 1106, 639, 998, 1108, 733, 9…

    #Primero calcular el promedio por gene_id y condicion
    promedios <- datos %>% #datos es la tabla original y %>% la pasa a la siguiente función
      group_by(gene_id, condicion) %>% #group_by agrupa los datos por gene y condición y %>% los pasa a la siguiente función
      summarise(promedio = mean(conteo), .groups = "drop") #calcula el promedio de cada grupo

    promedios

    ## # A tibble: 80 × 3
    ##    gene_id  condicion promedio
    ##    <chr>    <chr>        <dbl>
    ##  1 gene_001 Control      1311 
    ##  2 gene_001 Estres        984.
    ##  3 gene_002 Control       915 
    ##  4 gene_002 Estres        907.
    ##  5 gene_003 Control       927.
    ##  6 gene_003 Estres       1041.
    ##  7 gene_004 Control      1260.
    ##  8 gene_004 Estres       1799.
    ##  9 gene_005 Control       108.
    ## 10 gene_005 Estres        167 
    ## # ℹ 70 more rows

    #Después vamos a organizar los promedios para compararlos
    library(tidyr)

    ## Warning: package 'tidyr' was built under R version 4.5.3

    comparacion <- promedios %>% #toma los promedios que nos dio antes y los pasa a la siguiente función
      pivot_wider( #convertir filas en columnas 
        names_from = condicion, #Toma los valores de la columna condicion y los convierte en nombres de nuevas columnas
        values_from = promedio #«Toma los números de la columna promedio y colócalos en las nuevas columnas correspondientes».
      )

    comparacion

    ## # A tibble: 40 × 3
    ##    gene_id  Control Estres
    ##    <chr>      <dbl>  <dbl>
    ##  1 gene_001   1311    984.
    ##  2 gene_002    915    907.
    ##  3 gene_003    927.  1041.
    ##  4 gene_004   1260.  1799.
    ##  5 gene_005    108.   167 
    ##  6 gene_006    946.  1956 
    ##  7 gene_007    751.  2160.
    ##  8 gene_008    956.  4140 
    ##  9 gene_009    179.  1162.
    ## 10 gene_010   1124.   497 
    ## # ℹ 30 more rows

    #Calcular la razón entre el promedio en Estrés y el promedio en Control
    comparacion <- comparacion %>%
      mutate(razon = Estres / Control)

    comparacion

    ## # A tibble: 40 × 4
    ##    gene_id  Control Estres razon
    ##    <chr>      <dbl>  <dbl> <dbl>
    ##  1 gene_001   1311    984. 0.750
    ##  2 gene_002    915    907. 0.992
    ##  3 gene_003    927.  1041. 1.12 
    ##  4 gene_004   1260.  1799. 1.43 
    ##  5 gene_005    108.   167  1.54 
    ##  6 gene_006    946.  1956  2.07 
    ##  7 gene_007    751.  2160. 2.88 
    ##  8 gene_008    956.  4140  4.33 
    ##  9 gene_009    179.  1162. 6.50 
    ## 10 gene_010   1124.   497  0.442
    ## # ℹ 30 more rows

    #Identificar los cinco genes con mayor aumento relativo y muestra su anotación 
    #funcional.
    top5 <- comparacion %>%
      filter(is.finite(razon)) %>% #is.finite(razon) comprueba si cada valor de la columna razon es un número finito.
      arrange(desc(razon)) %>% #arrange() ordena las filas y desc() indica que queremos ordenarlas de mayor a menor.
      slice_head(n = 5) %>% #selecciona las primeras filas de una tabla (n = 5 = 5 filas)
      left_join( #sirve para que juntemos los datos de las tablas
        datos %>% distinct(gene_id, funcion), # Buscamos la funcion de cada gen sin repetir
        by = "gene_id" # Unimos las tablas usando el identificador del gen
      )

    top5

    ## # A tibble: 5 × 5
    ##   gene_id  Control Estres razon funcion                 
    ##   <chr>      <dbl>  <dbl> <dbl> <chr>                   
    ## 1 gene_009    179.  1162.  6.50 Transporte de membrana  
    ## 2 gene_029    237.  1318.  5.57 Metabolismo energético  
    ## 3 gene_039    312   1717.  5.50 Metabolismo energético  
    ## 4 gene_028    452   2127.  4.71 Replicación y reparación
    ## 5 gene_019    868.  4020.  4.63 Transporte de membrana

    # ¿Un gen con un aumento relativo grande necesariamente presenta un conteo alto de lecturas?

    # No necesariamente porque el aumento relativo compara el conteo promedio en estrés con el
    # de control. Un gen puede tener un aumento relativo muy grande aunque sus conteos sean bajos, 
    # siempre que en Control tenga un valor mucho menor.

## Ejercicio 8.2

    # Calcular el conteo promedio de cada gen considerando todas las muestras

    promedios_genes <- datos %>%  # Tomamos la tabla original y pasamos los datos a la siguiente función
      group_by(gene_id, funcion) %>%  # Agrupamos los datos por gen y por su función
      summarise(promedio = mean(conteo), .groups = "drop") # Calculamos el promedio de cada gen y quitamos la agrupación

    promedios_genes

    ## # A tibble: 40 × 3
    ##    gene_id  funcion                    promedio
    ##    <chr>    <chr>                         <dbl>
    ##  1 gene_001 Función desconocida           1147.
    ##  2 gene_002 Metabolismo de aminoácidos     911.
    ##  3 gene_003 Síntesis de proteínas          984 
    ##  4 gene_004 Síntesis de proteínas         1530.
    ##  5 gene_005 Regulación transcripcional     138.
    ##  6 gene_006 Replicación y reparación      1451.
    ##  7 gene_007 Metabolismo energético        1456.
    ##  8 gene_008 Metabolismo energético        2548.
    ##  9 gene_009 Transporte de membrana         670.
    ## 10 gene_010 Respuesta al estrés            811.
    ## # ℹ 30 more rows

    # Identificar los 15 genes con mayor conteo promedio
    top15 <- promedios_genes %>%  # Tomamos la tabla con los promedios
      arrange(desc(promedio)) %>%  # Ordenamos los genes de mayor a menor promedio
      slice_head(n = 15)  # Seleccionamos únicamente las primeras 15 filas

    top15

    ## # A tibble: 15 × 3
    ##    gene_id  funcion                    promedio
    ##    <chr>    <chr>                         <dbl>
    ##  1 gene_018 Metabolismo energético        2990.
    ##  2 gene_008 Metabolismo energético        2548.
    ##  3 gene_019 Transporte de membrana        2444 
    ##  4 gene_017 Replicación y reparación      1807.
    ##  5 gene_026 Regulación transcripcional    1669.
    ##  6 gene_036 Síntesis de proteínas         1585.
    ##  7 gene_004 Síntesis de proteínas         1530.
    ##  8 gene_023 Metabolismo de aminoácidos    1463.
    ##  9 gene_007 Metabolismo energético        1456.
    ## 10 gene_006 Replicación y reparación      1451.
    ## 11 gene_024 Metabolismo de aminoácidos    1445.
    ## 12 gene_015 Regulación transcripcional    1410.
    ## 13 gene_028 Replicación y reparación      1290.
    ## 14 gene_032 Respuesta al estrés           1240.
    ## 15 gene_001 Función desconocida           1147.

    # Determinar cuántos genes pertenecen a cada categoría funcional
    frecuencias <- top15 %>%  # Tomamos los 15 genes seleccionados
      group_by(funcion) %>%  # Agrupamos los genes según su categoría funcional
      summarise(cantidad = n(), .groups = "drop") # Contamos cuántos genes hay en cada categoría y quitamos la agrupación

    frecuencias

    ## # A tibble: 8 × 2
    ##   funcion                    cantidad
    ##   <chr>                         <int>
    ## 1 Función desconocida               1
    ## 2 Metabolismo de aminoácidos        2
    ## 3 Metabolismo energético            3
    ## 4 Regulación transcripcional        2
    ## 5 Replicación y reparación          3
    ## 6 Respuesta al estrés               1
    ## 7 Síntesis de proteínas             2
    ## 8 Transporte de membrana            1

    # Ordenar las categorías funcionales de mayor a menor frecuencia
    frecuencias <- frecuencias %>%  # Tomamos la tabla con las frecuencias
      arrange(desc(cantidad))  # Ordenamos las categorías de la más frecuente a la menos frecuente

    frecuencias

    ## # A tibble: 8 × 2
    ##   funcion                    cantidad
    ##   <chr>                         <int>
    ## 1 Metabolismo energético            3
    ## 2 Replicación y reparación          3
    ## 3 Metabolismo de aminoácidos        2
    ## 4 Regulación transcripcional        2
    ## 5 Síntesis de proteínas             2
    ## 6 Función desconocida               1
    ## 7 Respuesta al estrés               1
    ## 8 Transporte de membrana            1

    # ¿La categoría funcional más frecuente entre estos 15 genes necesariamente es la
    # más importante para la bacteria?

    # No necesariamente porque que una categoría tenga más genes entre los 15 con mayor
    # conteo promedio no significa que sea la más importante. Puede haber otras funciones
    # representadas por menos genes que sean esenciales para la bacteria. Además, el 
    # conteo de lecturas no nos dice directamente qué tan importante es una función biológica.
