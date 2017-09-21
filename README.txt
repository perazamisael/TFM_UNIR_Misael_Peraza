####################### README #############################


En primer lugar se han de descargar manualmente las imágenes desde la página web de GONG almacenándose en /SoSoft/TESTDATA.

Desde el terminal de Linux:
  
					>wget -r ftp://gong2.nso.edu/HA/haf/201103/20110301/

Con esto se descargarán sólo las imágenes con esa fecha, pero si nos interesa se puede descagar cada mes de cada año de la siguiente forma:

					>wget -r ftp://gong2.nso.edu/HA/haf/

(Siempre asegurarse que se descargan suficientes imágenes con nombre *Lh ya que este software solo funciona para imágenes captadas por ese telescopio).

Si no es posible descargar las imágenes, en /Images_Examples hay varias imágenes de ejemplo que se pueden tomar de prueba, siempre que se copien
al directorio /SoSoft/TESTDATA.

Si se ha elegido la opción de descargar las imágenes el siguiente paso es descomprimir dichas imágenes. Para ello se vuelve hacer uso de un comando
en la terminal de Linux:

					>for file in `ls *fz`; do echo $file ; funpack $file; done

Teniendo las imágenes descargadas descomprimidas hay que asegurarse que estén situadas en el directorio /SoSoft/TESTDATA.

A continuación hay que situarse en /TrackFil y lanzar estas órdenes:

					>export TRACKFIL_HOME_DIR=/TrackFil
					>./ssw scripts/trackfil_make_bin_angel.pro 

					>cd /SoSoft/FIL/FINALSSW
					>ssw
					IDL>restore,'trackfil.sav',/VERBOSE

Tras realizar estos pasos previos e iniciar IDL, es el momento de desplegar el software. En primer lugar, se detectan las protuberancias solares
de las imágenes descargadas y descomprimidas anteriormente:

					IDL>.r SoSoft.pro

Se puede observar el array de estructuras que contiene las características de los filamentos detectados a través de:

					IDL>globalstrfil

Luego, se le pasará como argumento de entrada globalstrfil a TrackFil para que éste haga su función:

					IDL>trackfil,globalstrfil,results,config_file='trackfil_test.config'

Para observar el nuevo array de estructuras  que ha generado TrackFil bastaría con llamarlo:

					IDL>results

Otra opción que nos sugiere TrackFil es poder imprimir en forma de lista el "TRACK_ID" de cada uno de los filamentos detectados por SoSoft:

					IDL>results.track_id
					
Finalmente, se tiene la opción de ejecutar ExOs:

					IDL>.r ExOs.pro

que nos desplegará el cálculo del "centro de masas" para el sistema de filamentos con el mismo "TRACK_ID" en cada imagen. 


