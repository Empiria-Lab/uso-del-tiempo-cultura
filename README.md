# uso-del-tiempo-cultura
**Estudio análisis de uso del tiempo y relación con perfiles de participación cultural**

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/active.svg)](STATUS.md) [![License](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/mit.svg)](LICENSE-MIT.md) [![License](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/cc_by_4_0.svg)](LICENSE-CC.md) [![Empiria](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/empirialab.svg)](https://empirialab.cl/)

## Resumen

Este repositorio es parte del "Estudio de análisis de uso del tiempo y relación con perfiles de participación cultural", contratado por el **Departamento de Estudios de la Subsecretaría de las Culturas y las Artes**. El objetivo general del estudio es analizar y contextualizar la Encuesta Nacional de Uso del Tiempo (II ENUT 2023) en relación con la Encuesta Nacional de Participación Cultural y Comportamiento Lector (ENPCCL 2024), con el fin de caracterizar perfiles de uso del tiempo de distintos grupos de interés para las políticas culturales.

Para asegurar la reproducibilidad del estudio, todas las transformaciones y análisis se han documentado en este repositorio que contiene los scripts completos en R, la estructura de carpetas, los archivos de configuración y las especificaciones detalladas del diseño del estudio. Los perfiles y matrices analíticas se han almacenado en formato columnar Parquet, lo que facilita su lectura eficiente en las distintas etapas del flujo de trabajo.

## Metadatos y preservación

Este repositorio se ha creado con control de versiones Git. Además, el entorno de trabajo en R se ha gestionado con `renv`, lo que permite congelar y restaurar las versiones de los paquetes utilizados. Paralelamente, se ha generado un registro sistemático de dependencias y referencias bibliográficas asociadas a los paquetes empleados utilizando `grateful`, fortaleciendo la transparencia del ecosistema de software.

A continuación se ofrece una breve descripción de los archivos relevantes de este repositorio:

## Cómo empezar

El código original se escribió entre noviembre y diciembre de 2025 utilizando `R v4.5.0 -- How About a Twenty-Six`. Es importante señalar que `renv.lock` es el archivo de bloqueo del proyecto, y que todos los paquetes R instalados y necesarios se registran allí.

Para replicar los principales resultados del artículo, deben ejecutarse los siguientes archivos:

## Licencia

El contenido de este proyecto está protegido por una licencia [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE-CC.md), y el código subyacente utilizado para dar formato y mostrar dicho contenido está protegido por una [licencia MIT](LICENSE-MIT.md).

## Autor

**Dr. Bastián González-Bustamante.** Coordinador de Estudios de [Empiria Lab](https://empirialab.cl/) y Profesor Asociado de la Facultad de Administración y Economía de la Universidad Diego Portales. También es investigador postdoctoral en Ciencias Sociales Computacionales en la Universidad de Leiden y Coordinador de Investigación del grupo de Ciencia Política Computacional de la Universidad de Oxford. Es Doctor en Ciencia Política por la Universidad de Oxford y Magíster en Ciencia Política, Administrador Público y Licenciado en Ciencias Políticas y Gubernamentales por la Universidad de Chile. Email: bastian.gonzalezbustamante@empirialab.cl. 

## Empiria Lab

[Empiria Lab](https://empirialab.cl/) es un equipo especializado que ofrece soluciones y asesorías para los ámbitos público y privado. [Empiria Lab](https://empirialab.cl/) nació como una idea en Oxford y, en 2024, se convirtió en un grupo de consultoría, con sede en Santiago y colaboradores en distintas partes del mundo. Esperamos que nuestros clientes disfruten de nuestros productos y servicios tanto como nosotros disfrutamos ofreciéndolos.

### Última revisión

8 de diciembre de 2025.