# Riesgo de Impago – Proyecto de Clasificación

Proyecto de análisis y modelado de riesgo de impago de clientes en una cooperativa financiera, desarrollado durante el curso de Minería de Datos y Aprendizaje Automático del programa de Estadística de la Universidad del Valle.  


**Resumen**

Se desarrolló un modelo de clasificación para anticipar el riesgo de impago de clientes de una cooperativa financiera, a partir de datos históricos sociodemográficos y de comportamiento de pago. En primer lugar, se llevó a cabo un análisis exploratorio para identificar variables clave y obtener insights preliminares sobre las tendencias de clientes con riesgo de impago frente a aquellos sin riesgo. A continuación, se entrenaron cuatro modelos (Random Forest, regresión logística, árbol de decisión y XGBoost) mediante validación cruzada para evaluar su capacidad predictiva y ajustar sus hiper-parámetros. Finalmente se comparo su desempeño sobre datos de prueba no vistos, dejando como mejor candidato al modelo de regresión logística por su capacidad para minimizar los falsos negativos, lo que reduce la probabilidad de otorgar crédito a clientes con riesgo de impago y de esta manera disminuir las pérdidas.

---

## Objetivos

- Identificar tendencias en las características de clientes con riesgo de impago frente
a aquellos sin riesgo.
- Comprender cuáles son las variables más relevantes para la decisión de otorgar un crédito.
- Entrenar un modelo de clasificación capaz de detectar clientes con riesgo de impago
para optimizar el proceso de aprobación de crédito.

---

## Tecnologías utilizadas

- `R` como lenguaje de programación
- `R-Studio` como entorno de desarrollo 

---

##  Descripción de archivos

- `Data_Clientes_Cooperativa.txt`: Datos crudos (4 117 observaciones y 11 variables).  
- `Script_Riesgo_Clientes.R`: Script en R con todo el flujo de trabajo (preprocesamiento, análisis exploratorio, modelado y evaluación).  
- `Reporte_Riesgo_de_impago.pdf`: Informe completo con los resultados.  
