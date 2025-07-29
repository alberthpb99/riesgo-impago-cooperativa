#------------------------------------------------------#
# 0. Configuración inicial-Librerías requeridas     ####                   
#------------------------------------------------------#
wd="/Users/alberth/Documents/Semestre\ 2025-1/minería"    # Ruta al Directorio de trabajo
setwd(wd)       

install.packages("easypackages")        
library("easypackages")

lib_req <- c("MASS","visdat","corrplot","plotrix","doBy","FactoMineR","factoextra","caret","e1071","pROC","class","rpart","rpart.plot","randomForest")
easypackages::packages(lib_req)         # carga de librerias.

#------------------------------------------------------------------------------#
# 1. Lectura y transformación de Datos                                      ####
#------------------------------------------------------------------------------#

df = read.table('Data_Clientes_Cooperativa.txt',header=T,na.strings=c(" ",""))
str(df)
visdat::vis_miss(df)    # visualizamos faltantes
colSums(is.na(df))      # numero de faltantes por columna (solo 1 en la variable Genero)
idx_faltante = which(is.na(df$GENERO))  # indice de valor faltante 
df[idx_faltante,]       # El faltante pertenece a la clase RIESGO = F

# Calculamos la moda del genero de la clase RIESGO = F
moda = names(which.max(table(df[df$RIESGO == 'F','GENERO'])))
df[idx_faltante,'GENERO'] <- moda    # Imputamos el valor faltante con la moda

# Columnas a convertir en factores
cols = c('GENERO','ESTADO_CIVIL','MODALIDAD_PAGO','HIPOTECA')
df[cols] = lapply(df[cols],factor)

df$RIESGO <- factor(df$RIESGO,levels = c("V","F"),labels = c("Yes","No"))

str(df) # Verificamos que ahora el formato es adecuado

#------------------------------------------------------------------------------#
# 2. Análisis exploratorio                                                  ####
#------------------------------------------------------------------------------#

#Graficamos las distribuciones de las diferentes variables

par(mfrow = c(1, 3))
tbl <- prop.table(table(df$GENERO, df$RIESGO), margin = 2)
barplot(tbl,                                     # riesgo segun genero
        main        = "Distribución de Riesgo según Genero",
        beside      = TRUE,             
        col         = c('#1A5276','#D5DBDB'), 
        legend.text = TRUE,
        args.legend = list(title = "Genero", x = "top"),
        xlab        = "Riesgo",
        ylab        = "Proporción")

tbl <- prop.table(table(df$HIPOTECA, df$RIESGO),margin=2)
barplot(tbl,                                     # riesgo segun hipoteca
        main        = "Distribución de Riesgo según Hipoteca",
        beside      = TRUE,             
        col         = c('#1A5276','#D5DBDB'), 
        legend.text = TRUE,
        args.legend = list(title = "Hipoteca", x = "topleft",inset = c(0.45, 0)),
        xlab        = "Riesgo",
        ylab        = "Proporción")

tbl <- prop.table(table(df$ESTADO_CIVIL, df$RIESGO), margin=2)
barplot(tbl,                                     # riesgo segun estado civil
        main        = "Distribución de Riesgo según Estado civil",
        beside      = TRUE,             
        col         = c("#34495E",'#2980B9',"skyblue"), 
        legend.text = TRUE,
        args.legend = list(title = "Estado civil", x = "top"),
        xlab        = "Riesgo",
        ylab        = "Proporción")
par(mfrow = c(1, 1))

tbl <- prop.table(table(df$MODALIDAD_PAGO, df$RIESGO),margin=2)
barplot(tbl,                                     #conteo de riesgo segun pagos
        main        = "Distribución de Riesgo segun Modalidad de pago",
        beside      = TRUE,             
        col         = c('#1A5276','#D5DBDB'), 
        legend.text = TRUE,
        args.legend = list(title = "Modalidad de pago", x = "topleft",cex=0.7,inset = c(0.03, 0)),
        xlab        = "Riesgo",
        ylab        = "Proporción")


par(mfrow = c(1, 3))
barplot(prop.table(table(df$RIESGO)),            #proporcion de riesgo
        main        = "Distribución de Riesgo",
        col         = c('tomato','#58D68D'),
        xlab        = "Riesgo",
        ylab        = "Proporción")

boxplot(EDAD~RIESGO,                             #boxplot de edad
        main        = "Distribución de Edad",
        col         = c('tomato','#58D68D'),
        data        = df,
        ylab        = "Edad",
        xlab        = "Riesgo")

boxplot(INGRESOS~RIESGO,                         #boxplot de ingresos
        main        = "Distribución de Ingresos",
        col         = c('tomato','#58D68D'),
        data        = df,
        varwidth    = T, 
        ylab        = "Ingresos",
        xlab        = "Riesgo",
        range       = 4)

par(mfrow = c(1, 3))
tbl <- prop.table(table(df$PRESTAMOS, df$RIESGO),margin=2)
barplot(tbl,                                     #conteo de riesgo segun prestamos
        main        = "Distribución de Riesgo según Prestamos",
        beside      = TRUE,             
        col         = c("#34495E",'#1A5276','#2980B9','#E5E7E9'), 
        legend.text = TRUE,
        args.legend = list(title = "N° de prestamos", x = "topleft",cex=0.85,inset = c(0.2, 0)),
        xlab        = "Riesgo",
        ylab        = "Proporción")

tbl <- prop.table(table(df$NUM_HIJOS, df$RIESGO),margin=2)
barplot(tbl,                                     #conteo de riesgo segun numero hijos
        main        = "Distribución de Riesgo según N° de hijos",
        beside      = TRUE,             
        col         = c("#34495E",'#2980B9',"skyblue",'#85C1E9','#D5DBDB'), 
        legend.text = TRUE,
        args.legend = list(title = "N° de hijos", x = "topleft",cex=0.85,inset = c(0.2, 0)),
        xlab        = "Riesgo",
        ylab        = "Proporción")

tbl <- prop.table(table(df$NUM_TARJETAS, df$RIESGO), margin=2)
barplot(tbl,                                     # riesgo segun numero tarjetas
        main        = "Distribución de Riesgo según N° de tarjetas",
        beside      = TRUE,             
        col         = c("#34495E",'#1A5276','#2980B9',"skyblue",'#85C1E9','#D5DBDB','#E5E7E9'), 
        legend.text = TRUE,
        args.legend = list(title = "N° de tarjetas", x = "topleft",cex=0.8,inset = c(0.25, 0)),
        xlab        = "Riesgo",
        ylab        = "Proporción")
par(mfrow = c(1, 1))

#Indicadores Resumen de las variables numéricas a través de la media y la std por RIESGO
coef.var = function(x){sd(x)/mean(x)}

doBy::summaryBy(EDAD + INGRESOS ~ RIESGO,
          data = df,
          FUN  = c(mean, sd, coef.var))

#------------------------------------------------------------------------------#
# 3. Entrenamiento de modelos                                               ####
#------------------------------------------------------------------------------#

#Eliminamos la columnda ID
df <- df[,-1]

Sum.Performance.tr=list()         # Aca guardaremos las distintas métricas

# Realizamos una particion estratificada (85% entrenamiento)
set.seed(42)
idx   <- createDataPartition(df$RIESGO, p = 0.85, list = FALSE)
df.tr <- df[idx, ]    # conjunto de entrenamiento
df.te <- df[-idx, ]   # conjunto de prueba


#------------------------------------------------------------------------------#
# Random Forest                                                             ####
#------------------------------------------------------------------------------#

# Definimos la estructura de la validación cruzada 
ctrl.CV <- trainControl(method = "repeatedcv",
                        number=10, repeats = 5,   # 10-fold repetido 5 veces.
                        savePredictions = "final")

# Creamos la grilla para el numero de variables
valor.mtry=expand.grid(mtry=2:5)

# Entrenamos el modelo
Model_RF <- train(RIESGO~. ,data=df.tr,method="rf", trControl=ctrl.CV, tuneGrid=valor.mtry)
Model_RF

# Graficamos el desempeño para los distintos valores de la grilla
plot(Model_RF, ylim= c(0.85,0.9),metric="Accuracy")
plot(Model_RF, ylim= c(0.5,0.7),metric="Kappa")

# Objeto con las distintas predicciones y observaciones
oof = Model_RF$pred

# Evaluamos el desempeño (Indicadores de correcta clasificación)
ICC_RF = caret::confusionMatrix(oof$pred, oof$obs, 
                                positive = "Yes", mode = 'everything')

# Guardamos los indicadores
Sum.Performance.tr$RF = ICC_RF$byClass


# Entrenamos un Random Forest con el mejor m-try para ver la feature importance

m = Model_RF$bestTune$mtry     # Número de variables predictoras. (best value)

Model_RF = randomForest::randomForest(RIESGO~., data=df.tr, ntree=500, mtry=m, importance=T)

# Visualizamos la importancia de las variables.
importancia = importance(Model_RF)
par(mfrow=c(1,2),mar = c(3, 7, 3, 0.5))
barplot(sort(importancia[,4],decreasing=F),col="blue",horiz=T,main=colnames(importancia)[4],las=1,cex.names=0.7)
barplot(sort(importancia[,3],decreasing=F),col="blue",horiz=T,main=colnames(importancia)[3],las=1,cex.names=0.7)
par(mfrow=c(1,1))

#------------------------------------------------------------------------------#
# Regresión Logistica                                                       ####
#------------------------------------------------------------------------------#

# Entrenamos primero sobre todas las variables para ver su significancia
Model_LogRegSat = glm(RIESGO~., data=df.tr,     # Modelo saturado
                      family = "binomial")      
anova(Model_LogRegSat)                          

# Definimos la estructura del cross-validation
ctrl.CV <- trainControl(method = "repeatedcv",
                        number=10, repeats = 5,   # 10-fold repetido 5 veces.
                        classProbs = TRUE,                  
                        summaryFunction = twoClassSummary,  
                        savePredictions = "final") 

# Entrenamos el modelo (omitiendo GENERO e HIPOTECA)
set.seed(42)
Model_LogReg <- train(RIESGO~. -GENERO -HIPOTECA,
                      data = df.tr, method = "glm", family = binomial(),
                      metric = "ROC", trControl = ctrl.CV)

# Guardamos las predicciones
oof <- Model_LogReg$pred    

# Curva ROC para Explorar el mejor punto de corte para la Regresión Logistica
roc  <- roc(response=oof$obs, predictor=oof$Yes, levels=c("No","Yes"), direction = "<")

# Graficamos la curva ROC
pROC::plot.roc(roc, legacy.axes = TRUE, print.thres = "best", print.auc = TRUE,
               auc.polygon = FALSE, max.auc.polygon = FALSE, auc.polygon.col = "gainsboro",
               col = 2, grid = TRUE) 

# Punto de corte optimo
pc_opt <- coords(roc, "best",best.method = "youden",ret = "threshold")[[1]]

# Generamos las clasificaciones con el punto de corte dado por la curva ROC
Predict_LogReg = factor(ifelse(oof$Yes > pc_opt, 'Yes', 'No'), levels = c("Yes","No"))

# Evaluamos el desempeño (Indicadores de correcta clasificación)
ICC_LogReg = caret::confusionMatrix(Predict_LogReg,oof$obs,
                                    positive='Yes', mode='everything')

# Guardamos los indicadores
Sum.Performance.tr$LogReg = ICC_LogReg$byClass


#------------------------------------------------------------------------------#
# Árbol de Clasificación                                                    ####
#------------------------------------------------------------------------------#

#Entrenamos el árbol (omitiendo GENERO, HIPOTECA y ESTADO_CIVIL)
Model_Tree<- rpart::rpart(formula = RIESGO ~ . -GENERO -HIPOTECA -ESTADO_CIVIL,
                          data = df.tr, control = rpart.control(cp = 0.001))

# Visualizamos el árbol
prp(Model_Tree, faclen = 0, cex = 0.6, extra = 1)

# Buscamos el mejor cp segun el cross validation realizado internamente por el árbol
printcp(Model_Tree) 
bestcp = Model_Tree$cptable[which.min(Model_Tree$cptable[,"xerror"]),"CP"]

# Podamos el árbol
Model_Tree = prune(Model_Tree, cp=bestcp)

# Visualizamos el nuevo árbol
prp(Model_Tree, faclen = 0, cex = 0.8, extra = 1)


# Realizamos el cross validation para medir el desempeño
fit_tree <- train(RIESGO ~ . -GENERO -HIPOTECA -ESTADO_CIVIL,
                  data=df.tr, method="rpart",
                  trControl=ctrl.CV, tuneGrid=data.frame(cp = bestcp),metric="ROC")

# Objeto con las distintas predicciones y observaciones
oof = fit_tree$pred

# Evaluamos el desempeño (Indicadores de correcta clasificación)
ICC_Tree = caret::confusionMatrix(oof$pred, oof$obs, 
                                  positive = "Yes", mode = 'everything')

# Guardamos los indicadores
Sum.Performance.tr$Tree = ICC_Tree$byClass


#------------------------------------------------------------------------------#
# XGBoost                                                                   ####
#------------------------------------------------------------------------------#

modelLookup('xgbTree')

# Estructura de validacion cruzada
ctrl.xgb <- trainControl(method="repeatedcv",number=5,repeats=3,
                         summaryFunction=twoClassSummary,savePredictions="final")

# Grilla de hiperparametros
xgb_grid <- expand.grid(
  nrounds            = c(100, 200),       # número de árboles
  max_depth          = c(3, 6, 9),        # profundidad máxima de cada árbol
  eta                = c(0.01, 0.1, 0.3), # tasa de aprendizaje
  gamma              = c(0, 1, 5),        # complejidad
  colsample_bytree   = 0.8,               # fracción de columnas por árbol
  min_child_weight   = 1,                 # peso mínimo en hojas
  subsample          = 0.8                # fracción de filas por iteración
)


# Entrenamiento del modelo 
Model_XGB <- train(
  RIESGO ~ . -GENERO -HIPOTECA -ESTADO_CIVIL,
  data      = df.tr,
  method    = "xgbTree",
  metric    = "Sens",        # Buscamos la mejor sensibilidad
  trControl = ctrl.xgb,
  tuneGrid  = xgb_grid,
  verbosity = 0 
)

# Resultado
Model_XGB


# Objeto con las distintas predicciones y observaciones
oof = Model_XGB$pred

# Evaluamos el desempeño (Indicadores de correcta clasificación)
ICC_XGB = caret::confusionMatrix(oof$pred, oof$obs, 
                                  positive = "Yes", mode = 'everything')

# Guardamos los indicadores
Sum.Performance.tr$XGB = ICC_XGB$byClass

#------------------------------------------------------------------------------#
# 4. Comparación del desempeño                                               ####
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Comparación con datos de entrenamiento                                    ####
#------------------------------------------------------------------------------#
Sum.Performance.tr = do.call(cbind,Sum.Performance.tr)
View(Sum.Performance.tr)

#------------------------------------------------------------------------------#
# Comparación con datos de test                                             ####
#------------------------------------------------------------------------------#

Sum.Performance.te=list()

# Random Forest
Predict_RF.te = predict(Model_RF,newdata = df.te)
ICC_RF.te = caret::confusionMatrix(Predict_RF.te, df.te$RIESGO ,positive = "Yes",mode = 'everything')
Sum.Performance.te$RF = ICC_RF.te$byClass

# Logistic Regression
pc.LogReg = pc_opt   #punto de corte
Predict_LogReg.te = predict(Model_LogReg, newdata = df.te, type="prob")[,'Yes']
Class.LogReg.te = factor(ifelse(Predict_LogReg.te > pc.LogReg,"Yes","No"), levels = c("Yes","No"))
ICC_LogReg.te = caret::confusionMatrix(Class.LogReg.te, df.te$RIESGO, positive = "Yes",mode = 'everything')
Sum.Performance.te$LogReg = ICC_LogReg.te$byClass

# Decision Tree
Predict_Tree.te = predict(Model_Tree, newdata=df.te ,type="class")
ICC_Tree.te = caret::confusionMatrix(Predict_Tree.te, df.te$RIESGO, positive = "Yes",mode = 'everything')
Sum.Performance.te$Tree = ICC_Tree.te$byClass

# XGBoost
prob_XGB.te = predict(Model_XGB, newdata = df.te, type = "prob")[, "Yes"]
Predict_XGB.te = factor(ifelse(prob_XGB.te > 0.5, "Yes", "No"), levels = c("Yes","No"))
ICC_XGB.te = caret::confusionMatrix(Predict_XGB.te, df.te$RIESGO, positive = "Yes",mode = 'everything')
Sum.Performance.te$XGB = ICC_XGB.te$byClass

# Juntamos los resultados de cada modelo
Sum.Performance.te = round(do.call(cbind,Sum.Performance.te),2)
View(Sum.Performance.te)

# Guardamos los modelos
#save(Model_RF, Model_LogReg, Model_Tree, Model_XGB, file = "modelos_entrenados.RData")
