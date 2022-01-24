server = function(input, output) {
  
  observeEvent(input$pdf, {file.show(file.path("pdf var.pdf")) })
  
  observeEvent(input$pdf2, {file.show(file.path("pdf2.pdf")) })
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro.Rhtml"),
      easyClose = TRUE,
      footer = tagList(
        actionButton("pdf2", label = "Aide", icon = icon("question"), style="color: #fff; background-color: #6e5b5b; border-color: #484545")
      )
    ))
  })
  
  
  output$tab <- renderTable({print(tab)})
  
  output$summary <- renderPrint({summary(data)})
  
  output$statdes <- renderTable({print(statdes)})
  
  output$graph1 <- renderPlot({graph1})
  
  output$graph2 <- renderPlot({graph2})
  
  output$graph3 <- renderPlot({graph3})
  
  
  ############
  output$lambdalasso <- renderText({paste("Vous avez sélectionné un lambda de : ", input$lambda.bestlasso,".")})
  output$lambdaalasso <- renderText({paste("Vous avez sélectionné un lambda de : ", input$lambda.bestalasso,".") })
  output$lambdaalphaelnet <- renderText({paste("Vous avez sélectionné un lambda de : ", input$lambda.bestelnet, "et un alpha de : ", input$alpha.bestelnet,".") })
  output$nbtree <- renderText({paste("Vous avez sélectionné un nombre d'arbre de : ", input$ntree,".") })
  output$nbit <- renderText({paste("Vous avez sélectionné un nombre d'itération de : ", input$mfinal,".") })
  #############
  
  
  #lasso
  
  lasso1 <- reactive({
    glmnet(x, y, alpha = 1, lamba=input$lambda.bestlasso , family = "binomial")
  })
  
  y.lasso1 <- reactive({
    predict(lasso1(), x_test, type="class",s=c(0))
  })
  
  output$coeflasso <- renderTable({ 
  coeff.lasso=coef(lasso1(), input$lambda.bestlasso)
  coeflasso=as.matrix(coeff.lasso)
  a=c("Intercept","RU_unsecuredlines","age","nb_3059days","debt_ratio","income_month","nb_creditloan","nb_90days","nb_realEloanlines","nb_6089days","nb_dependents")
  b=data.frame(x=a, y=coeflasso)
  colnames(b)=c("Variables","Coefficients")
  print(b) })
  
  output$matconfulasso <- renderTable({
  tab.lasso=as.matrix(table(test[,1], y.lasso1()))
  
  #correction de la matrice
  sp.lasso=tab.lasso[1]/(tab.lasso[1]+tab.lasso[3])
  se.lasso=tab.lasso[4]/(tab.lasso[2]+tab.lasso[4])
  
  name=c("Réponse 0", "Réponse 1")
  lasso1=c(round(freq0*sp.lasso*length(test[,1])),round(freq0*(1-sp.lasso)*length(test[,1])))
  lasso2=c(round(freq1*(1-se.lasso)*length(test[,1])),round(freq1*se.lasso*length(test[,1])))
  tab2.lasso=data.frame(name,lasso1,lasso2)
  colnames(tab2.lasso)=c(" ","Prédiction 0", "Prédiction 1")
  matlass=as.matrix(tab2.lasso)
  print (matlass) })
  
  
  output$acc.lasso <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.lasso1()==test[,1]),4),
      icon = icon("check-double"),
      color="red"
    )
  })

  output$auc.lasso <- renderValueBox({
    valueBox(
      subtitle="AUC", width=10,
      mapply(round, auc(roc(test[,1],as.numeric(y.lasso1()))),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  

  # adaptive lasso 
  alasso1 <- reactive({
    glmnet(x, y, family='binomial', lambda=input$lambda.bestalasso, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc', penalty.factor=w3)
  })
  
  y.alasso1 <- reactive({
    predict(alasso, x_test, type="class",s=c(0))
  })
  
  output$coefalasso <- renderTable({ 
  coeff.alasso=coef(alasso1(), input$lambda.bestlasso)
  coefalasso=as.matrix(coeff.alasso)
  a2=c("Intercept","RU_unsecuredlines","age","nb_3059days","debt_ratio","income_month","nb_creditloan","nb_90days","nb_realEloanlines","nb_6089days","nb_dependents")
  b2=data.frame(x=a2, y=coefalasso)
  colnames(b2)=c("Variables","Coefficients")
  print(b2) })
  
  output$matconfualasso <- renderTable({
  tab.alasso=as.matrix(table(test[,1], y.alasso1()))
  
  #correction de la matrice
  sp.alasso=tab.alasso[1]/(tab.alasso[1]+tab.alasso[3])
  se.alasso=tab.alasso[4]/(tab.alasso[2]+tab.alasso[4])
  
  name=c("Réponse 0", "Réponse 1")
  alasso1=c(round(freq0*sp.alasso*length(test[,1])),round(freq0*(1-sp.alasso)*length(test[,1])))
  alasso2=c(round(freq1*(1-se.alasso)*length(test[,1])),round(freq1*se.alasso*length(test[,1])))
  tab2.alasso=data.frame(name,alasso1,alasso2)
  colnames(tab2.alasso)=c(" ","Prédiction 0", "Prédiction 1")
  matalass=as.matrix(tab2.alasso)
  print(matalass)})
  
  output$acc.alasso <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.alasso1()==test[,1]),4),
      icon = icon("check-double"),
      color="red"
    )
  })

  output$auc.alasso <- renderValueBox({
    valueBox(
      subtitle="AUC", width=10,
      mapply(round, auc(roc(test[,1],as.numeric(y.lasso1()))),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  
  
  #elsticnet on améliore la réactivité de l'app
  
  elnet1 <- reactive({
    glmnet(x, y, family='binomial', lambda=input$lambda.bestelnet, alpha=input$alpha.bestelnet, parallel=TRUE, standardize=TRUE, type.measure='auc')
  })
  
  y.elnet1 <- reactive({
    predict(elnet1(), x_test, type="class",s=c(0))
  })


  output$coefelnet <- renderTable({
  coeff.elnet=coef(elnet1(), input$lambda.bestelnet)                                          
  coefelnet=as.matrix(coeff.elnet)
  a3=c("Intercept","RU_unsecuredlines","age","nb_3059days","debt_ratio","income_month","nb_creditloan","nb_90days","nb_realEloanlines","nb_6089days","nb_dependents")
  b3=data.frame(x=a3, y=coefelnet)
  colnames(b3)=c("Variables","Coefficients")
  print(b3) })
  
  output$matconfuelnet <- renderTable({
  tab.elnet=as.matrix(table(test[,1], y.elnet1()))
  
  #correction de la matrice
  sp.elnet=tab.elnet[1]/(tab.elnet[1]+tab.elnet[3])
  se.elnet=tab.elnet[4]/(tab.elnet[2]+tab.elnet[4])
  
  name=c("Réponse 0", "Réponse 1")
  elnet1=c(round(freq0*sp.elnet*length(test[,1])),round(freq0*(1-sp.elnet)*length(test[,1])))
  elnet2=c(round(freq1*(1-se.elnet)*length(test[,1])),round(freq1*se.elnet*length(test[,1])))
  tab2.elnet=data.frame(name,elnet1,elnet2)
  colnames(tab2.elnet)=c(" ","Prédiction 0", "Prédiction 1")
  matelnet=as.matrix(tab2.elnet)
  print(matelnet)})
  
  
  output$acc.elnet <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.elnet1()==test[,1]),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  
  output$auc.elnet <- renderValueBox({
    valueBox(
      subtitle="AUC", width=10,
      mapply(round, auc(roc(test[,1],as.numeric(y.elnet1()))),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  
  
  #random forest
  y.rf1 <- reactive({
    predict(randomForest(train$ser_delinquency~. , data=train, ntree=input$ntree, mtry=round(sqrt(ncol(x))), type=classification), x_test, type="class",s=c(0))
  })
  
  output$matconfurf <- renderTable({ 
  tab.Rforest=as.matrix(table(test[,1], y.rf1()))
  
  #correction de la matrice
  sp.Rforest=tab.Rforest[1]/(tab.Rforest[1]+tab.Rforest[3])
  se.Rforest=tab.Rforest[4]/(tab.Rforest[2]+tab.Rforest[4])
  
  name=c("Réponse 0", "Réponse 1")
  Rforest1=c(round(freq0*sp.Rforest*length(test[,1])),round(freq0*(1-sp.Rforest)*length(test[,1])))
  Rforest2=c(round(freq1*(1-se.Rforest)*length(test[,1])),round(freq1*se.Rforest*length(test[,1])))
  tab2.Rforest=data.frame(name,Rforest1,Rforest2)
  colnames(tab2.Rforest)=c(" ","Prédiction 0", "Prédiction 1")
  matrf=as.matrix(tab2.Rforest)
  print(matrf)})
  
  
  output$acc.rf <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.rf1()==test[,1]),4),
      icon = icon("check-double"),
      color="red"
    )
  })

  
  output$auc.rf <- renderValueBox({
    valueBox(
      subtitle="AUC",
      mapply(round, auc(roc(test[,1],as.numeric(y.rf1()))),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  
  y.boosting1 <- reactive({
    predict(boosting(ser_delinquency~., data=train, boos=FALSE, mfinal=input$mfinal, coeflearn="Breiman"), test[,-1])$class
  })
  
  
  output$matconfuboost <-renderTable ({
    #matrice de confusion
    tab.boosting=as.matrix(table(test[,1], y.boosting1()))
    
    #correction de la matrice
    sp.boosting=tab.boosting[1]/(tab.boosting[1]+tab.boosting[3])
    se.boosting=tab.boosting[4]/(tab.boosting[2]+tab.boosting[4])
    
    name=c("Réponse 0", "Réponse 1")
    boosting1=c(round(freq0*sp.boosting*length(test[,1])),round(freq0*(1-sp.boosting)*length(test[,1])))
    boosting2=c(round(freq1*(1-se.boosting)*length(test[,1])),round(freq1*se.boosting*length(test[,1])))
    tab2.boosting=data.frame(name, boosting1, boosting2)
    colnames(tab2.boosting)=c(" ","Prédiction 0", "Prédiction 1")
    matb=as.matrix(tab2.boosting)
    print(matb)})
  
  
  output$acc.boost <- renderValueBox({
    valueBox(
      subtitle="Précision",
      mapply(round, mean(y.boosting1()==test[,1]),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  
  output$auc.boost <- renderValueBox({
    valueBox(
      subtitle="AUC",
      mapply(round, auc(roc(test[,1],as.numeric(y.boosting1()))),4),
      icon = icon("check-double"),
      color="red"
    )
  })
  
  
  
  output$comparaison <- renderTable({(compare) })
  
  
  
  
  
  
}