library(dagitty)
library(ggdag)
# assumed DAG framework
coords <- data.frame(matrix(c("intercrop",0,0,
                              "nutrition",4,0,
                              "social",1,1,
                              "conservation",2,1,
                              "gender",3,1,
                              "plot",2,0.5), nrow=6, ncol=3, byrow = T))
colnames(coords) <- c("name", "x", "y")
dag <- dagify(nutrition~intercrop+social+conservation+gender+plot,
              intercrop~social+conservation+gender+plot,
              exposure = "intercrop",
              outcome="nutrition",
              coords = coords)
#plot
ggdag_classic(dag, text_col = "blue") + theme_dag_blank()

