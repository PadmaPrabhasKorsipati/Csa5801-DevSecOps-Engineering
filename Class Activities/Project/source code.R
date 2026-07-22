############################################################
# Professional DevSecOps Network Diagram
############################################################

# Node Labels
nodes <- c("Internet",
           "Firewall",
           "Web Server",
           "Application Server",
           "Database",
           "CI/CD Server",
           "Developer PC",
           "Backup Server")

# Node Positions
x <- c(5,5,5,5,3,7,7,7)
y <- c(8,7,6,5,4,4,3,2)

# Initial Colors
colors <- rep("lightgreen",8)

drawNetwork <- function(colors,titleText){
  
  plot(0,0,
       type="n",
       xlim=c(1,9),
       ylim=c(1,9),
       axes=FALSE,
       xlab="",
       ylab="",
       main=titleText,
       cex.main=1.5)
  
  # Connections
  segments(5,8,5,7,lwd=3,col="gray40")
  segments(5,7,5,6,lwd=3,col="gray40")
  segments(5,6,5,5,lwd=3,col="gray40")
  segments(5,5,3,4,lwd=3,col="gray40")
  segments(5,5,7,4,lwd=3,col="gray40")
  segments(7,4,7,3,lwd=3,col="gray40")
  segments(7,3,7,2,lwd=3,col="gray40")
  
  # Nodes
  symbols(x,y,
          circles=rep(0.35,8),
          inches=FALSE,
          bg=colors,
          fg="black",
          lwd=2,
          add=TRUE)
  
  # Labels
  text(x,y-0.55,
       labels=nodes,
       cex=0.85,
       font=2)
  
  # Legend
  legend("bottomleft",
         legend=c("Safe","Infected"),
         pt.bg=c("lightgreen","red"),
         pch=21,
         pt.cex=2,
         bty="n")
}

############################################################
# Initial Diagram
############################################################

drawNetwork(colors,
            "Threat Propagation Simulation in DevSecOps")

Sys.sleep(2)

############################################################
# Threat Propagation
############################################################

for(i in 3:8){
  
  colors[i] <- "red"
  
  drawNetwork(colors,
              paste("Threat Reached :",nodes[i]))
  
  Sys.sleep(1)
  
}

drawNetwork(colors,
            "Threat Propagation Completed")
