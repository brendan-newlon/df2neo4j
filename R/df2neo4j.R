#df2neo4j

ipak <- function(pkg){    
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]    
  if (length(new.pkg))         
    install.packages(new.pkg, dependencies = TRUE)    
  sapply(pkg, require, character.only = TRUE)}

packages <- c("neo4r","dplyr","utf8", "utils")
ipak(packages)


#' load_df_to_neo4j
#'
#' @description
#' Write a data.frame directly to nodes in Neo4j. 
#'
#' @details
#' This function allows you to write a data.frame directly to nodes in Neo4j. 
#' 
#' Note: The column names of the df will be used to name the properties. 
#' Eg. "Name", "Phone", "Email", "Client_ID"
#' 
#'
#' @param df The data.frame with data about your nodes and their properties
#' @param label Use label to indicate what type of node is represented in your df. 
#' You can assign one or more labels. 
#' 
#' For example, if one df contains data about your individual clients while another holds data about your 
#' corporate clients, you can label both as clients while also labeling one set as people and the other as organizations:
#' 
#' load_df_to_neo4j(
#'  individual_clients,
#'  label = c("Person", "Client"),
#'  Unique_ID_col = "Client_ID",
#'  other_constrain_col = c("Email", "Phone")
#'  )
#'  
#' load_df_to_neo4j(
#'  corporate_clients,
#'  label = c("Organization", "Client"),
#'  Unique_ID_col = "Client_ID",
#'  other_constrain_col = c("Client_Contact_Email", "Client_Contact_Phone")
#'  )
#' 
#' 
#' @param Unique_ID_col Indicate one column that provides a unique ID for each unique node. 
#' This could be an ID number or something else that should be unique, such as a personal email address or phone number.
#' 
#' @param other_constrain_col Identify any other columns that contain values that should
#' be unique to one node, such as a personal email address.
#' 
#' NOTE: If you've accidentally assigned two different IDs where the person's email 
#' address is the same, this will throw an error so you can locate the problem row.
#'
#' @return The function writes a .csv file in your Neo4j database import directory, then loads the data to Neo4j. 
#' Once done, the .csv file is no longer needed, and it will be overwritten the next time the function is used. 
#'
#'
#' @examples
#' load_df_to_neo4j(
#'    df, 
#'    label = c("Person", "Client"), 
#'    Unique_ID_col = "Client_ID", 
#'    other_constrain_col = c("Email", "Phone")
#' )
#' 

load_df_to_neo4j <- function(df, label, Unique_ID_col, other_constrain_col = "NONE"){
  all_labels <- paste0(":",label," ", collapse = " ") 
  
  names(df) <- names(df) %>% gsub("\\.", "_", .)
  df <- df %>% mutate_all(as.character)
  
  # replace NA with blank
  df <- df %>% replace(., is.na(.), "")
  
  write.table(df,file = paste0(dir,"df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )
  
  paste0("USING PERIODIC COMMIT 10000 LOAD CSV WITH HEADERS FROM 'file:///df.csv' AS line ", 
         "MERGE(a", all_labels,"{`", Unique_ID_col,"`: line.",Unique_ID_col,"})")  %>% call_neo4j(con)
  
  # create constraints where a property is unique?-----------------
  # FIRST: ON each label, constrain the Unique_ID_col
  for (i in 1:length(label)){
    # cat(paste0(  "CREATE CONSTRAINT ON (n:",label[i],") ASSERT n.",Unique_ID_col," IS UNIQUE"))  # !!!!!!!!!!!! For debugging
    paste0(  "CREATE CONSTRAINT ON (n:",label[i],") ASSERT n.",Unique_ID_col," IS UNIQUE") %>% call_neo4j(con)
  }
  
  # IF THERE ARE other constrain_cols
  if(other_constrain_col != "NONE") {
    
    # SECOND: ON each label, constrain each of the other constrain_col
    for (i in 1:length(label)) {
      if (length(other_constrain_col) > 1 || other_constrain_col != "NONE") {
        for (u in 1:length(other_constrain_col)) {
          paste0("CREATE CONSTRAINT ON (n:", label[i], ") ASSERT n.", other_constrain_col[u], " IS UNIQUE") %>% call_neo4j(con)
        }
      }
    }
    
    for (d in 1:length(other_constrain_col)) {
      cat(paste0("\n", "Sending MERGE ", d, " of ", length(other_constrain_col)))
      
      the_constrain_col <- (other_constrain_col[d])
      # what if values are missing from a unique-constrained column?
      omit_col <- other_constrain_col[!str_detect(other_constrain_col, the_constrain_col)]
      df1 <- df[,!(names(df) %in% omit_col)]
      
      df2 <- df1[which(str_detect(names(df1), Unique_ID_col))]
      df1[which(str_detect(names(df1), Unique_ID_col))] <- NULL
      df1 <- bind_cols(df2, df1)
      df1 <- df1[df[the_constrain_col] != "",]
      
      # simplify the properties identification to exactly match the column names
      props <- character()
      for (i in 2:ncol(df1)) {
        props <-
          paste0(props, paste0("`", names(df1[i]), "`: line.", names(df1[i]), ","))
      }
      
      props <- props %>%  gsub(",$", "", .)
      write.table(df1, file = paste0(dir, "df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA")
      
      paste0("USING PERIODIC COMMIT 10000 LOAD CSV WITH HEADERS FROM 'file:///df.csv' AS line ",
             "MERGE(a", all_labels, "{`", Unique_ID_col, Unique_ID_col,"}) SET a += { ", props," }")  %>%  call_neo4j(con)
    }
  } else {  # if there are no other constrain cols...
    # simplify the properties identification to exactly match the column names
    props <- character()
    for (i in 2:ncol(df)) {
      props <-
        paste0(props, paste0("`", names(df[i]), "`: line.", names(df[i]), ","))
    }
    props <- props %>%  gsub(",$", "", .)
    write.table(df, file = paste0(dir, "df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA")
    
    paste0("USING PERIODIC COMMIT 10000 LOAD CSV WITH HEADERS FROM 'file:///df.csv' AS line ", "MERGE(a", all_labels, "{`", Unique_ID_col, "`: line.", Unique_ID_col, "}) SET a += { ", props, " }")  %>%
      call_neo4j(con)
  }
} # End of function

#######################################################################

#' load_edges_to_neo4j
#'
#' @description
#' Write a data.frame of edges/relationships to Neo4j. 
#'
#' @details
#' This function allows you to write a data.frame of edges/relationships to connect nodes in Neo4j. 
#' 
#' Note: The column names of the df must identify the label(s) for the type(s) of nodes you are connecting, 
#' but the contents of the columns are unique ID values. 
#' 
#' For exammple, the first column might be named "Person" and the values in that column are the value you 
#' defined using Unique_ID_col when creating the nodes (See: ?load_df_to_neo4j). 
#' 
#'
#' @param df.with.labels.as.colnames A data.frame of two columns, with node labels as column names. Eg. "Client". 
#' It's ok for both columns to have the same name if they are the same kind of node. If a column is unnamed, 
#' I ~think~ it will match a node of any label with the unique property value you define.
#' 
#' @param a.unique.property The name of a unique property for nodes in the first column. Eg. "Client_ID". 
#' Use the name of the unique property exactly as you defined it when you created the nodes.
#' 
#' @param b.unique.property The name of a unique property for nodes in the second column. Eg. "Client_ID" 
#' 
#' @param relationship_in_cypher The relationship to create between the nodes in the first and second columns, written in Cypher code. 
#' Relationships can be directed or undirected, and they can be labeled or unlabeled. Cypher is not case-sensitive. 
#' 
#' - For directed relationships, the head of the arrow indicates direction: 
#'   eg. if A LIKES B, use "-[:Likes]->"  
#'   eg. if B LIKES A, use  "<-[:Likes]-" 
#'   eg. to define an unlabeled directed relationship from A towards B, use "-->"
#'   
#' - For mutual or undirected relationships, no arrow is needed. 
#'   eg. if A and B have met, use "-[:Has_Met]-"
#' 
#' NOTE: Only one relationship label can be defined at a time.
#'   eg. if everyone in columns A and B have met and each pair likes the other, 
#'   run the function once with "-[:Has_Met]-" 
#'   and run it a second time with "-[:Likes]-" 
#' 
#' ~ I hope to add a column for edge weights or other variable properties in the next version.
#'  
#'
#' @return The function writes a .csv file in your Neo4j database import directory, then loads the data to Neo4j. 
#' Once done, the .csv file is no longer needed, and it will be overwritten the next time the function is used. 
#'
#'
#' @examples
#' 
#' load_edges_to_neo4j(
#'    df,    
#'    a.unique.property = "Customer_ID", 
#'    b.unique.property = "Item_SKU", 
#'    relationship_in_cypher = "-[:PURCHASED]->"
#' )
#' # eg. The df might be two columns named "Customer" and "Product" if those are your node labels
#' 

load_edges_to_neo4j <- function(df.with.labels.as.colnames, a.unique.poperty, b.unique.poperty, relationship_in_cypher){
  
  # ADD OPTION TO HAVE A COLUMN FOR EDGE WEIGHTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  write.table(df.with.labels.as.colnames,file = paste0(dir,"df.with.labels.as.colnames.csv"), 
              append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )
  
  label_a <- names(df.with.labels.as.colnames[1])
  label_b <- names(df.with.labels.as.colnames[2])
  
  paste0("USING PERIODIC COMMIT 1000
LOAD CSV WITH HEADERS FROM \"file:///df.with.labels.as.colnames.csv\" as csvLine
MATCH (a :",label_a," { ",a.unique.poperty,": csvLine.",label_a," })
MATCH (b :",label_b," { ",b.unique.poperty,": csvLine.",label_b," })
MERGE (a) ",relationship_in_cypher," (b) ") %>%  call_neo4j(con)
} 

#####################################################################################

#' delete_all_neo4j
#'
#' @description
#' WARNING: This function deletes all nodes and relationships in your current Neo4j database. 
#'
#' @details
#' WARNING: This function deletes all nodes and relationships in your current Neo4j database. 
#' This comes in handy if you're developing a repeatable process to build your graph database and you want 
#' to repeatedly experiment and return to a blank slate.

delete_all_neo4j <- function() {
  "MATCH (n) DETACH DELETE n" %>%
    call_neo4j(con)
}
