#' df2neo4j
#'
#' Helpful R functions for working with Neo4j graph databases
#'
#' @docType package
#' @name df2neo4j
#'
#'
#' @import neo4r
#' @import dplyr
#' @import utf8
#' @import utils


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
#' @param neo.import.dir The directory location of the Neo4j import folder.
#'
#' @export
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

load_df_to_neo4j <- function(df, label, Unique_ID_col, other_constrain_col = "NONE", neo.import.dir){
  # dir = neo.import.dir

  dir = str_split(neo.import.dir,"\\\\") %>% .[[1]]
  # thedir = paste0("file.path(")
  thedir = character()
  for(i in seq_along(dir)){
    thedir = paste0(thedir,"'",dir[[i]],"',")
  }
  # dir = thedir %>% gsub(",'',",")",.)
  dir = thedir %>% gsub(",\'\',",",\'df.csv\'",.)
  dir = paste0("file.path(",dir,")")


  all_labels <- paste0(":",label," ", collapse = " ") %>% str_trim

  names(df) <- names(df) %>% gsub("\\.", "_", .)
  df <- df %>% mutate_all(as.character)

  # replace NA with blank
  df <- df %>% replace(., is.na(.), "")


  # write.table(df,file = paste0(dir,"df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )
  write.table(df,file = eval(parse(text=dir)), append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )

  # file.path(thedir,"df.csv")

  # write.table(df,file = paste0(dir,"df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )


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
      if (length(other_constrain_col) > 1 && other_constrain_col != "NONE") {
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
      # write.table(df1, file = paste0(dir, "df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA")



      # !!!!!!!!!!!!!!!!!!!!! PROBLEM IS HERE !!!!!!!!!!!!!!!!!!!!!!
    # when run from the package, for some reason it fails to do this step. But if you run it as a local function there's no problem.
      #FixMe

      paste0("USING PERIODIC COMMIT 10000 LOAD CSV WITH HEADERS FROM 'file:///df.csv' AS line ",
             "MERGE(a", all_labels, "{`", Unique_ID_col,"`: line.",Unique_ID_col,"}) SET a += { ", props," }")  %>%  call_neo4j(con)



          }
  } else {  # if there are no other constrain cols...
    # simplify the properties identification to exactly match the column names
    props <- character()
    if(ncol(df) > 1){
      for (i in 2:ncol(df)) {
        props <-
          paste0(props, paste0("`", names(df[i]), "`: line.", names(df[i]), ","))
      }
    props <- props %>%  gsub(",$", "", .)

        # write.table(df, file = paste0(dir, "df.csv"), append = FALSE, row.names = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA")
    write.table(df,file = eval(parse(text=dir)), append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )

    paste0("USING PERIODIC COMMIT 10000 LOAD CSV WITH HEADERS FROM 'file:///df.csv' AS line ", "MERGE(a", all_labels, "{`", Unique_ID_col, "`: line.", Unique_ID_col, "}) SET a += { ", props, " }")  %>%
      call_neo4j(con)
    }
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
#' @param neo.import.dir The directory location of the Neo4j import folder.
#'
#' @export
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

load_edges_to_neo4j <- function(
  df.with.labels.as.colnames, 
  a.unique.poperty, 
  b.unique.poperty, 
  relationship_in_cypher, 
  neo.import.dir,
  periodic_commit = "10000"
  ){
# dir = neo.import.dir

  dir = str_split(neo.import.dir,"\\\\") %>% .[[1]]
  # thedir = paste0("file.path(")
  thedir = character()
  for(i in seq_along(dir)){
    thedir = paste0(thedir,"'",dir[[i]],"',")
  }
  # dir = thedir %>% gsub(",'',",")",.)
  dir = thedir %>% gsub(",\'\',",",\'df.csv\'",.)
  dir = paste0("file.path(",dir,")")



    # ADD OPTION TO HAVE A COLUMN FOR EDGE WEIGHTS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  # write.table(df.with.labels.as.colnames,file = paste0(dir,"df.with.labels.as.colnames.csv"),
  #             append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )

  write.table(df.with.labels.as.colnames,file = eval(parse(text=dir)), append = FALSE, row.names = FALSE, quote = TRUE, sep=",", eol="\n",  na = "NA"  )



  label_a <- names(df.with.labels.as.colnames[1])
  label_b <- names(df.with.labels.as.colnames[2])

#   paste0("USING PERIODIC COMMIT 1000
# LOAD CSV WITH HEADERS FROM \"file:///df.with.labels.as.colnames.csv\" as csvLine
# MATCH (a :",label_a," { ",a.unique.poperty,": csvLine.",label_a," })
# MATCH (b :",label_b," { ",b.unique.poperty,": csvLine.",label_b," })
# MERGE (a) ",relationship_in_cypher," (b) ") %>%  call_neo4j(con)
  paste0("USING PERIODIC COMMIT ",periodic_commit," 
LOAD CSV WITH HEADERS FROM \"file:///df.csv\" as csvLine
MATCH (a :",label_a," { ",a.unique.poperty,": csvLine.",label_a," })
MATCH (b :",label_b," { ",b.unique.poperty,": csvLine.",label_b," })
MERGE (a) ",relationship_in_cypher," (b) ") %>%  call_neo4j(con)


  }




#####################################################################################

#' neo_df
#'
#' @description
#' A shortcut alias for load_df_to_neo4j.
#'
#' @details
#' 
#' @export


neo_df = function(df,label, Unique_ID_col, other_constrain_col = "NONE"){
  load_df_to_neo4j(df, label, Unique_ID_col, other_constrain_col, neo.import.dir = neo.import.dir)
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
#' @export

delete_all_neo4j <- function() {
  "MATCH (n) DETACH DELETE n" %>%
    call_neo4j(con)
}

#####################################################################################

#' enhance_from_keys
#'
#' @description
#' Enhance a target data.frame that has one column of keys or IDs from
#' the key-value pair that exists in two columns of a source data.frame.
#'
#' @details
#' This handles a join to enhance one data.frame that contains a column
#' of keys/IDs from another data.frame that contains columns of key-value
#' pairs. For example, to lookup names by reference to ID numbers.
#'
#'
#' @param target_df The data.frame to be enhanced.
#'
#'
#' @param source_key_col The column in the reference data.frame containing
#' the keys/IDs, passed as a vector, eg. Employees$id
#'
#' @param source_prop_col The column in the reference data.frame containing
#' the values, passed as a vector, eg. Employees$name
#'
#' @param target_key_col_name The name of the column in the target data.frame
#' containing the keys/IDs, passed as a string, eg. "Employee_ID"
#'
#' @param target_prop_name The name for the new column to be added to the
#' target data.frame containing the values, passed as a string,
#' eg. "Employee_Name"
#'
#'
#' @export


#_________________________________ enhance_from_prop_key()
enhance_from_keys = function(target_df, source_key_col, source_prop_col, target_key_col_name, target_prop_name, remove_target_key_col = TRUE){
  target_df = target_df %>% setNames(names(target_df) %>% gsub(paste0("^",target_prop_name,"$"),paste0(target_prop_name,"_REDUNDANT"),.) ) # just in case
  s = source_key_col %>% as.df %>% setNames(target_key_col_name)
  s[[target_prop_name]] = source_prop_col
  s = s %>% mutate_all(as.character) %>% .[complete.cases(.),]
  target_df = left_join(target_df, s)
  if(remove_target_key_col) { df[[target_key_col_name]] = NULL }
  return(target_df)
}

#####################################################################################

#' neo_named_nodes
#'
#'
#' @description
#' create unique-constrained named nodes from unique values in a dataframe column.
#'
#' @details
#' If you have a column with values that can stand as a unique identifier for the type
#' of nodes they represent, this function creates a constraint that nodes with this label are
#' uniquely constrained by the property key you identify.
#'
#' Imagine you want to create nodes representing US Postal Codes. Point this function to
#' a column containing the zip codes. You might set the node label to be "Zip_Code". By
#' default, the property key is set to "Name," but you might choose "Zip" as more intuitive
#' than using Cypher QL to match: .... where Zip_Code.Name = '24710'
#'
#'
#' @param df The data.frame containing the column.
#'
#' @param col The name of the column, as a string.
#'
#' @param label The node label to set in Neo4j for these nodes. As a string.
#'
#' @param property_key The name for the property key for these values. Eg. "Name" for nodes labeled "Person". The default is "Name". As a string.
#'
#' @export


#__________________________neo_named_nodes()
# create unique-constrained named nodes from unique values in a dataframe column.

neo_named_nodes = function(df, col,label = col, property_key = "Name"){
  y <- df[[col]] %>% as.df %>% na_if("")  %>% unique() %>% .[complete.cases(.),] %>% as.df %>% setNames(property_key)
  paste0("CREATE CONSTRAINT ON (n:",label,") ASSERT n.",property_key," IS UNIQUE") %>%
    call_neo4j(con)
  neo_df(y, label, Unique_ID_col = property_key)
}

#####################################################################################


#' unique_sep_values_of_col
#'
#'
#' @description
#' create a new single-column data.frame of unique values from a column of data that may
#' include multiple values in a cell separated by a separator character.
#'
#' @details
#' This takes a column of data that may include multiple values in a cell separated
#' by a separator character and creates a new single-column data.frame of unique values.
#'
#' @param df The data.frame containing the column of data.
#'
#' @param col The name of the data column, as a string.
#'
#' @param label The new column name. As a string.
#'
#' @param sep The separator between items in a cell of the column, eg "," or ";"
#'
#' @export


# ______________________________ unique_sep_values_of_col()
# for when a column has values (optionally separated by a character) that should be turned into a list of unique values.
# in this case, I'm using it to make uniqely-named nodes.
unique_sep_values_of_col = function(df, col , label = "df_col_name", sep = ";",...){
  if(label == "df_col_name"){
    label = names(df[col])
  }
  print(paste0("Labeling nodes as: ",label))
  x = df[[col]] %>% as.df %>% separate_rows(.,1, sep=sep) %>% na_if("")  %>% unique() %>% .[complete.cases(.),] %>% as.df %>% setNames(label)
  return (x)
}

# x = unique_sep_values_of_col(df = ECMCustom, col = "Level_of_Impact")



#####################################################################################



#' nodify_col
#'
#'
#' @description
#' Shortcut to get unique values from a df column using unique_sep_values_of_col() and
#' then create nodes from them.
#'
#' @details
#' remember that label is the NODE label in Neo4j, and propery_key is the unique
#' value that constrains nodes of that label.
#'
#' So if your df has a column with names of items, eg lizard_species, that are unique
#' names, this is perfect.
#'
#' If the column is already named Lizard_Species (which is what you want as the
#' Node label, ie :Lizard_Species) then you don't need to enter anything for label.
#'
#' If you want the unique property value to be "Name" (ie. Lizard_Species.Name) then
#' leave property_key blank.
#'
#' If some rows have multiple items (eg, the row was tagged with multiple species) then
#' set the sep and all you need to do is point this funtion at the df and column.
#'
#' @param df The data.frame containing the column of data.
#'
#' @param col The name of the data column, as a string.
#'
#' @param label The label to assign to these nodes. The default is to take this
#' label from the name of the column. As a string.
#'
#' @param property_key The name for the property key for these values. Eg. "Name" for nodes labeled "Person". The default is "Name". As a string.
#'
#' @param sep The separator between items in a cell of the column, eg "," or ";"
#'
#' @export


#___________________________ nodify_col()
# Shortcut to get unique values from a df column using unique_sep_values_of_col() and then creating nodes from them.
# remember that label is the NODE label in Neo4j, and propery_key is the unique value that constrains nodes of that label.
# So if your df has a column with names of items, eg lizard_species, that are unique names, this is perfect.
# If the column is already named Lizard_Species (which is what you want as the Node label, ie :Lizard_Species) then you don't need to enter anything for label.
# If you want the unique property value to be "Name" (ie. Lizard_Species.Name) then leave property_key blank.
# If some rows have multiple items (eg, the row was tagged with multiple species) then set the sep.
# All you need to do is point this funtion at the df and column.

nodify_col = function(df, col, label = "df_col_name", property_key = "Name", sep = ";"){
  if(label == "df_col_name"){
    label = names(df[col])
  }
  x = unique_sep_values_of_col(df,col,label,property_key, sep = sep) # a df with one column with label as its name
  neo_named_nodes(df = x, col = label , label = label, property_key = property_key)
}

# nodify_col(df = ECMCustom, col = "Level_of_Impact")


#####################################################################################


#' nodify_cols
#'
#' @description
#' A shortcut to run nodify_col on multiple columns. The only difference is to
#' enter the column names as a character vector.
#'
#' @details
#' Assuming all defaults, with lots of simple, unique-name-only columns representing
#' nodes that have the node label as column name, and will have unique prop called Name (or
#' whatever you set for all of them)
#'
#' ie. in Cypher: (:Label) and Label.Name
#'
#' @param df The data.frame containing the column of data.
#'
#' @param col The names of the data columns, as a list or vector, eg. col = c("Organization", "City", "Postal_Code")
#'
#' @param label The label to assign to these nodes. The default is to take this
#' label from the name of the column. As a string.
#'
#' @param property_key The name for the property key for these values. Eg. "Name" for nodes labeled "Person". The default is "Name". As a string.
#'
#' @param sep The separator between items in a cell of the column, eg "," or ";"
#'
#' @export

#________________________________ nodify_cols()   # note the plural
# Assuming all defaults, with lots of simple, unique-name-only columns representing nodes that have the node label as column name, and will have unique prop called Name (or whatever you set for all of them)
# ie: (:Label) and Label.Name
nodify_cols = function(df, col, label = "df_col_name", property_key = "Name", sep = ";"){
  label = "df_col_name"
  property_key = "Name"
  sep = ";"
  for (i in seq_along (col)){
    nodify_col(df , col[i])
  }
}
####################################################################################


#' edgify_cols
#'
#' @description
#' Write edges among two or more types of nodes described in a data.frame to Neo4j.
#'
#' @details
#' 
#'
#'
#' @param df A data.frame 
#' 
#' @param a.col.name col with unique ID for a
#' 
#' @param b.col.names can be a character string or vector - cols with unique IDs for b
#'
#' @param a.unique.property The name of a unique property for nodes in the first column. Eg. "Client_ID".
#' Use the name of the unique property exactly as you defined it when you created the nodes.
#'
#' @param b.unique.property The name of a unique property for nodes in the second column. Eg. "Client_ID"
#'
#' @param relationships_in_cypher The relationships to create between the nodes, written in Cypher code.
#' Relationships can be directed or undirected, and they can be labeled or unlabeled. Cypher is not case-sensitive.
#'
#' - For directed relationships, the head of the arrow indicates direction:
#'   eg. if A LIKES B, use "-[:Likes]->" # NOTE: brackets aren't displaying properly. Everything between the dashes should be enclosed in brackets.
#'   eg. if B LIKES A, use  "<-[:Likes]-"
#'   eg. to define an unlabeled directed relationship from A towards B, use "-->"
#'
#' - For mutual or undirected relationships, no arrow is needed.
#'   eg. if A and B have met, use "-[:Has_Met]-"
#'
#'
#' @param a.node.label
#' 
#' @param b.node.labels
#'
#' @param neo.import.dir The directory location of the Neo4j import folder.
#' 
#' @param periodic_commit Default is "10000" rows in memory at a time.
#'
#' @export
#'
#' @return The function writes a .csv file in your Neo4j database import directory, then loads the data to Neo4j.
#' Once done, the .csv file is no longer needed, and it will be overwritten the next time the function is used.
#'
#'
#' @examples
#'
#' edgify_cols(
#'    df = staff2,
#'    a.col.name = "userPrincipalName", 
#'    b.col.names = c("Business_Unit", "Group", "Department", "City", "Office"), # plural
#'    a.unique.property = a.col.name,
#'    b.unique.properties = "Name", # pl
#'    relationships_in_cypher = "-[:Works_In]->", # can be plural but defaults to unnamed relationship. If rels from a to all other are the same, enter it once.
#'    a.node.label = "Person", # default
#'    b.node.labels = b.col.names, # default, pl
#'    neo.import.dir,
#'    periodic_commit = "100000"
#' )
#' 
#' 
#' 
#'


edgify_cols = function(
  df,
  a.col.name, # col with unique ID for a
  b.col.names, # plural - cols with unique IDs for b
  a.unique.property = a.col.name, # what is the name of the unique property in the graph? Eg. "Email" if it's Person.Email
  b.unique.properties = "Name", # plural
  relationships_in_cypher = "--", # Plural but defaults to unnamed relationship. If rels from a to all other nodes are the same, enter it once.
  a.node.label = a.col.name, # default expects the df column name to be the node label in the graph
  b.node.labels = b.col.names, # default, pl
  neo.import.dir = neo.import.dir,
  periodic_commit = "10000"
){
  
  if(length(b.col.names) > 1 && length(relationships_in_cypher) == 1){
    relationships_in_cypher = rep(relationships_in_cypher, length(b.col.names))
  }
  
  if(length(b.col.names) > 1 && length(b.unique.properties) == 1){
    b.unique.properties = rep(b.unique.properties, length(b.col.names))
  }
  
  for(i in seq_along(b.col.names)){
    print(paste0("Linking ",i," of ",length(b.col.names),"... "))
    x <- list(df[[a.col.name]], df[[b.col.names[i]]]) %>% # Which columns represent unique IDs for SOURCE and TARGET?
      as.df %>% na_if("") %>% .[complete.cases(.),] %>%
      setNames(c(a.node.label,b.node.labels[i])) # What are the node labels for SOURCE and TARGET nodes?
    
    load_edges_to_neo4j(
      x,
      a.unique.property,
      b.unique.properties[i],
      relationship_in_cypher = relationships_in_cypher[i],
      neo.import.dir,
      periodic_commit
    )  
  }  # end for loop
} # end of function





#####################################################################################

#' as.df
#'
#' @description
#' A shortcut alias for x <- as.data.frame(x, stringsAsFactors = FALSE)
#'
#' @param x The data.frame.
#'
#' @export

as.df = function(x){x = as.data.frame(x,stringsAsFactors=FALSE)}


#' %notin%
#'
#' @description
#' A shortcut alias for Negage(`%in%`).
#'
#' @export

`%notin%` <- Negate(`%in%`)

#' neo
#'
#' @description
#' A shortcut alias for call_neo4j(con), where con is the connection as configured in the neo4r Read-Me file.
#'
#' @export

neo = function(x){x = x %>% call_neo4j(con)}


#' no_na_cols
#'
#' @description
#' Remove any column in a data.frame where all values are NA.
#'
#' @param df The data object to convert into a data.frame without factors.
#'
#' @export

no_na_cols = function(df){
  df = df[,colSums(is.na(df))<nrow(df)]
  return(df)
}
# no_na_cols = function(df){df = df[,colSums(is.na(df))<nrow(df)]}



#####################################################################################

#' neo.find.nodes
#'
#' @description
#' Helps to construct Cypher statements to search for matching nodes in Neo4j.
#'
#' @details
#'
#' @param search_term Term to search for in node properties. Not case-sensitive.
#'
#' @param var Variable to assign to this node. Defaults to "n"
#'
#' @param label To search only nodes with a particular label, eg "Person". Defaults to any label.
#'
#' @param property The property type to search in. Defaults to any property.
#'
#' @param exact Use TRUE if you want only exact matches of your search term. Defaults to FALSE to match any property value that CONTAINS your search term.
#'
#' @param return Used to construct the RETURN portion of the Cypher query.
#'
#' @export
#'
#' @return Cypher query (or partial query) as a string to pass to Neo4j
#'
#' @examples
#'
#' bn <- neo.find.nodes("Brendan")
#'

neo.find.nodes <- (function(search_term, var = "n", label = "", propery = "any_property", exact = FALSE, return = ""){
  var_lab <- var
  if(label !="") var_lab <- paste0(var,":",label)

  # add if statements for property arg #feature_request !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if(exact){
    find_node_query <- paste0("MATCH (",var_lab,") WHERE any(key in keys(",var,") WHERE TOLOWER(",var,"[key]) = '",search_term,"')")
  }
  if(!exact) find_node_query <- paste0("MATCH (",var_lab,") WHERE any(key in keys(",var,") WHERE TOLOWER(",var,"[key]) CONTAINS TOLOWER('",search_term,"'))")
  return <- tolower(return) %>% gsub("return","",.)
  node.id <-  paste0(find_node_query, "RETURN ID(",var,")") %>% neo() %>% .[[1]] %>% as.df %>% setNames("ID") %>% as.vector()

  # handle multiple matches. get id, properties for each
  results <- vector("list", length(node.id$ID))
  for (i in seq_along(node.id$ID)){
    n <- paste0("MATCH (n) WHERE ID(n) = ",node.id$ID[i]," RETURN n") %>% neo() %>% .[[1]] %>% as.df()
    n$ID <- node.id$ID[i]
    labels <- paste("MATCH (n) WHERE ID(n) = ",node.id$ID[i]," UNWIND(labels(n)) AS labels RETURN labels") %>% neo() %>% .[[1]]
    n$Labels <-  as.list(labels)
    results[[i]] <- n
  }

  res <- as.df(results[[1]])
  for (i in seq_along(results)){
    res <- bind_rows(res,results[[i]])
  }
  res <- res %>% unique

  #fix_me !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Reorder the returned property columns sensibly, eg: ID, Name, ... etc    # bearing in mind schemas & prop types will vary...

  return(res)
})

#####################################################################################

#' neo.rel
#'
#' @description
#' Helps to construct relationship statements for people not familiar with Cypher syntax
#'
#' @details
#'
#' @param rel_var To assign a variable to the relationship. Defaults to "r".
#'
#' @param relationship  The relationship label, eg. "Knows".
#'
#' @param degrees Degrees of separation defaults to 1. For a range use eg. "1..4" for nodes connected within 1 to 4 degrees of distance.
#'
#' @param directed Directed defaults to undirected, but can be changed to "r", "-->" or "right" for right-directed, eg. a --> b (or similar for left-directed).
#'
#' @export
#'
#' @return A string describing a relationship in Cypher
#'
#' @examples
#'


# Using R to write relationships for people not familiar with cypher

#_______ rel allows you to specify a relationship between the nodes.
#          # Relationship is the relationship label.
#          # Degrees of separation defaults to 1. FOr a range use eg. "1..4"
#          # Directed defaults to undirected, but can be changed to "r" for right pointing (a --> b) or "l" for left pointing


neo.rel <- function(rel_var = "r", relationship = "", degrees = "1", directed = "--"){
  rel <- ""
  if(relationship != "") rel <- relationship %>% gsub("^",":",.) %>% gsub("::",":",.)
  rel <- paste0(rel_var,rel) # add variable for that relationship
  if(degrees != 1) rel <- paste0(rel,"*",degrees)
  rel <- case_when(
    directed == "--"|| directed == tolower("undirected")|| directed == tolower("no") ~ paste0("-[",rel,"]-"),
    directed == "<--"|| directed == tolower("left")|| directed == tolower("l") ~ paste0("<-[",rel,"]-"),
    directed == "-->"|| directed == tolower("right")|| directed == tolower("r") ~ paste0("-[",rel,"]->")
  )
  return (list(rel_var,rel))
}


#####################################################################################

#' neo.search
#'
#' @description
#' Get results for a query involving two nodes and the relationship(s) or path between them
#'
#' @details The arguments below are the same for the first node, a, and the second node, b. (Just change the letter prefix.)
#'
#' @param a.search_term Term to search for in node properties. Not case-sensitive.
#' @param a.var Variable to assign to this node. Defaults to "n"
#' @param a.label To search only nodes with a particular label, eg "Person". Defaults to any label.
#' @param a.property The property type to search in. Defaults to any property.
#' @param a.exact Use TRUE if you want only exact matches of your search term. Defaults to FALSE to match any property value that CONTAINS your search term.
#'
#' @param rel_var To assign a variable to the relationship. Defaults to "r".
#' @param relationship  The relationship label.
#' @param degrees Degrees of separation defaults to 1. FOr a range use eg. "1..4"
#' @param directed Directed defaults to undirected, but can be changed to "r", "-->" or "right" for right-directed, eg. a --> b (or similar for left-directed).
#'
#' @export
#'
#' @return A relationship defined as a Cypher string.
#'
#' @examples
#'


neo.search <- function(
  #______ a is the first node you're looking for.
  a.search_term,
  a.var = "a",
  a.label,
  a.property,
  a.exact,
  #_______ b is the second node you're looking for.
  b.search_term,
  b.var = "b",
  b.label,
  b.property,
  b.exact,
  #_______ rel allows you to specify a relationship between the nodes.
  #          # Relationship is the relationship label.
  #          # Degrees of separation defaults to 1. FOr a range use eg. "1..4"
  #          # Directed defaults to undirected, but can be changed to "r" for right pointing (a --> b) or "l" for left pointing
  rel.var = "r",
  rel.relationship,
  rel.degrees,
  rel.directed
){

  node.a = neo.find.nodes(a.search_term,a.var, a.label,a.property,a.exact)
  node.b = neo.find.nodes(b.search_term,b.var, b.label,b.property,b.exact)
  rel = neo.rel(rel.var,rel.relationship,rel.degrees,rel.directed)

  result <- paste0("MATCH (a) WHERE ID(a) = ",node.a$ID," WITH a MATCH (a)",rel[[2]],"(b) WHERE ID(b) = ",node.b$ID," RETURN distinct a,b") %>% neo()
  return (result)
}
