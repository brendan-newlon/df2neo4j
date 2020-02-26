# df2neo4j
An R package that lets you load data from a data.frame into a Neo4j graph database with a single line of code.

# load_df_to_neo4j

This function allows you to write a data.frame directly to nodes in Neo4j. 

Note: The column names of the df will be used to name the properties. 
Eg. "Name", "Phone", "Email", "Client_ID"

The function writes a .csv file in your Neo4j database import directory, then loads the data to Neo4j. 
Once done, the .csv file is no longer needed, and it will be overwritten the next time the function is used. 


ARGUMENTS 
- df 
The data.frame with data about your nodes and their properties
- label 
Use label to indicate what type of node is represented in your df. You can assign one or more labels. 

For example, if one df contains data about your individual clients while another holds data about your 
corporate clients, you can label both as clients while also labeling one set as people and the other as organizations:

	load_df_to_neo4j(
	 individual_clients,
	 label = c("Person", "Client"),
	 Unique_ID_col = "Client_ID",
	 other_constrain_col = c("Email", "Phone")
	 )
 
	load_df_to_neo4j(
	 corporate_clients,
	 label = c("Organization", "Client"),
	 Unique_ID_col = "Client_ID",
	 other_constrain_col = c("Client_Contact_Email", "Client_Contact_Phone")
	 )

- Unique_ID_col 
Indicate one column that provides a unique ID for each unique node. 
This could be an ID number or something else that should be unique, such as a personal email address or phone number.

- other_constrain_col 
Identify any other columns that contain values that should
be unique to one node, such as a personal email address.

NOTE: If you've accidentally assigned two different IDs where the person's email 
address is the same, this will throw an error so you can locate the problem row.

EXAMPLE

	load_df_to_neo4j(	
	df, 
	label = c("Person", "Client"), 
	Unique_ID_col = "Client_ID", 
	other_constrain_col = c("Email", "Phone")
	)


# load_edges_to_neo4j

This function allows you to write a data.frame of edges/relationships to connect nodes in Neo4j. 

The function writes a .csv file in your Neo4j database import directory, then loads the data to Neo4j. 
Once done, the .csv file is no longer needed, and it will be overwritten the next time the function is used. 

Note: The column names of the df must identify the label(s) for the type(s) of nodes you are connecting, 
but the contents of the columns are unique ID values. 

For exammple, the first column might be named "Person" and the values in that column are the value you 
defined using Unique_ID_col when creating the nodes (See: ?load_df_to_neo4j). 

ARGUMENTS 
- df.with.labels.as.colnames 
A data.frame of two columns, with node labels as column names. Eg. "Client". 
It's ok for both columns to have the same name if they are the same kind of node. If a column is unnamed, 
I ~think~ it will match a node of any label with the unique property value you define.

- a.unique.property 
The name of a unique property for nodes in the first column. Eg. "Client_ID". Use the name of the unique property exactly as you defined it when you created the nodes.

- b.unique.property 
The name of a unique property for nodes in the second column. Eg. "Client_ID" 

- relationship_in_cypher 
The relationship to create between the nodes in the first and second columns, written in Cypher code. Relationships can be directed or undirected, and they can be labeled or unlabeled. Cypher is not case-sensitive. 

  For directed relationships, the head of the arrow indicates direction: 
  eg. if A LIKES B, use "-[:Likes]->"  
  eg. if B LIKES A, use  "<-[:Likes]-" 
  eg. to define an unlabeled directed relationship from A towards B, use "-->"
  
  For mutual or undirected relationships, no arrow is needed. 
  eg. if A and B have met, use "-[:Has_Met]-"

NOTE: Only one relationship label can be defined at a time.
  eg. if everyone in columns A and B have met and each pair likes the other, run the function once with "-[:Has_Met]-" and run it a second time with "-[:Likes]-" 

~ I hope to add a column for edge weights or other variable properties in the next version.

EXAMPLE:

	load_edges_to_neo4j(
	df,    
	a.unique.property = "Customer_ID", 
	b.unique.property = "Item_SKU", 
	relationship_in_cypher = "-[:PURCHASED]->"
	)

eg. The df might be two columns named "Customer" and "Product" if those are your node labels


# delete_all_neo4j

WARNING: This function deletes all nodes and relationships in your current Neo4j database. The function does not take any arguments.

This function comes in handy if you're developing a repeatable process to build your graph database and you want 
to repeatedly experiment and return to a blank slate. 

EXAMPLE:

	delete_all_neo4j() # WARNING - This will delete EVERYTHING in your graph database!





