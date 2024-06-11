# Standardize gene symbols ------------------------------------------------

# retrieve human symbol from mouse
format_dtmouse2human <- function(s3file_mouse2human) {
	mouse2human_dt <-
		aws.s3::s3read_using(data.table::fread, sep = "\t", object = s3file_mouse2human)
	mouse2human_dt[, n_human := length(unique(`Human Gene`)), by = "#Mouse Gene"]
	mouse2human_dt <- mouse2human_dt[n_human == 1]
	setnames(mouse2human_dt,
					 c("#Mouse Gene", "Human Gene"),
					 c("mouse_symbol", "human_symbol"))
	mouse2human_dt
}
# retrieve by gene
get_mouse2human <- function(mouse_query, dt_mouse2human) {
	query_uniq <- unique(mouse_query)
	map_dt <-
		dt_mouse2human[mouse_symbol %in% query_uniq &
									 	n_human == 1, .(mouse_symbol, human_symbol)]
	
	setkey(map_dt, "mouse_symbol")
	map_dt[mouse_query, human_symbol]
}

# retrieve human symbol from agm/green monkey
format_dtagm2human <- function(s3file_agm2human) {
	agm2human_dt <-
		aws.s3::s3read_using(data.table::fread, sep = "\t", object = s3file_agm2human)
	agm2human_dt <-
		unique(agm2human_dt[`Vervet-AGM homology type` == "ortholog_one2one",
												.(`Vervet-AGM gene name`,
													`Gene name`,
													`Source of gene name`,
													`Vervet-AGM homology type`)])
	setnames(
		agm2human_dt,
		c("Vervet-AGM gene name", "Gene name"),
		c("agm_symbol", "human_symbol")
	)
	unique(agm2human_dt[agm_symbol != ""])
}

# retrieve by gene
get_agm2human <- function(agm_query, dt_agm2human) {
	query_uniq <- unique(agm_query)
	map_dt <-
		dt_agm2human[agm_symbol %in% query_uniq, .(agm_symbol, human_symbol)]
	setkey(map_dt, "agm_symbol")
	map_dt[agm_query, human_symbol]
	
}

check_genesymbol <- function(query_genes, dt_map) {
	genes <- unique(query_genes)
	
	dt_map2 <-
		dt_map[, n_symbol := length(unique(human_symbol)), by = "gene_alias"]
	dt_map2 <- dt_map2[n_symbol == 1]
	approved <- genes %in% dt_map2[, human_symbol]
	
	alias_dt <-
		unique(dt_map2[gene_alias %in% genes, .(human_symbol, gene_alias)])
	
	convert_dt <- data.table(query = genes, approve_symbol = approved)
	convert_dt <-
		merge(
			convert_dt,
			alias_dt,
			by.x = "query",
			by.y = "gene_alias",
			all.x = T
		)
	convert_dt[, human_symbol := ifelse(approve_symbol == TRUE |
																				is.na(human_symbol),
																			query,
																			human_symbol)]
	
	setkey(convert_dt, "query")
	convert_dt[query_genes, human_symbol]
}

format_dthumanalias <- function(s3file_alias) {
	humanalias_dt <-
		aws.s3::s3read_using(data.table::fread, sep = "\t", object = s3file_alias)
	setnames(humanalias_dt,
					 colnames(humanalias_dt),
					 c("human_symbol", "gene_alias"))
	humanalias_dt
}