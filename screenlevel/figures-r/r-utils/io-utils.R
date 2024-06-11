dt_froms3 <- function(object_s3uri) {
	aws.s3::s3read_using(data.table::fread, sep = "\t", object = object_s3uri) %>%
		janitor::clean_names()
}

get_guidecounts <- function(report_id, comparison_id, s3path) {
	s3_object <-
		file.path(
			s3path,
			report_id,
			"mageck_inputs",
			paste0(comparison_id, "_pro-pathogen.mageck_comparison.tsv")
		)
	selcols <- c("sg_rna", "compare_from", "compare_to")
	dt <- unique(dt_froms3(s3_object)[, ..selcols]) %>%
		melt(
			id.vars = c("sg_rna"),
			variable.name = "sample",
			variable.factor = FALSE,
			value.name = "raw_count"
		)
	setnames(dt, "sg_rna", "sgrna")
	dt[, comparison_id := comparison_id]
}

get_guidelfc <- function(report_id, comparison_id, s3path, mageckpath = "mageck") {
	s3_object <-
		file.path(
			s3path,
			report_id,
			mageckpath,
			paste0(
				comparison_id,
				"_pro-pathogen.mageck_comparison.sgrna_summary.txt"
			)
		)
	selcols <- c("sgrna", "gene", "lfc")
	dt <- unique(dt_froms3(s3_object)[, ..selcols])
	dt[, comparison_id := comparison_id]
}

get_genescores <- function(report_id, comparison_id, s3path, mageckpath = "mageck") {
	s3_object <-
		file.path(
			s3path,
			report_id,
			mageckpath,
			paste0(
				comparison_id,
				"_pro-pathogen.mageck_comparison.gene_summary.txt"
			)
		)
	dt <- dt_froms3(s3_object)
	dt[, comparison_id := comparison_id]
}

write_genescores <- function(report_id, comparison_id, screen_name, s3path, s3outpath, s3bucket, mageckpath = "mageck") {
	s3_object <-
		file.path(
			s3path,
			report_id,
			mageckpath,
			paste0(
				comparison_id,
				"_pro-pathogen.mageck_comparison.gene_summary.txt"
			)
		)
	dt <- dt_froms3(s3_object)
	aws.s3::s3write_using(dt, 
												fwrite, 
												sep = "\t", 
												object = file.path(s3outpath, 
																					 mageckpath,
																					 paste0(
																					 	comparison_id, "_",
																					 	screen_name,
																					 	".mageck_comparison.gene_summary.txt"
																					 )), 
												bucket = s3bucket)
}

s3save_ggplot <- function(ggp, object_s3uri, s3bucket, ...) {
	tmpfile <- paste0(tempfile(), ".pdf")
	ggsave(tmpfile, plot = ggp, ...)
	put_object(tmpfile, object = object_s3uri, bucket = s3bucket)
	
}

get_genesets <- function(organism, gs_selected = 'H'){
	
	if(!grepl("\\|", gs_selected)){
		genesets <-  msigdbr::msigdbr(species = organism, category = gs_selected)
	}else{
		gscat <- tstrsplit(gs_selected, "\\|")[[1]]
		gs_subcat <- tstrsplit(gs_selected, "\\|")[[2]]
		genesets <-  msigdbr::msigdbr(species = organism, category = gscat, subcategory = gs_subcat)
	}
	
	genelist <- split(x = genesets$gene_symbol, f = genesets$gs_name)
	str_to_replace <- paste0(tstrsplit(names(genelist)[1], "_")[[1]], "_")
	names(genelist) <- gsub(str_to_replace, "", names(genelist))
	return(genelist) 
}