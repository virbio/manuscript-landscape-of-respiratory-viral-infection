# output format is gene, score, pval, fdr, lfc, pos/neg, numgoodsgrna, flag (when pos does not agree with median lfc)

format_mageckoutput <-
	function(dd,
					 report_cols = c("id",
					 								"num_guides",
					 								"LFC",
					 								"FDR",
					 								"pval",
					 								"score",
					 								"direction",
					 								"score_source"),
					 score_method = "mageck") {
		# LFC is median LFC for guides (same for neg and pos)
		dd[, LFC := `neg_lfc`]
		dd[, num_guides := num]
		# assign direction based on guide support
		dd[, FDR := ifelse(
			`neg_goodsgrna` > `pos_goodsgrna`  &
				max(`neg_goodsgrna`, `pos_goodsgrna`) > 1,
			`neg_fdr`,
			`pos_fdr`
		)]
		dd[, pval := ifelse(
			`neg_goodsgrna` > `pos_goodsgrna`  &
				max(`neg_goodsgrna`, `pos_goodsgrna`) > 1,
			`neg_p_value`,
			`pos_p_value`
		)]
		dd[, score := ifelse(
			`neg_goodsgrna` > `pos_goodsgrna`  &
				max(`neg_goodsgrna`, `pos_goodsgrna`) > 1,
			`neg_score`,
			`pos_score`
		)]
		
		dd[, direction := ifelse(
			`neg_goodsgrna` > `pos_goodsgrna`  &
				max(`neg_goodsgrna`, `pos_goodsgrna`) > 1,
			"neg",
			"pos"
		)]
		dd[, score_source := score_method]
		
		return(dd[, ..report_cols])
	}

get_mageckoutputbydir <- function(dt,
																	mydir = "pos",
																	id_cols = c("id", "num", "comparison_id"),
																	generic_cols = c(
																		"id",
																		"num_guides",
																		"comparison_id",
																		"score",
																		"pval",
																		"FDR",
																		"rank",
																		"goodsgrna",
																		"LFC"
																	)) {
	pos_cols <-
		c(id_cols, colnames(dt)[grepl(paste0("^", mydir), colnames(dt))])
	dt_pos <- dt[, ..pos_cols]
	setnames(dt_pos, colnames(dt_pos), generic_cols)
	dt_pos[, direction := mydir]
}

# Volcano Plot ------------------------------------------------------------

wrapper <- function(x, ...)
{
	paste(strwrap(x, ...), collapse = "\n")
}

plot_volcano <-
	function (dt,
						x = "LFC",
						y = "pval",
						Label = "id",
						top = 5,
						topnames = NULL,
						x_cutoff = 0,
						y_cutoff = 0,
						xhitcol = "LFC",
						yhitcol = "FDR",
						xhit_cutoff = 0,
						yhit_cutoff = 0.05,
						main = NULL,
						xlab = "Log2 Fold Change",
						ylab = "-Log10(P)",
						control_guides = "NO_SITE",
						label_fontsize = 5,
						label_nudgex = 0.05,
						...)
	{
		requireNamespace("ggrepel", quietly = TRUE) ||
			stop("need ggrepel package")
		BIGNO <- 1000000
		
		plot_cols <- unique(c(Label, x, y, xhitcol, yhitcol))
		gg <- dt[, ..plot_cols]
		
		# annotate hits by direction
		
		gg[, hit := "a"]
		gg[grepl(control_guides, get(Label)), hit := "ctrl"]
		gg[get(xhitcol) > xhit_cutoff &
			 	get(yhitcol) < yhit_cutoff, hit := "up"]
		gg[get(xhitcol) < (-1) * xhit_cutoff &
			 	get(yhitcol) < yhit_cutoff, hit := "down"]
		
		gg[, id_label := ""]
		gg[get(Label) %in% topnames, id_label := get(Label)]
		gg[, up_rank := BIGNO]
		gg[hit == "up", up_rank := frank(get(y), ties.method = "random")]
		gg[, down_rank := BIGNO]
		gg[hit == "down", down_rank := frank(get(y), ties.method = "random")]
		
		gg[up_rank <= top, id_label := get(Label)]
		gg[down_rank <= top, id_label := get(Label)]
		
		minnonzero <- gg[get(y) > 0, min(get(y))]
		
		gg[, y_plot := ifelse(get(y) == 0,-log10(minnonzero[1]),-log10(get(y)))]
		myfillcolour = c(
			a = "gray80",
			ctrl = "#D4A017",
			up = "#e41a1c",
			down = "#377eb8"
		)
		
		p = ggplot(gg, aes(
			x = get(x),
			y = y_plot,
			colour = hit,
			fill = hit
		))
		p = p + geom_jitter(
			position = "jitter",
			show.legend = FALSE,
			alpha = 0.8,
			size = 0.5
		)
		p = p + theme_classic()
		p = p + theme(
			text = element_text(
				colour = "black",
				size = 12,
				family = "Helvetica"
			),
			plot.title = element_text(hjust = 0.5,
																size = 12),
			axis.text = element_text(colour = "gray10")
		)
		if (y_cutoff > 0) {
			p = p + geom_hline(yintercept = -log10(y_cutoff),
												 linetype = "dotted")
		}
		p = p + geom_vline(xintercept = c(-x_cutoff, x_cutoff),
											 linetype = "dotted")
		
		p = p + labs(x = xlab,
								 y = ylab,
								 title = wrapper(main, width = 50))
		if (!(top == 0 & is.null(topnames))) {
			p = p + ggrepel::geom_text_repel(
				aes(
					x = gg[id_label != "", get(x)],
					y = gg[id_label != "", y_plot],
					label = gg[id_label != "", id_label]
				),
				data = gg[id_label != ""],
				nudge_x = label_nudgex,
				fontface = "bold",
				size = label_fontsize,
				box.padding = unit(0.4, "lines"),
				segment.color = "black",
				point.padding = unit(0.3, "lines"),
				segment.size = 0.5,
				max.overlaps = 100
			)
		}
		
		p = p + scale_color_manual(values = myfillcolour)
		p = p + scale_fill_manual(values = myfillcolour)
		p = p + theme(legend.position = "none")
		
		return(p)
	}
