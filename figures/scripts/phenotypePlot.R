function (d, max.y, max.x, suggestive.line, significant.line, 
    size.x.labels = 9, size.y.labels = 9, switch.axis = F, sort.by.value = F, 
    sort.by.category.value = F, base.labels = F, annotate.phenotype.description, 
    annotate.angle = 0, annotate.size = 5, annotate.level, annotate.phenotype = F, 
    annotate.snp.w.phenotype = F, annotate.snp = F, annotate.snp.angle = 0, 
    annotate.list, annotate.only.largest = T, lc.labels = F, 
    x.group.labels = T, x.phenotype.labels = F, sizes = F, direction = F, 
    point.size = 3, use.color = T, color.palette, title = paste0("Phenotype Plot ", 
        date()), x.axis.label = "Phenotypes", y.axis.label = "Values", 
    y.axis.interval = 5) 
{
    if (sum(c("phenotype", "value") %in% names(d)) < 2) 
        stop("Data input must contain columns phenotype and value.")
    if (!missing(annotate.phenotype.description)) {
        if (is.data.frame(annotate.phenotype.description) & sum(c("phenotype", 
            "description") %in% names(annotate.phenotype.description)) == 
            2) {
            d = merge(d, annotate.phenotype.description)
            annotate.phenotype.description = T
        }
        else if (is.logical(annotate.phenotype.description)) {
            if (annotate.phenotype.description == T & !length(d$description)) {
                stop("Annotate.phenotype.description must contain columns phenotype and description, or be TRUE with provided d$description.")
            }
        }
        else {
            stop("Annotate.phenotype.description must contain columns phenotype and description, or be TRUE with provided d$description.")
        }
    }
    else {
        annotate.phenotype.description = F
    }
    if ((annotate.snp | annotate.snp.w.phenotype) & !("snp" %in% 
        names(d))) 
        stop("You requested SNP annotation but d$snp is not defined.")
    if (annotate.snp.w.phenotype & annotate.snp) 
        warning("You requested SNP annotation twice")
    annotate = annotate.phenotype.description | annotate.phenotype | 
        annotate.snp | annotate.snp.w.phenotype
    if (annotate & missing(annotate.level) & missing(annotate.list)) {
        warning("You requested annotation, but did not specify annotate.level or annotate.list.")
        annotate = F
    }
    if (!use.color & !missing(color.palette)) 
        stop("You requested no color, but provided a color palette.")
    if (use.color) {
        if (missing(color.palette)) {
            if (!("color" %in% names(d))) 
                stop("You requested color, but did not provide a color attribute in d or color.palette")
            else if (class(d$color) == "factor") 
                warning("The color attribute is a factor and no color palette is provided: R default color scheme will be used. Convert color to character if it contains color names or codes")
        }
        if (!missing(color.palette) & !length(d$color)) {
            if (length(d$groupnum)) 
                d$color = d$groupnum
            else stop("You requested use.color, but d$color or d$groupnum were not provided to distinguish groups.")
        }
    }
    else {
        d$color = "#000000"
    }
    if (sizes & !length(d$size)) 
        stop("You requested size information, but did not provide d$size")
    if (direction & !length(d$direction)) 
        stop("You requested direction information, but did not provide d$direction")
    if (!sizes) 
        d$size = point.size
    if (!annotate.phenotype.description & !annotate.phenotype & 
        !annotate.snp & !annotate.snp.w.phenotype) 
        lc.labels = F
    if (sort.by.value & x.group.labels) 
        stop("One cannot sort universally by value and have x group labels. Try sort.by.category.value or not labeling the groups.")
    if ((sort.by.category.value | x.group.labels) & (sum(c("groupnum", 
        "group") %in% names(d)) < 2)) {
        stop("Requested group information, but did not provide d$groupnum and d$group.")
    }
    if (!("groupnum" %in% names(d))) {
        d$groupnum = 0
    }
    d = d[!(is.na(d$phenotype) | is.na(d$value)), ]
    d = d[order(d$phenotype), ]
    if (missing(max.x)) 
        max.x = length(unique(d$phenotype))
    phenotypes = aggregate(value ~ phenotype + groupnum, d, FUN = max)
    phenotypes = phenotypes[order(phenotypes$value, decreasing = T), 
        ][1:min(nrow(phenotypes), max.x), ]
    if (sort.by.value) {
        phenotypes = phenotypes[order(phenotypes$value, decreasing = T), 
            ]
    }
    else if (sort.by.category.value) {
        phenotypes = phenotypes[order(-phenotypes$groupnum, phenotypes$value, 
            decreasing = T), ]
    }
    else {
        phenotypes = phenotypes[order(phenotypes$groupnum, phenotypes$phenotype), 
            ]
    }
    phenotypes$seq = 1:nrow(phenotypes)
    phenotypes = phenotypes[, c("phenotype", "seq", "value")]
    names(phenotypes)[3] = "min.value"
    d = inner_join(phenotypes, d, by = "phenotype")
    d = d[order(d$seq), ]
    d <- d %>% mutate(description = case_when(description == 
        "Develomental delays and disorders" ~ "Developmental delays and disorders", 
        description == "Mental retardation" ~ "Intellectual Disabilities", 
        description == "Other persistent mental disorders due to conditions classified elsewhere" ~ 
            "Other persistent mental disorders", description == 
            "Skull and face fracture and other intercranial injury" ~ 
            "Intercranial injury", TRUE ~ as.character(description)))
    if (missing(max.y)) 
        max.y = ceiling(max(d$value))
    if (switch.axis) {
        d = d[nrow(d):1, ]
        d$groupnum = max(d$groupnum) - d$groupnum + 1
        d$seq = max(d$seq) - d$seq + 1
    }
    if (x.group.labels) {
        labels = summarize(group_by(d, groupnum), tick = mean(unique(seq)), 
            label = as.character(group[1]))
        labels = labels[order(labels$tick), ]
    }
    if (missing(color.palette)) {
        color.palette = unique(d[order(d$seq), ]$color)
        names(color.palette) = color.palette
    }
    else {
        names(color.palette) = unique(d[order(d$seq), ]$color)
    }
    if (!switch.axis) {
        plot = ggplot(d, ylab = y.axis.label, xlab = x.axis.label)
        if (!missing(suggestive.line) & !is.na(suggestive.line)) 
            plot = plot + geom_hline(yintercept = suggestive.line, 
                colour = "red", alpha = I(1/3), size = 1)
        d <- d %>% mutate(annotate = ifelse(phenotype %in% annotate.list, 
            TRUE, FALSE))
        d <- d %>% mutate(fill = case_when(!annotate ~ 0.5, TRUE ~ 
            1))
        #print(head(d))
        #print(unique(d$color))
        color.palette = unique(d[order(d$seq), ]$color)
        names(color.palette) = color.palette
        plot = plot + aes(seq, value, size = size, colour = color)
        if (!sizes) 
            plot = plot + scale_size(range = c(point.size, point.size), 
                guide = "none")
        plot = plot + geom_point()
        plot = plot + scale_colour_manual(values = color.palette, 
            guide = "none")
        plot = plot + scale_alpha_manual(values = fill, guide = F)
        if (x.group.labels) {
            plot = plot + scale_x_continuous(name = x.axis.label, 
                limits = c(1, max.x), breaks = labels$tick, labels = labels$label, 
                expand = c(0.01, 0))
        }
        else {
            plot = plot + scale_x_continuous(name = x.axis.label, 
                limits = c(1, max.x), breaks = c(-100), labels = c(""), 
                expand = c(0.015, 0))
        }
        plot = plot + scale_y_continuous(y.axis.label, limits = c(0, 
            max.y), breaks = seq(0, max.y, y.axis.interval), 
            expand = c(0, 0.2))
        plot = plot + theme(panel.background = element_blank(), 
            panel.grid.minor = element_blank(), axis.text.x = element_text(size = size.x.labels, 
                colour = "black", angle = 40, hjust = 0.7, vjust = 0.7), 
            axis.text.y = element_text(size = size.y.labels, 
                colour = "black"), axis.line = element_line(colour = "black"), 
            axis.ticks = element_line(colour = "black"))
    }
    else {
        plot = ggplot(d, xlab = y.axis.label, ylab = x.axis.label)
        if (!missing(suggestive.line) & !is.na(suggestive.line)) 
            plot = plot + geom_vline(xintercept = suggestive.line, 
                colour = "blue", alpha = I(1/3), size = 1)
        if (!missing(significant.line) & !is.na(significant.line)) 
            plot = plot + geom_vline(xintercept = significant.line, 
                colour = "red", alpha = I(1/3), size = 1)
        plot = plot + aes(value, seq, size = size, colour = color)
        if (!sizes) 
            plot = plot + scale_size(range = c(point.size, point.size), 
                guide = "none")
        plot = plot + geom_point()
        plot = plot + scale_colour_manual(values = color.palette, 
            guide = "none")
        if (x.group.labels) {
            plot = plot + scale_y_continuous(name = x.axis.label, 
                limits = c(1, max.x), breaks = labels$tick, labels = labels$label, 
                expand = c(0.015, 0.02))
        }
        else {
            plot = plot + scale_y_continuous(name = x.axis.label, 
                limits = c(0, max.x), breaks = c(-100), labels = c(""), 
                expand = c(0.015, 0))
        }
        plot = plot + scale_x_continuous(y.axis.label, limits = c(0, 
            max.y), breaks = seq(0, max.y, y.axis.interval), 
            expand = c(0, 0.2))
        plot = plot + theme(panel.background = element_blank(), 
            panel.grid.minor = element_blank(), axis.text.y = element_text(size = size.x.labels, 
                colour = "black", hjust = 1, vjust = 0.5), axis.text.x = element_text(size = size.y.labels, 
                colour = "black", hjust = 0.5, vjust = 0), axis.line = element_line(colour = "black"), 
            axis.ticks = element_line(colour = "black"))
    }
    plot = plot + theme(legend.position = "none")
    if (sizes) {
        plot = suppressWarnings(plot + theme(legend.position = "right") + 
            scale_size("Size", range = c(point.size, 2 * point.size)))
    }
    if (direction) {
        plot = plot + aes(shape = factor(direction), fill = color) + 
            scale_shape("Direction", solid = TRUE) + scale_shape_manual(values = c(25, 
            24)) + scale_fill_manual(values = color.palette, 
            guide = "none")
    }
    if (annotate) {
        d$annotate = F
        if (!missing(annotate.list)) 
            d[d$phenotype %in% annotate.list, ]$annotate = T
        if ((!missing(annotate.level)) & (sum(d$value >= annotate.level) > 
            0)) 
            d[d$value >= annotate.level, ]$annotate = T
        if (sum(d$value != d$min.value) > 0 & annotate.only.largest) 
            d[d$value != d$min.value, ]$annotate = F
        if (!annotate.phenotype.description) {
            d$description = ""
        }
        if (annotate.phenotype) 
            d$description = paste(d$phenotype, ifelse(d$description == 
                "", "", paste(":", d$description)), sep = "")
        if (annotate.snp.w.phenotype) 
            d$description = paste(d$snp, ifelse(d$description == 
                "", "", paste(":", d$description)), sep = "")
        d$description = substr(d$description, 1, 60)
        if (lc.labels) 
            d$description = tolower(d$description)
        if (sum(d$annotate) == 0) {
            warning("Annotation requested, but no points met criteria")
        }
        else {
            if (annotate.phenotype.description | annotate.phenotype | 
                annotate.snp.w.phenotype) {
                #print(head(d))
                plot = plot + if (!base.labels) {
                  ggrepel::geom_label_repel(aes(label = description), 
                    colour = "black", data = d[d$annotate, ], 
                    size = annotate.size, angle = annotate.angle, 
                    min.segment.length = 0, force = 100, force_pull = 1, 
                    max.iter = 20000, max.overlaps = 30, box.padding = unit(0.2, 
                      "lines"), point.padding = unit(0.2, "lines"), 
                    fill = alpha(d[which(d$annotate), ]$color, 
                      0.5), segment.curvature = -0.5, segment.ncp = 10, 
                    arrow = arrow(length = unit(0.015, "npc")), 
                    segment.angle = 40, segment.shape = 1, segment.linetype = 6, 
                    segment.alpha = 0.5)
                }
                else {
                  geom_text(aes(label = description), colour = "black", 
                    data = d[d$annotate, ], hjust = 0, size = annotate.size, 
                    angle = annotate.angle)
                }
            }
            if (annotate.snp) {
                plot = plot + if (!base.labels) {
                  ggrepel::geom_text_repel(aes(label = snp), 
                    colour = "black", data = d[d$annotate, ], 
                    size = annotate.size, angle = annotate.snp.angle)
                }
                else {
                  geom_text(aes(label = snp), colour = "black", 
                    data = d[d$annotate, ], hjust = 0, size = annotate.size, 
                    angle = annotate.snp.angle)
                }
            }
        }
    }
    plot = plot + labs(title = title) + theme(title = element_text(size = 13))
    plot
}
