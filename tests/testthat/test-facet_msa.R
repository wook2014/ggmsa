library(ggmsa)
library(ggplot2)


test_that("facet_msa preserves geom_polygon data", {
    msa <- system.file("extdata", "sample.fasta", package = "ggmsa")
    plot <- ggmsa(msa, start = 10, end = 20)
    polygon_data <- plot$layers[[2]]$data

    faceted_plot <- plot + facet_msa(field = 5)
    faceted_polygon_data <- faceted_plot$layers[[2]]$data

    expect_s3_class(faceted_plot$layers[[2]]$geom, "GeomPolygon")
    expect_equal(
        faceted_polygon_data[names(polygon_data)],
        polygon_data
    )
    expect_true("facet" %in% names(faceted_polygon_data))
    expect_true("facet" %in% names(faceted_plot$layers[[1]]$data))

    built_plot <- ggplot_build(faceted_plot)
    expect_equal(nrow(built_plot$data[[2]]), nrow(polygon_data))
})
