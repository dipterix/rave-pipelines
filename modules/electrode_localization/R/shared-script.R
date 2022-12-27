library(dipsaus)
fastmap2 <- dipsaus::fastmap2
parse_svec <- dipsaus::parse_svec
drop_nulls <- dipsaus::drop_nulls
`%OF%` <- dipsaus::`%OF%`

LOCALIZATION_METHODS <- list(
  "Re-sampled CT" = "resampled",
  "FSL transform + Raw CT + MRI" = "fsl",
  "CT (IJK) to MR (RAS) transform + Raw CT" = "ijk2ras",
  "Localize without CT" = "no_ct"
)
