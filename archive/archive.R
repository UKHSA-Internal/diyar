# Up to v.0.4.0
# mths <- c("across", "inbetween", "chain", "reverse", "aligns_start", "aligns_end", "exact", "overlap", "none")
# mths <- sort(mths)
# mths_lst <- lapply(seq_len(length(mths)), function(i){
#   combn(length(mths), i, simplify = FALSE)
# })
# mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
# mths_lst <- lapply(mths_lst, function(x){
#   paste0(mths[x], collapse ="|")
# })
# mths_lst <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)
# mths_lst_cd <- lapply(mths, function(x){
#   if(!x %in% c("overlap", "none")){
#     which(grepl(x, mths_lst) & !grepl("overlap|none", mths_lst))
#   }else if (x == "overlap"){
#     which(grepl(x, mths_lst) & !grepl("none", mths_lst))
#   }else if (x == "none"){
#     which(grepl(x, mths_lst))
#   }
# })
# names(mths_lst_cd) <- mths
# overlap_methods <- list(options = mths_lst, methods = mths_lst_cd)
# save(list = "overlap_methods", file = "data/overlap_methods.RData")

mths <- c("across", "inbetween", "chain", "reverse", "aligns_start", "aligns_end", "exact", "none", "overlap")
mths <- sort(mths)
mths_lst <- lapply(seq_len(length(mths)), function(i){
  combn(length(mths), i, simplify = FALSE)
})
mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
mths_lst <- lapply(mths_lst, function(x){
  paste0(mths[x], collapse ="|")
})
mths_lst_1 <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)

mths <- c("x_across_y", "y_across_x", "x_inbetween_y",
          "y_inbetween_x", "x_chain_y", "y_chain_x",
          "reverse", "aligns_start", "aligns_end",
          "exact", "none", "overlap")
mths <- sort(mths)
mths_lst <- lapply(seq_len(length(mths)), function(i){
  combn(length(mths), i, simplify = FALSE)
})
mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
mths_lst <- lapply(mths_lst, function(x){
  paste0(mths[x], collapse = "|")
})
mths_lst_2 <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)

mths <- c("x_across_y", "y_across_x", "x_inbetween_y",
          "y_inbetween_x", "x_chain_y", "y_chain_x",
          "x_aligns_start_y", "y_aligns_start_x", "x_aligns_end_y",
          "y_aligns_end_x", "reverse",
          "exact", "none", "overlap")
mths <- sort(mths)
mths_lst <- lapply(seq_len(length(mths)), function(i){
  combn(length(mths), i, simplify = FALSE)
})
mths_lst <- unlist(mths_lst, recursive = FALSE, use.names = FALSE)
mths_lst <- lapply(mths_lst, function(x){
  paste0(mths[x], collapse = "|")
})
mths_lst_3 <- unlist(mths_lst, recursive = TRUE, use.names = FALSE)

mths_lst <- c(mths_lst_1, mths_lst_2, mths_lst_3)
mths_lst[grepl("overlap", mths_lst)] <- "overlap"
mths_lst[grepl("none", mths_lst)] <- "none"
mths_lst <- mths_lst[!duplicated(mths_lst)]

mths_lst_cd <- lapply(c(mths, "across", "chain", "inbetween", "aligns_start", "aligns_end"), function(x){
  which(grepl(x, mths_lst))
})
names(mths_lst_cd) <- c(mths, "across", "chain", "inbetween", "aligns_start", "aligns_end")

mths_lst <- list(cd = seq_len(length(mths_lst)), nm = mths_lst)

# Retire reverse overlaps option and codes
retired_codes <- which(grepl("reverse", mths_lst$nm))
mths_lst_cd <- lapply(mths_lst_cd, function(x){
  x[!x %in% retired_codes]
})

mths_lst_cd <- mths_lst_cd[names(mths_lst_cd) != "reverse"]
mths_lst <- lapply(mths_lst, function(x) x[which(!grepl("reverse", mths_lst$nm))])

# Retire overlaps option codes for the now wrapper overlap methods
exc_lgk <- lapply(mths_lst$nm, function(x){
  any(unlist(strsplit(x, "\\|"), use.names = FALSE) %in% c("across", "chain", "inbetween", "aligns_start", "aligns_end"))
})
exc_lgk <- unlist(exc_lgk, use.names = FALSE)

retired_codes <- mths_lst$cd[exc_lgk]
mths_lst_cd <- lapply(mths_lst_cd, function(x){
  x[!x %in% retired_codes]
})
mths_lst <- lapply(mths_lst, function(x) x[which(!exc_lgk)])
mths_lst_cd <- mths_lst_cd[!names(mths_lst_cd) %in% c("across", "chain", "inbetween", "aligns_start", "aligns_end")]

overlap_methods <- list(options = mths_lst, methods = mths_lst_cd)
save(list = "overlap_methods", file = "data/overlap_methods.RData")
