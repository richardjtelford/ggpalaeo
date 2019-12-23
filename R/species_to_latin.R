#' Italicise species names
#' @description Italicises species names, but not modifiers such as "var."
#' @param x character; one or more species names to be italicised
#' @param families logical; should family names be italicised
#' @param modifiers character; vector of taxonomic terms not to italicise
#' @param etc character; vector of other words that should not be italicised
#' @param modifiers_with_dot logical, should modifiers end in a dot. Use NA to accept modifiers either with or without a trailing dot.
#' @param postfix character; text appended to species names that should not be italicised
#' @param first_with_dot_only logical;
#' @param first_capital_only logical; if TRUE only the first word starting with a capital letter is italicised
#' @param no_numbers logical; if TRUE any words with numbers are not italicised
#' @param no_extra_capitals logical; if TRUE any words with capitals other than first letter are not italicised
#' @param text logical; TRUE gives output as character vector; FALSE gives list of expressions
#' @examples
#' species_to_italics(c("Navicula spp.", "Navicula spp", "Poaceae"))
#' library(ggplot2)
#' data(SWAP, package = "rioja")
#' SWAP$spec %>% mutate(pH = SWAP$pH) %>%
#' pivot_longer(cols = -pH, names_to = "taxa", values_to = "percent") %>%
#'   left_join(as_tibble(SWAP$names), by = c("taxa" = "CODE")) %>%
#'   group_by(taxa) %>%
#'   filter(sum(percent > 0) > 60, max(percent > 20)) %>%
#'   ggplot(aes(x = pH, y = percent)) +
#'   geom_point() +
#'   facet_wrap(~TaxonName,
#'              labeller = as_labeller(
#'                species_to_italics,
#'                default = label_parsed))
#' @importFrom stringr str_split str_detect str_replace_all str_remove_all str_replace
#' @importFrom purrr pluck map map_chr
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @export

species_to_italics <- function(
  x,
  families = TRUE,
  modifiers = c("spp", "sp", "fo", "var", "agg"),
  postfix = c("-type", "-t"),
  etc = character(0),
  first_with_dot_only = TRUE,
  first_capital_only = TRUE,
  no_numbers = TRUE,
  no_extra_capitals = TRUE,
  text = TRUE){

  #postfix
  postfix <- paste0(postfix, collapse = "|")
  postfix <- paste0("(.*)(", postfix, ")")

  italisiser <- function(xx,
                         families,
                         modifiers, etc, postfix,
                         first_with_dot_only,
                         first_capital_only,
                         no_numbers,
                         no_extra_capitals){
    #escape problem characters
    problem <- "([~\\[\\]\\*\\+\\?\\,])"
    xx <- str_replace_all(xx, problem, "*'\\1'*" )

    #Split string
    xx <- xx %>% str_split(" ") %>% pluck(1)

    #remove any leading/trailing ~*
    xx <- str_remove_all(xx, "^[~\\*]+|[~\\*]+$")

    #position nr
    n <- seq_along(xx)

    #italicise as required
    xx <- case_when(
      families & str_detect(xx, "(aceae)|(idae)$") ~ xx,
      first_with_dot_only & str_detect(xx, "\\.") & n > 1 ~ xx,
      first_capital_only & str_detect(xx, "$[A-Z]")  & n > 1 ~ xx,
      no_numbers & str_detect(xx, "\\d") ~ xx,
      no_extra_capitals & str_detect(xx, "^.+[A-Z]") ~ xx,
      str_detect(xx, postfix) ~ str_replace(xx, postfix, "italic(\\1)\\2"),
      xx %in% c(modifiers, etc) ~ xx,
      TRUE ~ paste0("italic(", xx, ")")
    ) %>%
      paste0(collapse = "~")

    return(xx)
  }

  result <- map_chr(x, italisiser,
                    families = families,
                    modifiers = modifiers,
                    etc = etc,
                    postfix = postfix,
                    first_with_dot_only = first_with_dot_only,
                    first_capital_only = first_capital_only,
                    no_numbers = no_numbers,
                    no_extra_capitals = no_extra_capitals)

  if(text){
    return(result)
  }

  expr <- result %>%
    map(~parse(text = .x))
  return(expr)
}

