#' Load farse data from csv file
#'
#'  This function will used to import csv file to tydeverse tibble format.
#'  Import will check if file exists. In case that file does not exist, function will stop.
#'
#'
#' @param filename A csv file name that will be imported into tibble
#'
#' @return  This function will return tibble from imported csv
#'
#'
#' @export

fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create name of file from year
#'
#'  Simple function, that will create file name from imput string or
#'  number. In case that input is not numeric, than function changes
#'  type to numeric.
#'
#' @param year
#'
#' @return Returns string that contains file name
#'
#' @examples
#'
#' make_filename('2015')
#' make_filename(2015)
#'
#' @export



make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Function fars_read_years reads in multiple files and creates list of tibbls
#'
#'  Function will take in vector of years, either in string or numbers.
#'  fars_read_years will use functions make_filename, to create file name
#'  to by loaded into list. Function fars_read will be also used in order to
#'  read in cvs files. Final step of the function will be adding new column year
#'  from variable year.
#'
#'  fars_read_years will have check on existance of csv file with year, that was entered
#'  in vector of years.
#'
#' @param years Requires vector of years, that will be imported into list of tibbles
#'
#' @return  Function will return list of tibbles
#'
#' @examples
#'
#' fars_read_years(c(2013, 2014))
#' fars_read_years(c('2013', '2014'))
#'
#' @export

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' Function fars_summarize_years creates summarized version of years and months
#'
#'  This function uses output from fars_read_years which has output of list of
#'  tibbles. Secund step is creating union of all tibbles into one table. Third step
#'  is creating summaries of number combinations by year and month. final step is
#'  to create new table with values of month in columns and number of existing
#'  combinations as values in columns.
#'
#'
#'
#' @param years A character string giving the text the function will print
#'
#' @return  This function will return tibble of values that have number of
#'          combinations of months and years.
#'
#'
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Function fars_map_state reads in fars data for one state and displays number
#' of occurences on the map
#'
#' This function uses two previusly defined functions: make_filename (for creating
#' a name of imported file) and make_filename (for loading csv file from file).
#' In the secund step it verifies if the state.number is in the unique list of
#' states. In case that number is not correct, than execution of function is stoped.
#' Next step is to filter tibble by selected state. Secund check is done in order to
#' verify if there are any accidents in mentioned state. Final step of function is
#' to plot number of accindents on the map by lattitude and longitude
#'
#'
#'
#' @param state.num Unique identifier of state
#' @param year Single year that will be analyzed in map
#'
#' @return This function will return map of selected state with number of occurences
#'         by GPS cordinates with lattitude and longitude.
#'
#'
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
