# fgeo --------------------------------------------------------------------

# Pick/drop rows with matching conditions
fgeo.base::drop_status()
fgeo.base::pick_status()
fgeo.base::pick_plotname()
fgeo.base::pick_dbh

# Edit dataframe columns or vectors
fgeo.base::collapse_censusid()

# Data (fgeo)
fgeo.base::luquillo_stem_random_tiny
fgeo.base::luquillo_vft_4quad

# gral --------------------------------------------------------------------

# NAs / sanitize vector / conditions
fgeo.base::abort_na()
fgeo.base::inform_na()
fgeo.base::warn_na()
# df, list, matrix, vector



# Check
fgeo.base::check_crucial_names()
fgeo.base::check_unique()
fgeo.base::check_unique_vector()



# Pick/drop rows with matching conditions
fgeo.base::drop_if_na()  # rename to drop_na()



# Edit data (dataframe columns, vectors, lists or matrice).
fgeo.base::fill_na()
# Edit strings (add str_ to all or remove)
fgeo.base::str_suffix_match()  # TODO: remove str_?
fgeo.base::str_to_tidy_names()  # TODO: remove str_? compare to tibble::set_tidy_names()?
# Collapse add collapse_ or TODO: Search for a short synonym.
fgeo.base::commas()
fgeo.base::or()
fgeo.base::regex_line()  # enline







# Logical tests (general)
fgeo.base::exists_in_pkg()
fgeo.base::is_named()



# Find / search (general)
fgeo.base::find_datasets()



# Restructure data
fgeo.base::gather_matrix()  # rename to mtx
fgeo.base::gather_matrix_ls()  # rename to mtxs

# Guess / summary / approximate
fgeo.base::round_any()
fgeo.base::guess_max()
fgeo.base::guess_plotdim()

# Names
fgeo.base::name_df_lst()  # Rename to _dfs or _ls()
fgeo.base::nms_extract_match()





# git ---------------------------------------------------------------------

# From .R send to terminal: control + alt + enter
git checkout master
git merge dev
git pull
git push
git checkout dev
