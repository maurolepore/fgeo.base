# fgeo --------------------------------------------------------------------

# Pick/drop rows with matching conditions
fgeo.base::drop_status()
fgeo.base::pick_status()
fgeo.base::pick_plotname()
fgeo.base::pick_dbh

# Data (fgeo)
fgeo.base::luquillo_stem_random_tiny
fgeo.base::luquillo_vft_4quad

# Edit dataframe columns or vectors
fgeo.base::collapse_censusid()



# gral --------------------------------------------------------------------

# NAs / sanitize vector / conditions
fgeo.base::abort_na()
fgeo.base::inform_na()
fgeo.base::warn_na()
# df, list, matrix, vector
fgeo.base::fill_na()

# Check
fgeo.base::check_crucial_names()
fgeo.base::check_unique()
fgeo.base::check_unique_vector()



# Pick/drop rows with matching conditions
fgeo.base::drop_if_na()  # rename to drop_na()


# Edit dataframe columns or vectors
# Edit strings
fgeo.base::str_suffix_match()  # remove str_
fgeo.base::str_to_tidy_names()  # remove str_
# Collapse add collapse_ or synonym.
fgeo.base::commas()
fgeo.base::or()
fgeo.base::regex_line()







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
