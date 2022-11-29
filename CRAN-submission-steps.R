# Locally
devtools::spell_check()            # fix any spelling errors
devtools::check()                  # run local package check

# Check the package under various architectures, using CRAN build configurations
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()
devtools::check_mac_release()
rhub::check_for_cran()

# After all checks complete
devtools::release()                # final checklist, build and submit package

# After package is accepted to CRAN
# - Add a Github release
# - Update version number in DESCRIPTION
# - Add section for next version in NEWS
