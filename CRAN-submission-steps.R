# Locally
devtools::spell_check()            # fix any spelling errors[DONE]
devtools::check()                  # run local package check [DONE]

# Check the package under various architectures, using CRAN build configurations
devtools::check_win_devel() # Tested
devtools::check_win_release() # Tested
devtools::check_win_oldrelease() # Tested
devtools::check_mac_release() # Tested
rhub::check_for_cran() # Tested
#rhub::check_on_windows()
#rhub::check_on_debian()


# After all checks complete
devtools::release()                # final checklist, build and submit package

# After package is accepted to CRAN
# - Add a Github release
# - Update version number in DESCRIPTION
# - Add section for next version in NEWS
