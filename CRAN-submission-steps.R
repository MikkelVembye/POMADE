# Locally
devtools::spell_check()            # fix any spelling errors - still 9 false positives
devtools::check()                  # run local package check [DONE]

# Check the package under various architectures, using CRAN build configurations
devtools::check_win_devel() # Tested 1 note about maintainer
devtools::check_win_release() # Tested 1 note about maintainer
devtools::check_win_oldrelease() # Tested 1 note about maintainer
devtools::check_mac_release() # Tested Succeeded
rhub::check_for_cran() # Tested note about maintainer
rhub::check_on_debian() # Succeeded
revdepcheck::revdep_check(num_workers = 4)

# After all checks complete
devtools::release()                # final checklist, build and submit package

# After package is accepted to CRAN
# - Add a Github release
# - Update version number in DESCRIPTION MHV: To 0.2.0.9999?
# - Add section for next version in NEWS MHV: 0.2.0?
