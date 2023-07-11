import os
import subprocess
import rpy2.robjects as robjects

# Configuration for pkgdown documentation
robjects.r('pkgdown::build_site()')

# Sphinx configuration
extensions = []
html_theme = "alabaster"
html_static_path = ['docs']
master_doc = 'index'
