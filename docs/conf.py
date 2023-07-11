import os
import subprocess

# Configuration for pkgdown documentation
subprocess.call(["R", "-e", "pkgdown::build_site()"])

# Sphinx configuration
extensions = []
html_theme = "alabaster"
html_static_path = ['docs']
master_doc = 'index'
