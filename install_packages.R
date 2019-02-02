# FIND AND PREPARE TO INSTALL ALL PACKAGES

# february, 2019

pkgs <- installed.packages()

pkgs <- pkgs[,1]

install.packages(pkgs)
