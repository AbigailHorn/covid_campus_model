
# STEP 1: Install through brew

# STEP 2: Find through `locate` command terminal
  # Found at /usr/local/var/homebrew/linked/jags/include/

# STEP 3: Configure R installation files to find the jags installation
install.packages("rjags",
                      configure.args ="--with-jags-include=/usr/local/var/homebrew/linked/jags/include/JAGS --with-jags-lib=/usr/local/var/homebrew/linked/jags/lib"
)
