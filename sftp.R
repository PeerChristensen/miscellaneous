

library(RCurl)
#x = scp("remote.ssh.host.com", "/home/dir/file.txt", "My.SCP.Passphrase", user="username")
user="cdep4769geom"
pw = "MMK123Conzoom09"

x = scp(host="cdep4769geom@st.uk.experian.com", keypasswd = "MMK123Conzoom09",
        key = "C:/Users/pch/Documents/conzoom.priv.key.rsa")


library(ssh)
#ssh_connect(host, keyfile = NULL, passwd = askpass, verbose = FALSE)
ssh_connect(host = "cdep4769geom@st.uk.experian.com", keyfile = "C:/Users/pch/Documents/conzoom", 
            passwd = "MMK123Conzoom09", verbose = T)


"C:/Users/pch/Documents/conzoom"
"C:\\Users\\pch\\Documents\\conzoom"

import pysftp

srv = pysftp.Connection(host="st.uk.experian.com", username="cdep4769geom", private_key = "C:\\Users\\pch\\Documents\\conzoom", private_key_pass="MMK123Conzoom09")

# Get the directory and file listing

sftp -i conzoom cdep4769geom@st.uk.experian.com
data = srv.listdir()

# Closes the connection
#srv.close()