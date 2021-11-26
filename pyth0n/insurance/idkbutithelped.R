
library(reticulate)
os <- import("os")
os$listdir(".")


use_python("/Users/Silas/AppData/Local/r-miniconda/envs/r-reticulate/python.exe")
use_virtualenv("myenv")


Sys.setenv(RETICULATE_PYTHON = "my-reticulated-project/bin/python")
reticulate::py_config()

py_install("python-igraph", pip = T)
import("igraph")



conda_list()
use_condaenv("3.10", required = TRUE)
pyconfig()

Sys.getenv("RETICULATE_PYTHON")
reticulate::repl_python()
