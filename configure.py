#!/usr/bin/env  python

import glob
import os
import os.path
import string
import subprocess

config_tmpl = string.Template(
"""abstract project Config is
   ZMQ_Library_Dir := "$zmqlibdir";
end Config;
""");

LIB_DIR_SEARCH_LIST = [
    "%s/lib" % (os.environ['HOME']),
    "/usr/local/lib",
    "/lib",
    "/usr/lib"]

def system_lib_dirs():
    for root in LIB_DIR_SEARCH_LIST:
        if os.path.isdir(root):
            yield root
        for x in glob.glob(root+"*"):
            if x != root and os.path.isdir(x) and os.path.basename(x) != "libexec":
                yield x

def pkg_config_lib_dir(name):
    return subprocess.check_output(["pkg-config", name, "--variable=libdir"], stderr=file(os.devnull)).strip()

def search_lib_dirs(name):
    for libdir in system_lib_dirs():
        poss = [x for x in os.listdir(libdir) if os.path.isfile(os.path.join(libdir,x)) and x.split(".")[0] == name]
        if len(poss) > 0:
            return libdir
    raise Exception("%s not found in lib dirs"%name)

def get_lib_dir(name):
    try:
        return pkg_config_lib_dir(name)
    except Exception, e:
        pass
    try:
        return search_lib_dirs(name)
    except:
        pass
    raise Exception("%s: could not find in pkg-config or %s"%(name, LIB_DIR_SEARCH_LIST))

def main():
    root_path = os.path.dirname(os.path.abspath(__file__))
    with open("config.gpr", "w") as f:
        f.write(config_tmpl.substitute({"zmqlibdir":get_lib_dir("libzmq")}))

if __name__ == "__main__":
    main()
