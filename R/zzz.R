
.onLoad <- function(libname, pkgname) {
  packageStartupMessage(
    "===============================================================================\n",
    "   _____                         ____            _       ____     ___     ____ \n",
    "  | ____|   __ _   ___   _   _  |  _ \\   _   _  | |__   |  _ \\   / _ \\   / ___|\n",
    "  |  _|    / _` | / __| | | | | | |_) | | | | | | '_ \\  | |_) | | | | | | |    \n",
    "  | |___  | (_| | \\__ \\ | |_| | |  __/  | |_| | | |_) | |  _ <  | |_| | | |___ \n",
    "  |_____|  \\__,_| |___/  \\__, | |_|      \\__,_| |_.__/  |_| \\_\\  \\___/   \\____|\n",
    "                        |___/                                                  \n",
    "                                                                               \n",
    "      EasyPubROC: Simplify ROC analysis and visualization                      \n",
    "      Version: ", as.character(utils::packageVersion(pkgname)), "              \n",
    "                                                                               \n",
    "      Feedback and contributions are welcome!                                  \n",
    "      If you have any suggestions or issues, please contact us at:             \n",
    "      tiandonglee@gmail.com                                                    \n",
    "==============================================================================="
  )
}
