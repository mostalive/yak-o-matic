import System.Environment
import Test.DocTest

main = doctest ["-no-user-package-conf","-package-conf", "cabal-dev/packages-7.4.1.conf", "yak"]
