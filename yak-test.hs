import System.Environment
import Test.DocTest

main = doctest ["-package-conf", "cabal-dev/packages-7.4.1.conf","-package text-0.11.2.0", "yak"]
