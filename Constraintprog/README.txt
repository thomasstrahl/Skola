!!! Run initcs first

1. Execution of JaCoP programs for command line

javac Main.java
java Main

System variable CLASSPATH is set to both jacop.jar and jdom.jar.

2. Execution of minizinc models

a) compilation to flatzinc with JaCoP library

mzn2fzn -G jacop model.mzn

b) Execution using JaCoP library

jzinc [options] model.fzn

To get a list of available options use jzinc -h.
 
