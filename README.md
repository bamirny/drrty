# DRRTY

* Test your installation

   docker run hello-world

  If this fails, you will need to correct your installation.

* Move to where you unpacked the microc source:

  cd microc

* Invoke docker

docker run --rm -it -v `pwd`:/home/drrty -w=/home/drrty columbiasedwards/plt
