rm -rf src
mkdir -p src/main/scala/psksvp
mkdir -p src/main/resources
cp -R ../CodeWithJVM/src/psksvp/FileSystem src/main/scala/psksvp/.
cp -R ../CodeWithJVM/src/psksvp/QuineMcCluskey.scala src/main/scala/psksvp/.
cp -R ../CodeWithJVM/src/psksvp/Verica src/main/scala/psksvp
cp -R ../CodeWithJVM/src/psksvp/Python src/main/scala/psksvp
cp ../CodeWithJVM/src/psksvp/package.scala src/main/scala/psksvp/.
