# To create java classes:

protoc -I=src/main/java --java_out=src/main/java --proto_path=. all-supported.proto


# About

This project surves as an integration test between the Bones Protobuf implementation and the Java Protobuf implementation
to ensure compatability.



