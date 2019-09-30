# If This Test fails

May need to reprint the all-supported.proto type from the "print protofile" test located in
the [ProtobufScalacheckTest](../protobuf/src/test/scala/com/bones/protobuf/ProtobufScalacheckTest.scala)
and then recreate the java classes with the command: 

```bash
protoc -I=src/main/java --java_out=src/main/java --proto_path=. all-supported.proto
```

protoc can be downloaded at the [protocol-buffers](https://developers.google.com/protocol-buffers/docs/downloads) website.

# About

This project serves as an integration test between the Bones Protobuf implementation and the Java Protobuf implementation
to ensure comparability.



