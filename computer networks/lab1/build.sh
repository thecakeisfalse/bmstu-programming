#!/bin/bash

export GOPATH=`pwd`

cp src/proto/* src/server/
cp src/proto/* src/client/

cd src/server
go get github.com/mgutz/logxi/v1
go get github.com/skorobogatov/input
go build -o ../../bin/server
cd ../..

cd src/client
go get github.com/mgutz/logxi/v1
go get github.com/skorobogatov/input
go build -o ../../bin/client
cd ../..
