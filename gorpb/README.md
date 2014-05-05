## Usage

    package mylib

    import "github.com/basho/riak_pb/gorpb" // riak_pb is now available

    func main() {
        foo := riak_pb.RpbFoo{
           Bar: []byte("Baz"),
        }
        // Do stuff with foo
    }

Built with [goprotobuf](https://code.google.com/p/goprotobuf/).
