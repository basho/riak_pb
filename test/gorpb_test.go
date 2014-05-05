package test

import "testing"
import "github.com/riaken/riak_pb/gorpb"

func TestGorpb(t *testing.T) {
	req := riak_pb.RpbGetReq{}
	req.Bucket = []byte("bucket")
}
