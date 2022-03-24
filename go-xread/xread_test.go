package xread_test

import (
	"io"
	"math/rand"
	"testing"

	"github.com/sj14/playground/go-xread"
	"github.com/stretchr/testify/require"
)

type testReader struct {
	calls         int
	errAfterCalls int
	errType       error
}

func (tr *testReader) Read(p []byte) (n int, err error) {
	tr.calls++
	if tr.calls > tr.errAfterCalls {
		return rand.Intn(1), tr.errType
	}
	return rand.Int(), nil
}

func TestReadUnordered(t *testing.T) {
	reader0 := &testReader{errAfterCalls: 3, errType: io.EOF}
	reader1 := &testReader{errAfterCalls: 1, errType: io.ErrClosedPipe}

	_, err := xread.New(reader0, nil).Read()
	require.NoError(t, err)

	_, err = xread.New(reader1, nil).Read()
	require.NoError(t, err)

	_, err = xread.New(reader0, nil).Read()
	require.NoError(t, err)

	_, err = xread.New(reader1, nil).Read()
	require.EqualError(t, io.ErrClosedPipe, err.Error())

	_, err = xread.New(reader0, nil).Read()
	require.NoError(t, err)

	_, err = xread.New(reader1, nil).Read()
	require.EqualError(t, io.ErrClosedPipe, err.Error())

	_, err = xread.New(reader0, nil).Read()
	require.EqualError(t, io.EOF, err.Error())
}
