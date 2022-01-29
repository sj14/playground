package xread

import "io"

var err error

// Read is an attempt for solving https://github.com/golang/go/issues/40385#issuecomment-663775687,
// thus you can process the error before processing n.
func Read(r io.Reader, p []byte) (int, error) {
	if err != nil {
		// Return the error from the previous call.
		return 0, err
	}

	var n int
	n, err = r.Read(p)
	if n > 0 {
		// Do not immediately return an error when bytes were read.
		return n, nil
	}

	// No bytes were read, return whatever Read() returned.
	return n, err
}

// ReadStrict is like Read() but always returns an io.EOF error when no bytes were read.
func ReadStrict(r io.Reader, p []byte) (int, error) {
	n, err := Read(r, p)
	if n <= 0 && err == nil {
		return n, io.EOF
	}
	return n, err
}
