package xread

import "io"

type Reader struct {
	err            error
	originalReader io.Reader
	originalP      []byte
}

func New(r io.Reader, p []byte) *Reader {
	return &Reader{originalReader: r, originalP: p}
}

// Read is an attempt for solving https://github.com/golang/go/issues/40385#issuecomment-663775687,
// thus you can process the error before processing n.
func (re *Reader) Read() (int, error) {
	if re.err != nil {
		// Return the error from the previous call.
		return 0, re.err
	}

	var n int
	n, re.err = re.originalReader.Read(re.originalP)
	if n > 0 {
		// Do not immediately return an error when bytes were read.
		return n, nil
	}

	// No bytes were read, return whatever Read() returned.
	return n, re.err
}

// ReadStrict is like Read() but always returns an io.EOF error when no bytes were read and there would be no error otherwise.
func (re *Reader) ReadStrict(r io.Reader, p []byte) (int, error) {
	n, err := re.Read()
	if n <= 0 && err == nil {
		return n, io.EOF
	}
	return n, err
}
