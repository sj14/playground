// based on https://www.socketloop.com/tutorials/golang-recaptcha-example
package recaptcha

import (
	"encoding/json"
	"errors"
	"io/ioutil"
	"net"
	"net/http"
	"net/url"
	"time"
)

type ResponseV2 struct {
	Success     bool      `json:"success"`
	ChallengeTS time.Time `json:"challenge_ts"` // timestamp of the challenge load (ISO format yyyy-MM-dd'T'HH:mm:ssZZ)
	Hostname    string    `json:"hostname"`     // the hostname of the site where the reCAPTCHA was solved
	ErrorCodes  []int     `json:"error-codes"`  // optional
}

type ResponseV3 struct {
	Score  float64 `json:"score"`
	Action string  `json:"action"`
	ResponseV2
}

func VerifyV2(secret string, r *http.Request) (*ResponseV2, error) {
	body, err := verify(secret, r)
	if err != nil {
		return nil, err
	}

	var resp ResponseV2
	if err := json.Unmarshal(body, &resp); err != nil {
		return nil, err
	}

	return &resp, nil
}

// TODO: untested
func VerifyV3(secret string, r *http.Request) (*ResponseV3, error) {
	body, err := verify(secret, r)
	if err != nil {
		return nil, err
	}

	var resp ResponseV3
	if err := json.Unmarshal(body, &resp); err != nil {
		return nil, err
	}

	return &resp, nil
}

var ErrNoRecaptcha = errors.New("missing recaptcha response in request")

func verify(secret string, r *http.Request) ([]byte, error) {
	response := r.FormValue("g-recaptcha-response")
	if response == "" {
		return nil, ErrNoRecaptcha
	}

	remoteip, _, err := net.SplitHostPort(r.RemoteAddr)
	if err != nil {
		return nil, err
	}

	verifyData := url.Values{
		"secret":   {secret},   // private key
		"response": {response}, // response from the client to verify
		"remoteip": {remoteip}, // client ip (optional)
	}

	jsonResp, err := http.PostForm("https://www.google.com/recaptcha/api/siteverify", verifyData)
	if err != nil {
		return nil, err
	}
	defer jsonResp.Body.Close()

	body, err := ioutil.ReadAll(jsonResp.Body)
	if err != nil {
		return nil, err
	}

	return body, nil
}
