package hanlder

import "net/http"

// https://stackoverflow.com/questions/21936332/idiomatic-way-of-requiring-http-basic-auth-in-go
func BasicAuth(wantUser, wantPass string, fn http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		user, pass, _ := r.BasicAuth()
		if user != wantUser || pass != wantPass {
			w.Header().Set("WWW-Authenticate", `Basic realm=""`)
			http.Error(w, "Unauthorized.", http.StatusUnauthorized)
			return
		}
		fn(w, r)
	}
}
