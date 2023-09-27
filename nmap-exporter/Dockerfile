FROM golang:1 AS builder
WORKDIR /go/src/github.com/sj14/playground/nmap-exporter
COPY . ./
RUN go mod download
RUN CGO_ENABLED=0 go build -o app .

FROM alpine:latest  
RUN apk --no-cache add -U nmap
WORKDIR /root/
COPY --from=builder /go/src/github.com/sj14/playground/nmap-exporter/app ./
CMD ["./app"]
