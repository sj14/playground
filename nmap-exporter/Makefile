.PHONY: docker-build-push
docker-build-push:
	docker buildx build --platform linux/amd64,linux/arm64 --push --tag ghcr.io/sj14/nmap-exporter:latest .

.PHONY: podman-build-push
podman-build-push:
	podman build --platform linux/amd64 --tag ghcr.io/sj14/nmap-exporter:latest .
	podman push ghcr.io/sj14/nmap-exporter:latest
