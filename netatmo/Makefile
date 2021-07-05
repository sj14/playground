.PHONY: build-api
build-api:
	docker buildx build --platform linux/amd64,linux/arm64 --push --tag ghcr.io/sj14/netatmo-api:latest api

.PHONY: build-csv
build-csv:
	docker buildx build --platform linux/amd64,linux/arm64 --push --tag ghcr.io/sj14/netatmo-csv:latest csv

