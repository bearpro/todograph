.PHONY: build build-frontend build-backend dev-frontend dev-backend dev-mongo test-frontend test-backend docker-build docker-up docker-down docker-logs

build: build-frontend build-backend

build-frontend:
	$(MAKE) -C src/app build

build-backend:
	cd src/backend && pnpm build

dev-frontend:
	$(MAKE) -C src/app run-dev

dev-backend:
	cd src/backend && pnpm dev

dev-mongo:
	docker compose up -d mongo

test-frontend:
	cd src/app-test-runner && elm-test ../app-tests/Page/TodoGraph/LayoutTest.elm

test-backend:
	cd src/backend && pnpm test

docker-build:
	docker compose build app

docker-up:
	docker compose up -d

docker-down:
	docker compose down

docker-logs:
	docker compose logs -f app
