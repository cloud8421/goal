DB_FILE := ~/Dropbox/Goals/goals.db

build-dev:
	stack build --flag *:dev
.PHONY: build-dev

build-prod:
	stack build
.PHONY: build-prod

start:
	stack exec goal start -- --database-file $(DB_FILE)
.PHONY: start

start-embedded:
	stack exec goal start -- --database-file $(DB_FILE) --embedded
.PHONY: start

migrate:
	stack exec goal migrate -- --database-file $(DB_FILE)
.PHONY: migrate

db-console:
	sqlite3 $(DB_FILE)
.PHONY: db-console

install:
	stack install
.PHONY: install
