DB_FILE := ~/Dropbox/Goals/goals.db

start:
	stack exec goal start -- --database-file $(DB_FILE)
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
