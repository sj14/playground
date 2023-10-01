package postgres

import (
	"log"
	"time"

	"github.com/jmoiron/sqlx"
	"github.com/lib/pq"
	_ "github.com/lib/pq" // postgres driver
)

type PostgresStore struct {
	conn *sqlx.DB
}

func NewPostgresStore(dbURL string) *PostgresStore {
	conn, err := sqlx.Connect("postgres", dbURL)
	if err != nil {
		log.Fatal(err)
	}

	postgresHandler := &PostgresStore{conn}
	if _, err := conn.Exec("SET TIME ZONE 'UTC';"); err != nil {
		log.Println(err)
	}
	postgresHandler.init()
	return postgresHandler
}

func (s *PostgresStore) CloseConn() error {
	return s.conn.Close()
}

func (p *PostgresStore) init() {
	// CREATE TABLE public.names (
	// 	date TIMESTAMP(6) NOT NULL DEFAULT now():::TIMESTAMP,
	// 	name VARCHAR(255) NOT NULL,
	// 	ip STRING NOT NULL,
	// 	vip BOOL NOT NULL DEFAULT false,
	// 	ignore BOOL NOT NULL DEFAULT false,
	// 	CONSTRAINT names_pkey PRIMARY KEY (rowid ASC)
	//   )

	sqlStmt := `
	CREATE TABLE IF NOT EXISTS hosts (ip TEXT NOT NULL, time TIMESTAMP NOT NULL );
	CREATE INDEX IF NOT EXISTS idx_hosts_time ON hosts ("time") STORING (ip);
	CREATE INDEX IF NOT EXISTS idx_names_ip ON network.public.names (ip) STORING (name, vip);
	`

	if _, err := p.conn.Exec(sqlStmt); err != nil {
		log.Fatalln(err)
	}
}

func IsNotFound(err error) bool {
	if err, ok := err.(*pq.Error); ok {
		// Here err is of type *pq.Error, you may inspect all its fields, e.g.:
		if err.Code == "02000" {
			return true
		}
	}
	return false
}

func (s *PostgresStore) StoreHost(ip string, t time.Time) error {
	_, err := s.conn.Exec(
		`INSERT INTO hosts (ip, time) VALUES($1, $2)`, ip, t)
	return err
}
