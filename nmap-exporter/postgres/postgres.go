package postgres

import (
	"time"

	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq" // postgres driver
)

type PostgresStore struct {
	conn *sqlx.DB
}

func NewPostgresStore(dbURL string) (*PostgresStore, error) {
	conn, err := sqlx.Connect("postgres", dbURL)
	if err != nil {
		return nil, err
	}

	if _, err := conn.Exec("SET TIME ZONE 'UTC';"); err != nil {
		return nil, err
	}

	store := &PostgresStore{conn}
	if err := store.init(); err != nil {
		return nil, err
	}
	return store, nil
}

func (s *PostgresStore) CloseConn() error {
	return s.conn.Close()
}

func (s *PostgresStore) init() error {
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

	_, err := s.conn.Exec(sqlStmt)
	return err
}

func (s *PostgresStore) StoreHost(ip string, t time.Time) error {
	_, err := s.conn.Exec(
		`INSERT INTO hosts (ip, time) VALUES($1, $2)`, ip, t)
	return err
}
