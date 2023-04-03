package main

import (
	"database/sql"
	"fmt"
	"log"
	"os"

	"github.com/charmbracelet/bubbles/table"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	_ "github.com/mattn/go-sqlite3"
)

func getValues() map[string][]interface{} {

	sqliteDB := NewSQLite("test.db")

	rows, err := sqliteDB.db.Query("select * from products")
	if err != nil {
		panic(err)
	}

	columnNames, err := rows.Columns()
	if err != nil {
		panic(err)
	}
	// key -> column name
	columnValues := make(map[string][]interface{})

	// See https://github.com/mathaou/termdbms/blob/be6f397196077cc7c9ced86e6460470e3b223f3e/viewer/modelutil.go#L170
	for rows.Next() { // each row of the table
		// golang wizardry
		columns := make([]interface{}, len(columnNames))
		columnPointers := make([]interface{}, len(columnNames))
		// init interface array
		for i := range columns {
			columnPointers[i] = &columns[i]
		}

		rows.Scan(columnPointers...)

		for i, colName := range columnNames {
			val := columnPointers[i].(*interface{})
			columnValues[colName] = append(columnValues[colName], *val)
		}
	}
	for key, values := range columnValues {
		log.Printf("key: %v\n", key)

		for _, val := range values {
			log.Printf("val: %v\n", val)
		}
	}

	return columnValues
}

// SQLite implements the bencher interface.
type SQLite struct {
	db *sql.DB
}

// NewSQLite retruns a new SQLite bencher.
func NewSQLite(path string) *SQLite {
	// Automatically creates the DB file if it doesn't exist yet.
	db, err := sql.Open("sqlite3", fmt.Sprintf("%s?cache=shared", path))
	if err != nil {
		log.Fatalf("failed to open connection: %v\n", err)
	}

	db.SetMaxOpenConns(1)
	p := &SQLite{db: db}
	return p
}

var baseStyle = lipgloss.NewStyle().
	BorderStyle(lipgloss.NormalBorder()).
	BorderForeground(lipgloss.Color("240"))

type model struct {
	table table.Model
}

func (m model) Init() tea.Cmd { return nil }

func (m model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "esc":
			if m.table.Focused() {
				m.table.Blur()
			} else {
				m.table.Focus()
			}
		case "q", "ctrl+c":
			return m, tea.Quit
		case "enter":
			return m, tea.Batch(
				tea.Printf("Let's go to %s!", m.table.SelectedRow()[1]),
			)
		}
	}
	m.table, cmd = m.table.Update(msg)
	return m, cmd
}

func (m model) View() string {
	return baseStyle.Render(m.table.View()) + "\n"
}

func toRows(tableValues map[string][]interface{}) ([]table.Column, []table.Row) {
	var columns []table.Column
	var rows []table.Row

	for colName, colValues := range tableValues {
		columns = append(columns, table.Column{
			Title: colName,
			Width: 10,
		})

		log.Printf("COLUMN: %v\n", colName)

		for rowIdx, colVal := range colValues {
			valStr := fmt.Sprintf("%v", colVal)
			if len(rows) <= rowIdx {
				rows = append(rows, []string{})
			}
			rows[rowIdx] = append(rows[rowIdx], valStr)

			log.Println(rows[rowIdx])
		}
	}

	return columns, rows
}

func main() {

	tableValues := getValues()

	columns, rows := toRows(tableValues)
	// log.Println()
	// log.Println()
	// log.Println()
	// log.Println("Columns:")

	// log.Println(columns)

	// log.Println("Values:")
	// for i, vals := range rows {
	// 	log.Printf("ID %v:", i)

	// 	for val := range vals {
	// 		log.Printf("%v", val)
	// 	}
	// 	log.Println()
	// }

	// columns := []table.Column{
	// 	{Title: "Rank", Width: 4},
	// 	{Title: "City", Width: 10},
	// 	{Title: "Country", Width: 10},
	// 	{Title: "Population", Width: 10},
	// }

	// rows := []table.Row{
	// 	{"1", "Tokyo", "Japan", "37,274,000"},
	// 	{"2", "Delhi", "India", "32,065,760"},
	// 	{"3", "Shanghai", "China", "28,516,904"},
	// 	{"4", "Dhaka", "Bangladesh", "22,478,116"},
	// }

	t := table.New(
		table.WithColumns(columns),
		table.WithRows(rows),
		table.WithFocused(true),
		table.WithHeight(7),
	)

	s := table.DefaultStyles()
	s.Header = s.Header.
		BorderStyle(lipgloss.NormalBorder()).
		BorderForeground(lipgloss.Color("240")).
		BorderBottom(true).
		Bold(false)
	s.Selected = s.Selected.
		Foreground(lipgloss.Color("229")).
		Background(lipgloss.Color("57")).
		Bold(false)
	t.SetStyles(s)

	m := model{t}
	if _, err := tea.NewProgram(m).Run(); err != nil {
		fmt.Println("Error running program:", err)
		os.Exit(1)
	}
}
