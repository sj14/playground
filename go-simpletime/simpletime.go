package simpletime

import (
	"strings"
)

// ParseFormat converts a simple format to Go's native formatting.
func ParseFormat(format string) string {
	format = strings.ReplaceAll(format, "YYYY", "2006") // Long year
	format = strings.ReplaceAll(format, "YY", "06")     // Short year
	format = strings.ReplaceAll(format, "MM", "01")     // Month (2-digit)
	format = strings.ReplaceAll(format, "M", "1")       // Month (1-digit)
	format = strings.ReplaceAll(format, "DD", "02")     // Day (2-digit)
	format = strings.ReplaceAll(format, "D", "2")       // Day (1-digit)

	format = strings.ReplaceAll(format, "hh", "15") // Hour (2-digit)
	format = strings.ReplaceAll(format, "mm", "04") // Minute (2-digit)
	format = strings.ReplaceAll(format, "m", "4")   // Minute (1-digit)
	format = strings.ReplaceAll(format, "ss", "05") // Second (2-digit)
	format = strings.ReplaceAll(format, "s", "5")   // Second (1-digit)

	return format
}
