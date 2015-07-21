// Utility functions for the dripline package go here.
package dripline

import (
	//"errors"
	//"fmt"
	"os"
	"path/filepath"
	"strings"
)

// ConvertToMsgCode converts interface{} values with the types that typically underly JSON-encoded integers
func ConvertToMsgCode(ifcVal interface{}) (MsgCodeT) {
	switch val := ifcVal.(type) {
		case int64:
			return MsgCodeT(val)
		case uint64:
			return MsgCodeT(val)
		default:
			return 999
	}
}

// ConvertToMsgCode converts interface{} values with the types that typically underly JSON-encoded integers
func ConvertToString(ifcVal interface{}) (string) {
	switch val := ifcVal.(type) {
		case string:
			return val
		case []uint8:
			return string(val)
		default:
			return "UNKNOWN MESSAGE TYPE"
	}
}



