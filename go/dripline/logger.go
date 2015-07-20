/*
* logger.go
*
* Logging functions with severity
*
*/

package dripline

import (
	"os"

	"github.com/op/go-logging"
)

// global logger
var Log = logging.MustGetLogger("dripline")
var format = logging.MustStringFormatter(
    "%{color}%{id:03x} %{time:15:04:05.000} %{level:.4s} [%{shortfunc}] ▶ %{message}%{color:reset}",
)

var currentBackends []logging.Backend
func AddBackend(backend logging.Backend) {
	currentBackends = append(currentBackends, backend)
	logging.SetBackend(currentBackends...)
}

func InitializeLogging() {
	backend := logging.NewLogBackend(os.Stdout, "", 0)
	backendFormatter := logging.NewBackendFormatter(backend, format)
	AddBackend(backendFormatter)
}

	
