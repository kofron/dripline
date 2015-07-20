/*
 * dripline_test.go
 * 
 * Test program for the golang dripline interface.
 *
 */

package main

import (
	"github.com/project8/dripline/dripline"
)


func main() {
	dripline.InitializeLogging()

	var sender *AmqpSender = StartSender("amqp://nsoblath:wantmoelectrons@higgsino.physics.ucsb.edu")
	if (sender == nil) {
		Log.Critical("Sender did not start")
		return
	}

	senderInfo := dripline.PrepareSenderInfo("dripline", "dripline_test", "0.0", "abcdefg", "localhost", "me")

	replyChan := make(chan message, 1)
	request := (&sender).PrepareRequest("dripline_test", "application/json", MOCommand, senderInfo, replyChan, nil)

	(&sender).SendMessage(request)

}