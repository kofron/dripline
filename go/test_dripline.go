/*
 * dripline_test.go
 * 
 * Test program for the golang dripline interface.
 *
 */

package main

import (
 	"time"
	"github.com/project8/dripline/go/dripline"
)


func main() {
	dripline.InitializeLogging()

	// Create the sender object, and connect to the broker
	var sender *(dripline.AmqpSender) = dripline.StartSender("amqp://nsoblath:wantmoelectrons@higgsino.physics.ucsb.edu")
	if (sender == nil) {
		dripline.Log.Critical("Sender did not start")
		return
	}

	dripline.Log.Info("Sender started")

	// Create a SenderInfo struct; in real applications it's useful to make a "master" copy of this.
	senderInfo := dripline.PrepareSenderInfo("dripline", "dripline_test", "0.0", "abcdefg", "localhost", "me")

	// Create a channel on which we'll expect to receive a reply
	replyChan := make(chan dripline.Reply, 1)

	// Create the request object
	request := dripline.PrepareRequest("dripline_test", "application/json", dripline.MOCommand, senderInfo, replyChan, nil)

	dripline.Log.Info("Request ready")

	time.Sleep(1*time.Second)

	// Send the request
	(*sender).SendRequest(request)

	dripline.Log.Info("Request sent")

	dripline.Log.Info("Waiting for reply; use ctrl-c to cancel")
	reply := <-replyChan

	dripline.Log.Info("Reply received")

	dripline.Log.Info("Test complete")
}