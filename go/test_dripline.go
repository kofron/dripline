/*
 * dripline_test.go
 * 
 * Test program for the golang dripline interface.
 *
 */

package main

import (
 	"flag"
 	"os"
 	"time"

	"github.com/project8/dripline/go/dripline"
	"github.com/project8/swarm/Go/logging"
)


func main() {
	logging.InitializeLogging()
	logging.ConfigureLogging("DEBUG")

	// user needs help
	var needHelp bool

	// RabbitMQ broker user and password
	var user, password string

	// set up flag to point at conf, parse arguments and then verify
	flag.BoolVar(&needHelp, "help", false, "Display this dialog")
	flag.StringVar(&user, "user", "", "RabbitMQ broker user")
	flag.StringVar(&password, "pword", "", "RabbitMQ broker password")
	flag.StringVar(&broker, "broker", "", "RabbitMQ broker")
	flag.Parse()

	if needHelp {
		flag.Usage()
		os.Exit(1)
	}

	url := "amqp://" + user + ":" + password + "@" + broker

	// Bob will be receiving a message from Alice.
	// Start a goroutine to handle and reply to that message
	go func(){
		bob := dripline.StartService(url, "dt_bob")
		if (bob == nil) {
			logging.Log.Critical("Bob did not start")
			return
		}
		logging.Log.Info("Bob has started")

		// Expect to receive a request from Alice
		if subscribeErr := bob.SubscribeToRequests("dt_bob"); subscribeErr != nil {
			logging.Log.Critical("Bob could not subscribe to requests: %v", subscribeErr)
			return
		}

		request := <- bob.Receiver.RequestChan
		logging.Log.Info("Bob has received a request: %v", request)

		senderInfo := dripline.PrepareSenderInfo("dripline", "dripline_test", "0.0", "abcdefg", "localhost", "Bob")
		reply := dripline.PrepareReplyToRequest(request, dripline.RCSuccess, "Received message!", senderInfo)
		if sendErr := bob.SendReply(reply); sendErr != nil {
			logging.Log.Critical("Bob could not send the reply: %v", sendErr)
			return
		}
		logging.Log.Info("Bob has sent the reply")
		time.Sleep(5 * time.Second) // pause to make sure the reply gets sent

		bob.Stop()
		logging.Log.Info("Bob has stopped")
	}()

	// pause to make sure Bob is ready
	time.Sleep(5 * time.Second)

	alice := dripline.StartService(url, "")
	if (alice == nil) {
		logging.Log.Critical("Alice did not start")
		return
	}
	logging.Log.Info("Alice has started")

	// Alice sends a request to Bob
	senderInfo := dripline.PrepareSenderInfo("dripline", "dripline_test", "0.0", "abcdefg", "localhost", "Alice")
	request := dripline.PrepareRequest("dt_bob", "application/json", dripline.MOCommand, senderInfo)
	replyChan, sendErr := alice.SendRequest(request)
	if sendErr != nil {
		logging.Log.Critical("Alice could not send the request: %v", sendErr)
		return
	}
	logging.Log.Info("Alice has sent the request")

	logging.Log.Info("Alice is awaiting the reply from Bob")
	reply := <- replyChan
	logging.Log.Info("Alice has received a reply: %v", reply)

	alice.Stop()
	logging.Log.Info("Alice has stopped")
}