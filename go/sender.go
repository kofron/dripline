/*
* amqp.go
*
* the amqp functions takes care of communication via the AMQP protocol
*
* Two threads can be used for receiving and sending AMQP messages, respectively: AmqpReceiver and AmqpSender
 */

package dripline

import (
	"errors"
	"fmt"
	"os"
	"os/user"
	"strings"
	"sync"
	"time"
	"unsafe"

	"github.com/streadway/amqp"
	"github.com/ugorji/go/codec"
	"github.com/kardianos/osext"
	"code.google.com/p/go-uuid/uuid"
)

// AmqpSender is a goroutine responsible for sending AMQP messages received on a channel
// Broker address format: amqp://[user:password]@(address)[:port]
//    Required: address
//    Optional: user/password, port
func AmqpSender(brokerAddress string, exchange string, sendQueue <-chan Message, stopQueue <-chan bool, doneSignal chan<- bool) {
	// Connect to the AMQP broker
	// Deferred command: close the connection
	connection, receiveErr := amqp.Dial(brokerAddress)
	if receiveErr != nil {
		Log.Critical("Unable to connect to the AMQP broker at (%s) for receiving:\n\t%v", brokerAddress, receiveErr.Error())
		doneSignal <- true
		return
	}
	defer connection.Close()

	// Create the channel object that represents the connection to the broker
	// Deferred command: close the channel
	channel, chanErr := connection.Channel()
	if chanErr != nil {
		Log.Critical("Unable to get the AMQP channel:\n\t%v", chanErr.Error())
		doneSignal <- true
		return
	}
	defer channel.Close()

	Log.Info("AMQP sender started successfully")

amqpLoop:
	for {
		select {
		// the control messages can stop execution
		case stopSig := <-stopQueue:
			if stopSig == true {
				Log.Info("AMQP sender stopping on interrupt.")
				break amqpLoop
			}
		// process any message reuqests receivec on the send-messsage queue
		case message := <-sendQueue:

			// Get the UUID for the correlation ID
			correlationId := message.CorrId
			if message.CorrId == "" {
				correlationId = uuid.New()
			}

			// if a reply is requested (as indicated by a non-nil reply channel), add the channel to the reply map
			if message.ReplyChan != nil {
				replyMap[correlationId] = message.ReplyChan
			}

			// encode the message
			body, encErr := message.Encode()
			if encErr != nil {
				Log.Error("An error occurred while encoding a message: \n\t%v", encErr)
			}

			var amqpMessage = amqp.Publishing {
				ContentEncoding: p8Message.Encoding,
				Body: body,
				ReplyTo: replyTo,
				CorrelationId: correlationId,
			}

			routingKey := strings.Join(message.Target, TargetSeparator)

			//log.Printf("[amqp sender] Encoded message:\n\t%v", amqpMessage)
			Log.Debug("Sending message to routing key <%s>", routingKey)

			// Publish!
			pubErr := channel.Publish(exchange, routingKey, false, false, amqpMessage)
			if pubErr != nil {
				Log.Error("Error while sending message:\n\t%v", pubErr)
			}

		} // end select block
	} // end for loop

	doneSignal <- true
}


