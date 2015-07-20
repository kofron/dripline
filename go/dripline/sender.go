/*
* amqp.go
*
* the amqp functions takes care of communication via the AMQP protocol
*
* Two threads can be used for receiving and sending AMQP messages, respectively: AmqpReceiver and AmqpSender
 */

package dripline

import (
	"github.com/streadway/amqp"
	"code.google.com/p/go-uuid/uuid"
)

type AmqpSender struct {
	BrokerAddress string
	DoneSignal chan bool
	sendRequestQueue chan Request
	sendReplyQueue chan Reply
	sendAlertQueue chan Alert
	sendInfoQueue chan Info
	stopQueue chan bool
}

func StartSender(brokerAddress string) (sender *AmqpSender) {
	sender = nil

	var newSender = AmqpSender {
		BrokerAddress: brokerAddress,
		DoneSignal: make(chan bool, 1),
		sendRequestQueue: make(chan Request, 100),
		sendReplyQueue: make(chan Reply, 100),
		sendAlertQueue: make(chan Alert, 100),
		sendInfoQueue: make(chan Info, 100),
		stopQueue: make(chan bool, 5),
	}

	go runAmqpSender(&newSender)

	isDone := <-newSender.DoneSignal
	if isDone {
		Log.Critical("Sender did not start")
		return
	}

	sender = &newSender
	return
}

func (sender *AmqpSender) SendRequest(toSend Request) {
	Log.Debug("Submitting request to send")
	(*sender).sendRequestQueue <- toSend
	return
}

func (sender *AmqpSender) SendReply(toSend Reply) {
	(*sender).sendReplyQueue <- toSend
	return
}

func (sender *AmqpSender) SendAlert(toSend Alert) {
	(*sender).sendAlertQueue <- toSend
	return
}

func (sender *AmqpSender) SendInfo(toSend Info) {
	(*sender).sendInfoQueue <- toSend
	return
}

func (sender *AmqpSender) StopSender() {
	(*sender).stopQueue <- true
	return
}

// runAmqpSender is a goroutine responsible for sending AMQP messages received on a channel
// Broker address format: amqp://[user:password]@(address)[:port]
//    Required: address
//    Optional: user/password, port
func runAmqpSender(sender *AmqpSender) {
	// Connect to the AMQP broker
	// Deferred command: close the connection
	connection, receiveErr := amqp.Dial(sender.BrokerAddress)
	if receiveErr != nil {
		Log.Critical("Unable to connect to the AMQP broker at (%s) for receiving:\n\t%v", sender.BrokerAddress, receiveErr.Error())
		sender.DoneSignal <- true
		return
	}
	defer connection.Close()

	// Create the channel object that represents the connection to the broker
	// Deferred command: close the channel
	channel, chanErr := connection.Channel()
	if chanErr != nil {
		Log.Critical("Unable to get the AMQP channel:\n\t%v", chanErr.Error())
		sender.DoneSignal <- true
		return
	}
	defer channel.Close()

	Log.Info("AMQP sender started successfully")
	sender.DoneSignal <- false

amqpLoop:
	for {
		select {
		// the control messages can stop execution
		case stopSig := <-sender.stopQueue:
			if stopSig == true {
				Log.Info("AMQP sender stopping on interrupt.")
				break amqpLoop
			}
		case request := <-sender.sendRequestQueue:
			Log.Debug("Sending a request")
			// if a reply is requested (as indicated by a non-nil reply channel), add the channel to the reply map
			if request.ReplyChan != nil {
				//replyMap[correlationId] = message.ReplyChan
			}
			// encode the message
			body, encErr := (&request).Encode()
			if encErr != nil {
				Log.Error("An error occurred while encoding a request message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&request).send(channel, body)
		case reply := <-sender.sendReplyQueue:
			Log.Debug("Sending a reply")
			// encode the message
			body, encErr := (&reply).Encode()
			if encErr != nil {
				Log.Error("An error occurred while encoding a reply message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&reply).send(channel, body)
		case alert := <-sender.sendAlertQueue:
			Log.Debug("Sending a alert")
			// encode the message
			body, encErr := (&alert).Encode()
			if encErr != nil {
				Log.Error("An error occurred while encoding an alert message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&alert).send(channel, body)
		case info := <-sender.sendInfoQueue:
			Log.Debug("Sending a info")
			// encode the message
			body, encErr := (&info).Encode()
			if encErr != nil {
				Log.Error("An error occurred while encoding an info message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&info).send(channel, body)
		} // end select block
	} // end for loop

	sender.DoneSignal <- true
	return
}

func (message *Message) send(channel *amqp.Channel, body []byte) {
	// Get the UUID for the correlation ID
	correlationId := (*message).CorrId
	if (*message).CorrId == "" {
		correlationId = uuid.New()
	}

	var amqpMessage = amqp.Publishing {
		ContentEncoding: (*message).Encoding,
		Body: body,
		ReplyTo: (*message).ReplyTo,
		CorrelationId: correlationId,
	}

	//log.Printf("[amqp sender] Encoded message:\n\t%v", amqpMessage)
	Log.Debug("Sending message to routing key <%s>", (*message).Target)

	// Publish!
	pubErr := channel.Publish((*message).exchange, (*message).Target, false, false, amqpMessage)
	if pubErr != nil {
		Log.Error("Error while sending message:\n\t%v", pubErr)
	}
}
