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

type AmqpReceiver struct {
	BrokerAddress     string
	Connected         bool
	channel           amqp.Channel
	exchangeName      string
	queueName         string
	stopQueue         chan bool
	DoneSignal        chan bool
	requestMap        map[string]chan Request
	replyMap          map[string]chan Reply
	alertMap          map[string]chan Alert
	infoMap           map[string]chan Info
}

// StartReceiver creates a receiver object and starts the receiver routine running.
// To use the default exchange, leave an empty string for that parameter.
func StartReceiver(brokerAddress, exchange, queue string) (receiver *AmqpReceiver) {
	receiver = nil

	var newReceiver = AmqpReceiver {
		BrokerAddress:    brokerAddress,
		Connected:        false,
		exchangeName:     exchange,
		queueName:        queue,
		stopQueue:        make(chan bool, 5),
		DoneSignal:       make(chan bool, 1),
		requestMap:       make(map[string]chan Request),
		replyMap:         make(map[string]chan Reply),
		alertMap:         make(map[string]chan Alert),
		infoMap:          make(map[string]chan Info),
	}

	go runAmqpReceiver(&newReceiver)

	isDone := <-newReceiver.DoneSignal
	if isDone {
		Log.Critical("Receiver did not start")
		return
	}

	receiver = &newReceiver
	return
}

// DeclareExchange creates an exchange if it doesn't already exist
func (receiver *AmqpReceiver) DeclareExchange(name string, exchangeType string) (e error) {
	if receiver.Connected == false {
		e = fmt.Error("Receiver is not connected to a broker")
		return
	}
	e = receiver.channel.ExchangeDeclare(name, exchangeType, false, false, false, false, nil)
	return
}

func (receiver *AmqpReceiver) ReceiveRequests(routingKey, reqChan chan Request) (e error) {
	if receiver.Connected == false {
		e = fmt.Error("Receiver is not connected to a broker")
		return
	}

	if _, hasKey := receiver.requestMap[routingKey]; hasKey == true {
		e = fmt.Error("Receiver already receiving requests with routing key <%v>", routingKey)
		return
	}

	if e = receiver.channel.QueueBind(receiver.queueName, routingKey, receiver.exchangeName, false, nil); e != nil {
		return
	}

	receiver.requestMap[routingKey] := reqChan
	return
}

func (receiver *AmqpReceiver) ReceiveReplies(routingKey, replyChan chan Reply) (e error) {
	if receiver.Connected == false {
		e = fmt.Error("Receiver is not connected to a broker")
		return
	}

	if _, hasKey := receiver.replyMap[routingKey]; hasKey == true {
		e = fmt.Error("Receiver already receiving replies with routing key <%v>", routingKey)
		return
	}

	if e = receiver.channel.QueueBind(receiver.queueName, routingKey, receiver.exchangeName, false, nil); e != nil {
		return
	}

	receiver.replyMap[routingKey] := replyChan
	return
}

func (receiver *AmqpReceiver) ReceiveAlerts(routingKey, alertChan chan Alert) (e error) {
	if receiver.Connected == false {
		e = fmt.Error("Receiver is not connected to a broker")
		return
	}

	if _, hasKey := receiver.alertMap[routingKey]; hasKey == true {
		e = fmt.Error("Receiver already receiving alerts with routing key <%v>", routingKey)
		return
	}

	if e = receiver.channel.QueueBind(receiver.queueName, routingKey, receiver.exchangeName, false, nil); e != nil {
		return
	}

	receiver.alertMap[routingKey] := alertChan
	return
}

func (receiver *AmqpReceiver) ReceiveInfos(routingKey, infoChan chan Info) (e error) {
	if receiver.Connected == false {
		e = fmt.Error("Receiver is not connected to a broker")
		return
	}

	if _, hasKey := receiver.infoMap[routingKey]; hasKey == true {
		e = fmt.Error("Receiver already receiving infos with routing key <%v>", routingKey)
		return
	}

	if e = receiver.channel.QueueBind(receiver.queueName, routingKey, receiver.exchangeName, false, nil); e != nil {
		return
	}

	receiver.infoMap[routingKey] := infoChan
	return
}

func (receiver *AmqpReceiver) GetRequestQueue(name string) (queue chan Request) {
	queue = (*receiver).requestMap[name]
	return
}

func (receiver *AmqpReceiver) GetReplyQueue(name string) (queue chan Reply) {
	queue = r(*receiver).eplyMap[name]
	return
}

func (receiver *AmqpReceiver) GetAlertQueue(name string) (queue chan Alert) {
	queue = (*receiver).alertMap[name]
	return
}

func (receiver *AmqpReceiver) GetInfoQueue(name string) (queue chan Info) {
	queue = (*receiver).infoMap[name]
	return
}

func (receiver *AmqpReceiver) Stop() {
	if receiver.Connected == false {
		Log.Info("Receiver is not connected to a broker")
		return
	}
	Log.Debug("Submitting stop request")
	(*sender).stopQueue <- true
	return
}


// runAmqpReceiver is a goroutine for receiving and handling AMQP messages
// Broker address format: amqp://[user:password]@(address)[:port]
//    Required: address
//    Optional: user/password, port
func runAmqpReceiver(receiver *AmqpReceiver) {
	connection, receiveErr := amqp.Dial(receiver.BrokerAddress)
	if receiveErr != nil {
		Log.Critical("Unable to connect to the AMQP broker at (%s) for receiving:\n\t%v", receiver.BrokerAddress, receiveErr.Error())
		receiver.DoneSignal <- true
		return
	}
	defer connection.Close()

	// Create the channel object that represents the connection to the broker
	// Deferred command: close the channel
	receiver.channel, chanErr := connection.Channel()
	if chanErr != nil {
		Log.Critical("Unable to get the AMQP channel:\n\t%v", chanErr.Error())
		receiver.DoneSignal <- true
		return
	}
	defer receiver.channel.Close()

	// Declare the queue
	// Deferred command: delete the queue
	_, queueDeclErr := channel.QueueDeclare(receiver.queueName, false, true, true, false, nil)
	if queueDeclErr != nil {
		Log.Critical("Unable to declare queue <%s>:\n\t%v", queueName, queueDeclErr.Error())
		receiver.DoneSignal <- true
		return
	}
	defer func() {
		if _, err := channel.QueueDelete(queueName, false, false, false); err != nil {
			Log.Error("Error while deleting queue:\n\t%v", err)
		}
	}()

	// Start consuming messages on the queue
	// Channel::Cancel is not executed as a deferred command, because consuming will be stopped by Channel.Close
	messageQueue, consumeErr := channel.Consume(receiver.queueName, "", false, true, true, false, nil)
	if consumeErr != nil {
		Log.Critical("Unable start consuming from queue <%s>:\n\t%v", receiver.queueName, consumeErr.Error())
		receiver.DoneSignal <- true
		return
	}

	Log.Info("AMQP Receiver started successfully")
	receiver.Connected = true
	defer func() {receiver.Connected = false}()

amqpLoop:
	for {
		select {
		// the control messages can stop execution
		case stopSig := <-sender.stopQueue:
			if stopSig == true {
				Log.Info("AMQP sender stopping on interrupt.")
				break amqpLoop
			} else {
				Log.Warn("Received on the stop queue, but it wasn't \"true\"")
			}
		// process any AMQP messages that are received
		case amqpMessage := <-messageQueue:
			// Send an acknowledgement to the broker
			amqpMessage.Ack(false)

			DecodeAndHandle(amqpMessage,
				func(request Request){
					if reqChan, hasChan := receiver.requestMap[request.RoutingKey]; hasChan == true {
						reqChan <- request
					}
				},
				func(Reply){},
				func(Alert){},
				func(Info){},
				)

			//log.Printf("[amqp receiver] Message:\n\t%v", p8Message)
		} // end select block
	} // end for loop
}
