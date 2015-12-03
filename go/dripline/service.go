/*
* service.go
*
* An AMQP Service can be used by a client for both sending and receiving dripline wire protocol messages.
 */

package dripline

import (
	"fmt"

	"github.com/streadway/amqp"
	"code.google.com/p/go-uuid/uuid"

	"github.com/project8/swarm/Go/logging"
)



type AmqpReceiver struct {
	QueueName         string
	RequestChan      chan Request
	//ReplyChan        chan Reply
	AlertChan        chan Alert
	InfoChan         chan Info
	subscriptionCount int
	messageQueue      <-chan amqp.Delivery
}

type AmqpSender struct {
	RequestExchangeName   string
	AlertExchangeName     string
	InfoExchangeName      string
	requestChan      chan Request
	replyChan        chan Reply
	alertChan        chan Alert
	infoChan         chan Info
}


type AmqpService struct {
	BrokerAddress     string
	Connected         bool
	DoneSignal        chan bool
	Receiver          AmqpReceiver
	Sender            AmqpSender
	channel           *amqp.Channel
	stopQueue         chan bool
}

//*******************************
//*** Start-Service Functions ***
//*******************************

// ServiceDefaults sets up a Service struct with the default values
func ServiceDefaults() (service *AmqpService) {
	var newService = AmqpService {
		BrokerAddress: "localhost",
		Connected: false,
		DoneSignal:    make(chan bool, 1),
		Receiver:      AmqpReceiver {
			QueueName: "my_queue",
			RequestChan:    make(chan Request, 100),
			//ReplyChan:      make(chan Reply, 100),
			AlertChan:      make(chan Alert, 100),
			InfoChan:       make(chan Info, 100),
		},
		Sender:        AmqpSender {
			RequestExchangeName: "requests",
			AlertExchangeName:   "alerts",
			InfoExchangeName:    "requests",
			requestChan:    make(chan Request, 100),
			replyChan:      make(chan Reply, 100),
			alertChan:      make(chan Alert, 100),
			infoChan:       make(chan Info, 100),
		},
		stopQueue:     make(chan bool, 5),
	}

	service = &newService
	return
}

// StartService runs an AMQP service; this function should be used if the service object has already been setup as desired.
func (service *AmqpService) StartService() {
	go runAmqpService(service)

	isDone := <-service.DoneSignal
	if isDone {
		logging.Log.Critical("Service did not start")
		return
	}

	return
}

// StartService runs an AMQP service with the given broker address and queue name.
// All other parameters are set to the default values.
func StartService(brokerAddress, queueName string) (service *AmqpService) {
	service = ServiceDefaults()
	service.BrokerAddress = brokerAddress
	service.Receiver.QueueName = queueName

	service.StartService()
	return
}


//******************************
//*** Send-Message Functions ***
//******************************

// SendRequest sends a Request message.  It creates a reply queue, begins consuming on it, and returns the channel on which the client can wait for the Reply message.
func (service *AmqpService) SendRequest(toSend Request) (replyChan <-chan Reply, queueName string, e error) {
	logging.Log.Debug("Submitting request to send")
	queue, declareErr := service.channel.QueueDeclare("", false, true, true, false, nil)
	if declareErr != nil {
		logging.Log.Error("Unable to declare the reply queue: %v", declareErr)
		e = declareErr
		return
	}
	queueName = queue.Name

	toSend.ReplyTo = queueName
	bindErr := service.channel.QueueBind(queue.Name, queue.Name, service.Sender.RequestExchangeName, false, nil)
	if bindErr != nil {
		logging.Log.Error("Unable to bind the reply queue <%s>:\n\t%v", queue.Name, bindErr)
		e = bindErr
		return
	}

	messageQueue, consumeErr := service.channel.Consume(queueName, "", false, true, true, false, nil)
	if consumeErr != nil {
		logging.Log.Error("Unable start consuming from reply queue <%s>:\n\t%v", queue.Name, consumeErr.Error())
		e = consumeErr
		return
	}

	service.Sender.requestChan <- toSend
	logging.Log.Debug("Request sent")

	replyChanFull := make(chan Reply, 1)

	go func() {
		amqpMessage := <-messageQueue
		// Send an acknowledgement to the broker
		amqpMessage.Ack(false)

		decodeErr := DecodeAndHandle(&amqpMessage,
			func(request Request){
				logging.Log.Error("Unexpected request received while waiting for a reply")
			},
			func(reply Reply){
				replyChanFull <- reply
			},
			func(alert Alert){
				logging.Log.Error("Unexpected alert received while waiting for a reply")
			},
			func(info Info){
				logging.Log.Error("Unexpected info received while waiting for a reply")
			},
		)
		if decodeErr != nil {
			logging.Log.Error("An error occurred while decoding a message: \n\t%v", decodeErr)
			return
		}

		if err := service.DeleteQueue(queueName); err != nil {
			logging.Log.Error("Error while deleting reply queue:\n\t%v", err)
			return
		}
		return
	}()

	replyChan = replyChanFull

	return
}

// SendReply sends a Reply message.
func (service *AmqpService) SendReply(toSend Reply) (error) {
	service.Sender.replyChan <- toSend
	return nil
}

// SendAlert sends an Alert message.
func (service *AmqpService) SendAlert(toSend Alert) (error) {
	service.Sender.alertChan <- toSend
	return nil
}

// SendInfo sends an Info message.
func (service *AmqpService) SendInfo(toSend Info) (error) {
	service.Sender.infoChan <- toSend
	return nil
}

// Stop interrupts and halts the AMQP service.
func (service *AmqpService) Stop() {
	logging.Log.Debug("Submitting stop request")
	service.stopQueue <- true
	return
}

//***************************
//*** Subscribe Functions ***
//***************************

// SubscribeToRequests binds the Requests exchange to the service's queue with the given routing key.
func (service *AmqpService) SubscribeToRequests(routingKey string) (e error) {
	if service.Connected == false {
		e = fmt.Errorf("Service is not connected to a broker")
		return
	}
	if e = service.channel.QueueBind(service.Receiver.QueueName, routingKey, service.Sender.RequestExchangeName, false, nil); e != nil {
		return
	}
	service.Receiver.subscriptionCount++
	service.beginConsuming()
	logging.Log.Debug("Subscription established: ex(%s) @ rk(%s) --> q(%s)", service.Sender.RequestExchangeName, routingKey, service.Receiver.QueueName)
	return
}
/*
func (service *AmqpService) SubscribeToReplies(routingKey, exchange string) (e error) {
	if service.Receiver.Connected == false {
		e = fmt.Errorf("Service is not connected to a broker")
		return
	}
	if e = service.channel.QueueBind(service.Receiver.QueueName, routingKey, exchange, false, nil); e != nil {
		return
	}
	service.Receiver.subscriptionCount++
	service.beginConsuming()
	return
}
*/
// SubscribeToRequests binds the Alerts exchange to the service's queue with the given routing key.
func (service *AmqpService) SubscribeToAlerts(routingKey string) (e error) {
	if service.Connected == false {
		e = fmt.Errorf("Service is not connected to a broker")
		return
	}
	if e = service.channel.QueueBind(service.Receiver.QueueName, routingKey, service.Sender.AlertExchangeName, false, nil); e != nil {
		return
	}
	service.Receiver.subscriptionCount++
	service.beginConsuming()
	logging.Log.Debug("Subscription established: ex(%s) @ rk(%s) --> q(%s)", service.Sender.AlertExchangeName, routingKey, service.Receiver.QueueName)
	return
}

// SubscribeToRequests binds the Infos exchange to the service's queue with the given routing key.
func (service *AmqpService) SubscribeToInfos(routingKey string) (e error) {
	if service.Connected == false {
		e = fmt.Errorf("Service is not connected to a broker")
		return
	}
	if e = service.channel.QueueBind(service.Receiver.QueueName, routingKey, service.Sender.InfoExchangeName, false, nil); e != nil {
		return
	}
	service.Receiver.subscriptionCount++
	service.beginConsuming()
	logging.Log.Debug("Subscription established: ex(%s) @ rk(%s) --> q(%s)", service.Sender.InfoExchangeName, routingKey, service.Receiver.QueueName)
	return
}


//***********************
//*** Other Functions ***
//***********************

// DeleteQueue allows the user to delete a particular queue
func (service* AmqpService) DeleteQueue(queueName string) (error) {
	_, err := service.channel.QueueDelete(queueName, false, false, false)
	return err
}


//*************************
//*** Private Functions ***
//*************************

func (service *AmqpService) beginConsuming() {
	// Start consuming messages on the queue if there are subscriptions
	// Channel::Cancel is not executed as a deferred command, because consuming will be stopped by Channel.Close
	if service.Receiver.subscriptionCount == 0 {
		return
	}
	messageQueue, consumeErr := service.channel.Consume(service.Receiver.QueueName, "", false, true, true, false, nil)
	if consumeErr != nil {
		logging.Log.Critical("Unable start consuming from queue <%s>:\n\t%v", service.Receiver.QueueName, consumeErr.Error())
		service.DoneSignal <- true
		return
	}
	service.Receiver.messageQueue = messageQueue
	logging.Log.Debug("Started consuming on queue %s", service.Receiver.QueueName)
	// reset the amqpLoop, because the message queue has been updated
	service.stopQueue <- false
	return
}

// runAmqpSender is a goroutine responsible for sending AMQP messages received on a channel
// Broker address format: amqp://[user:password]@(address)[:port]
//    Required: address
//    Optional: user/password, port
func runAmqpService(service *AmqpService) {
	// Connect to the AMQP broker
	// Deferred command: close the connection
	connection, receiveErr := amqp.Dial(service.BrokerAddress)
	if receiveErr != nil {
		logging.Log.Critical("Unable to connect to the AMQP broker at (%s) for receiving:\n\t%v", service.BrokerAddress, receiveErr.Error())
		service.DoneSignal <- true
		return
	}
	defer connection.Close()
	service.Connected = true
	logging.Log.Debug("Connected to AMQP broker (%s)", service.BrokerAddress)

	// Create the channel object that represents the connection to the broker
	// Deferred command: close the channel
	channel, chanErr := connection.Channel()
	if chanErr != nil {
		logging.Log.Critical("Unable to get the AMQP channel:\n\t%v", chanErr.Error())
		service.DoneSignal <- true
		return
	}
	defer channel.Close()
	logging.Log.Debug("Channel with AMQP broker established")
	service.channel = channel


	// Setup to Receive

	if service.Receiver.QueueName != "" {
		if _, queueDeclErr := service.channel.QueueDeclare(service.Receiver.QueueName, false, true, true, false, nil); queueDeclErr != nil {
			logging.Log.Critical(queueDeclErr.Error())
			service.DoneSignal <- true
			return
		}
		defer func() {
			if _, err := service.channel.QueueDelete(service.Receiver.QueueName, false, false, false); err != nil {
				logging.Log.Error("Error while deleting queue:\n\t%v", err)
			}
		}()
		logging.Log.Debug("Queue declared: %s", service.Receiver.QueueName)

		// Try to begin consuming, which will only actually happen if there are already subscriptions
		service.beginConsuming()

		logging.Log.Info("AMQP service ready to receive messages")
	}

	// Setup to send messages

	if service.Sender.RequestExchangeName != "" {
		exchangeErr := service.channel.ExchangeDeclare(service.Sender.RequestExchangeName, "topic", false, false, false, false, nil)
		if exchangeErr != nil {
			logging.Log.Critical("Unable to declare the requests exchange (%s)", service.Sender.RequestExchangeName)
			service.DoneSignal <- true
			return
		}
		logging.Log.Debug("Requests exchange is ready")
	}

	if service.Sender.AlertExchangeName != "" {
		exchangeErr := service.channel.ExchangeDeclare(service.Sender.AlertExchangeName, "topic", false, false, false, false, nil)
		if exchangeErr != nil {
			logging.Log.Critical("Unable to declare the alerts exchange (%s)", service.Sender.AlertExchangeName)
			service.DoneSignal <- true
			return
		}
		logging.Log.Debug("Alerts exchange is ready")
	}

	if service.Sender.InfoExchangeName != "" {
		exchangeErr := service.channel.ExchangeDeclare(service.Sender.InfoExchangeName, "topic", false, false, false, false, nil)
		if exchangeErr != nil {
			logging.Log.Critical("Unable to declare the infos exchange (%s)", service.Sender.InfoExchangeName)
			service.DoneSignal <- true
			return
		}
		logging.Log.Debug("Infos exchange is ready")
	}

	logging.Log.Info("AMQP service ready to send messages")

	logging.Log.Notice("AMQP service started successfully")
	service.DoneSignal <- false

amqpLoop:
	for {
		select {
		// the control messages can stop execution
		case stopSig, chanOpen := <-service.stopQueue:
			if ! chanOpen {
				logging.Log.Error("Control queue is closed")
				break amqpLoop
			}

			if stopSig == true {
				logging.Log.Info("AMQP service stopping on interrupt.")
				break amqpLoop
			} else {
				logging.Log.Debug("Received on the stop queue, but it wasn't \"true\"")
				continue amqpLoop
			}
		case request, chanOpen := <-service.Sender.requestChan:
			if ! chanOpen {
				logging.Log.Error("Outgoing request channel is closed")
				break amqpLoop
			}

			logging.Log.Debug("Sending a request")
			// encode the message
			body, encErr := (&request).Encode()
			if encErr != nil {
				logging.Log.Error("An error occurred while encoding a request message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&request).send(channel, body)
		case reply, chanOpen := <-service.Sender.replyChan:
			if ! chanOpen {
				logging.Log.Error("Outgoing reply channel is closed")
				break amqpLoop
			}

			logging.Log.Debug("Sending a reply")
			// encode the message
			body, encErr := (&reply).Encode()
			if encErr != nil {
				logging.Log.Error("An error occurred while encoding a reply message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&reply).send(channel, body)
		case alert, chanOpen := <-service.Sender.alertChan:
			if ! chanOpen {
				logging.Log.Error("Outgoing alert channel is closed")
				break amqpLoop
			}

			logging.Log.Debug("Sending a alert")
			// encode the message
			body, encErr := (&alert).Encode()
			if encErr != nil {
				logging.Log.Error("An error occurred while encoding an alert message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&alert).send(channel, body)
		case info, chanOpen := <-service.Sender.infoChan:
			if ! chanOpen {
				logging.Log.Error("Outgoing info channel is closed")
				break amqpLoop
			}

			logging.Log.Debug("Sending a info")
			// encode the message
			body, encErr := (&info).Encode()
			if encErr != nil {
				logging.Log.Error("An error occurred while encoding an info message: \n\t%v", encErr)
				continue amqpLoop
			}
			(&info).send(channel, body)
		// process any AMQP messages that are received
		case amqpMessage, chanOpen := <-service.Receiver.messageQueue:
			if ! chanOpen {
				logging.Log.Error("Incoming message channel is closed")
				break amqpLoop
			}

			// Send an acknowledgement to the broker
			amqpMessage.Ack(false)

			decodeErr := DecodeAndHandle(&amqpMessage,
				func(request Request){
					service.Receiver.RequestChan <- request
				},
				func(reply Reply){
					//service.Receiver.ReplyChan <- reply
					logging.Log.Error("Received an unexpected reply")
				},
				func(alert Alert){
					service.Receiver.AlertChan <- alert
				},
				func(info Info){
					service.Receiver.InfoChan <- info
				},
			)
			if decodeErr != nil {
				logging.Log.Error("An error occurred while decoding a message: \n\t%v", decodeErr)
				continue amqpLoop
			}

			//logging.Log.Printf("[amqp receiver] Message:\n\t%v", p8Message)
		} // end select block
	} // end for loop

	service.DoneSignal <- true
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

	//logging.Log.Printf("[amqp sender] Encoded message:\n\t%v", amqpMessage)
	logging.Log.Debug("Sending message to routing key <%s>", (*message).Target)

	// Publish!
	pubErr := channel.Publish((*message).exchange, (*message).Target, false, false, amqpMessage)
	if pubErr != nil {
		logging.Log.Error("Error while sending message:\n\t%v", pubErr)
	}
}

