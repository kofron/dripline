/*
* amqp.go
*
* the amqp functions takes care of communication via the AMQP protocol
*
* Two threads can be used for receiving and sending AMQP messages, respectively: AmqpReceiver and AmqpSender
 */
package dripline
/*
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
*/
type AmqpReceiver struct {
	BrokerAddress string
	sendQueue chan Message
	stopQueue chan bool
	DoneSignal chan bool
}


/*

// AmqpReceiver is a goroutine for receiving and handling AMQP messages
func AmqpReceiver(ctrlQueue chan ControlMessage, reqQueue chan ControlMessage, poolCount *sync.WaitGroup) {
	// decrement the wg counter at the end
	defer poolCount.Done()
	defer Log.Info("[AMQP receiver is finished.")

	// Connect to the AMQP broker
	// Deferred command: close the connection
	brokerAddress := viper.GetString("amqp.broker")
	if viper.GetBool("amqp.use-auth") {
		if Authenticators.Amqp.Available == false {
			Log.Critical("AMQP authentication is not available")
			reqQueue <- StopExecution
			return
		}
		brokerAddress = Authenticators.Amqp.Username + ":" + Authenticators.Amqp.Password + "@" + brokerAddress
	}
	brokerAddress = "amqp://" + brokerAddress
	if viper.IsSet("amqp.port") {
		brokerAddress = brokerAddress + ":" + viper.GetString("amqp.port")
	}
	connection, receiveErr := amqp.Dial(brokerAddress)
	if receiveErr != nil {
		Log.Critical("Unable to connect to the AMQP broker at (%s) for receiving:\n\t%v", brokerAddress, receiveErr.Error())
		reqQueue <- StopExecution
		return
	}
	defer connection.Close()

	// Create the channel object that represents the connection to the broker
	// Deferred command: close the channel
	channel, chanErr := connection.Channel()
	if chanErr != nil {
		Log.Critical("Unable to get the AMQP channel:\n\t%v", chanErr.Error())
		reqQueue <- StopExecution
		return
	}
	defer channel.Close()

	// Create the exchange if it doesn't already exist
	exchangeName := viper.GetString("amqp.exchange")
	exchDeclErr := channel.ExchangeDeclare(exchangeName, "topic", false, false, false, false, nil)
	if exchDeclErr != nil {
		Log.Critical("Unable to declare exchange <%s>:\n\t%v", exchangeName, exchDeclErr.Error())
		reqQueue <- StopExecution
		return
	}

	// Declare the "hornet" queue
	// Deferred command: delete the "hornet" queue
	queueName := viper.GetString("amqp.queue")
	_, queueDeclErr := channel.QueueDeclare(queueName, false, true, true, false, nil)
	if queueDeclErr != nil {
		Log.Critical("Unable to declare queue <%s>:\n\t%v", queueName, queueDeclErr.Error())
		reqQueue <- StopExecution
		return
	}
	defer func() {
		if _, err := channel.QueueDelete(queueName, false, false, false); err != nil {
			Log.Error("Error while deleting queue:\n\t%v", err)
		}
	}()

	// Bind the "hornet" queue to the exchange, and subscribe it to all routing keys that start with "hornet"
	// Deferred command: unbind the "hornet" queue from the exchange
	queueBindErr := channel.QueueBind(queueName, queueName+".#", exchangeName, false, nil)
	if queueBindErr != nil {
		Log.Critical("Unable to bind queue <%s> to exchange <%s>:\n\t%v", queueName, exchangeName, queueBindErr.Error())
		reqQueue <- StopExecution
		return
	}
	defer func() {
		if err := channel.QueueUnbind(queueName, queueName+".#", exchangeName, nil); err != nil {
			Log.Error("Error while unbinding queue:\n\t%v", err)
		}
	}()

	// Start consuming messages on the queue
	// Channel::Cancel is not executed as a deferred command, because consuming will be stopped by Channel.Close
	messageQueue, consumeErr := channel.Consume(queueName, "", false, true, true, false, nil)
	if consumeErr != nil {
		Log.Critical("Unable start consuming from queue <%s>:\n\t%v", queueName, queueBindErr.Error())
		reqQueue <- StopExecution
		return
	}

	Log.Info("AMQP Receiver started successfully")
	AmqpReceiverIsActive = true
	defer func() {AmqpReceiverIsActive = false}()

amqpLoop:
	for {
		select {
		// the control messages can stop execution
		case controlMsg := <-ctrlQueue:
			if controlMsg == StopExecution {
				Log.Info("AMQP receiver stopping on interrupt.")
				break amqpLoop
			}
		// process any AMQP messages that are received
		case message := <-messageQueue:
			// Send an acknowledgement to the broker
			message.Ack(false)

			// Decode the body of the message
			//log.Printf("[amqp receiver] Received message with encoding %s", message.ContentEncoding)
			var body map[string]interface{}
			switch message.ContentEncoding {
			case "application/json":
				//log.Printf("this is a json message")
				handle := new(codec.JsonHandle)
				decoder := codec.NewDecoderBytes(message.Body, handle)
				jsonErr := decoder.Decode(&body)
				if jsonErr != nil {
					Log.Error("Unable to decode JSON-encoded message:\n\t%v", jsonErr)
					continue amqpLoop
				}
			case "application/msgpack":
				//log.Printf("this is a msgpack message")
				handle := new(codec.MsgpackHandle)
				decoder := codec.NewDecoderBytes(message.Body, handle)
				msgpackErr := decoder.Decode(&body)
				if msgpackErr != nil {
					Log.Error("Unable to decode msgpack-encoded message:\n\t%v", msgpackErr)
					continue amqpLoop
				}
			default:
				Log.Error("Message content encoding is not understood: %s", message.ContentEncoding)
				continue amqpLoop
			}
			//log.Printf("[amqp receiver] Message body:\n\t%v", body)

			// Message contents validation
			// required elements: msgtype, timestamp, sender_info
			msgTypeIfc, msgtypePresent := body["msgtype"]
			timestampIfc, timestampPresent := body["timestamp"]
			senderInfoIfc, senderInfoPresent := body["sender_info"]
			if msgtypePresent && timestampPresent && senderInfoPresent == false {
				Log.Error("Message is missing a required element:\n\tmsgtype: %v\n\ttimestamp: %v\n\tsender_info: %v", msgtypePresent, timestampPresent, senderInfoPresent)
				continue amqpLoop
			}
			msgType := ConvertToMsgCode(msgTypeIfc)

			// Translate the body of the message into a P8Message object
			senderInfo := senderInfoIfc.(map[interface{}]interface{})
			p8Message := P8Message {
				Encoding:   message.ContentEncoding,
				CorrId:     message.CorrelationId,
				MsgType:    msgType,
				TimeStamp:  ConvertToString(timestampIfc),
				SenderInfo: SenderInfo{
					Package:  ConvertToString(senderInfo["package"]),
					Exe:      ConvertToString(senderInfo["exe"]),
					Version:  ConvertToString(senderInfo["version"]),
					Commit:   ConvertToString(senderInfo["commit"]),
					Hostname: ConvertToString(senderInfo["hostname"]),
					Username: ConvertToString(senderInfo["username"]),
				},
			}

			if payloadIfc, hasPayload := body["payload"]; hasPayload {
				p8Message.Payload = payloadIfc
			}

			// validation for certain types of messages
			switch msgType {
			case MTReply:
				if retcodeIfc, retcodePresent := body["retcode"]; retcodePresent == false {
					Log.Error("Message is missing a required element:\n\tretcode: %v", retcodePresent)
					continue amqpLoop
				} else {
					p8Message.RetCode = ConvertToMsgCode(retcodeIfc)
				}
			case MTRequest:
				
				if msgopIfc, msgopPresent := body["msgop"]; msgopPresent == false {
					Log.Error("Request message is missing a required element:\n\tmsgop: %v", msgopPresent)
					continue amqpLoop
				} else {
					p8Message.MsgOp = ConvertToMsgCode(msgopIfc)
				}
			case MTAlert:
				Log.Error("Cannot handle Alert messages")
			case MTInfo:
				Log.Error("Cannot handle Info messages")
			default:
				Log.Error("Unknown message type: %v", msgType)
			}

			routingKeyParts := strings.Split(message.RoutingKey, TargetSeparator)
			if len(routingKeyParts) > 1 {
				p8Message.Target = routingKeyParts[1:len(routingKeyParts)]
			}

			//log.Printf("[amqp receiver] Message:\n\t%v", p8Message)

			// Handle with the message according to the message type
			switch msgType {
			case MTReply:
				Log.Info("Received reply message: %d", p8Message.RetCode)
				if replyHandlerChan, canReply := replyMap[message.CorrelationId]; canReply {
					replyHandlerChan <- p8Message
				}
			case MTRequest:
				// Handle with the request message according to the target
				if len(p8Message.Target) == 0 {
					Log.Error("No Hornet target provided")
				} else {
					switch p8Message.Target[0] {
					case "quit-hornet":
						reqQueue <- StopExecution
					case "print-message":
						Log.Notice("Message received for printing:")
						Log.Notice("\tEncoding: %v", p8Message.Encoding)
						Log.Notice("\tCorrelation ID: %v", p8Message.CorrId)
						Log.Notice("\tMessage Type: %v", p8Message.MsgType)
						Log.Notice("\tTimestamp: %v", p8Message.TimeStamp)
						Log.Notice("\tSenderInfo:")
						Log.Notice("\t\tPackage: %v", p8Message.SenderInfo.Package)
						Log.Notice("\t\tExe: %v", p8Message.SenderInfo.Exe)
						Log.Notice("\t\tVersion: %v", p8Message.SenderInfo.Version)
						Log.Notice("\t\tCommit: %v", p8Message.SenderInfo.Commit)
						Log.Notice("\t\tHostname: %v", p8Message.SenderInfo.Hostname)
						Log.Notice("\t\tUsername: %v", p8Message.SenderInfo.Username)
						Log.Notice("\tPayload:")
						for key, value := range p8Message.Payload.(map[interface{}]interface{}) {
							switch typedValue := value.(type) {
								case []byte:
									Log.Notice("\t\t%s (byte sl): %v", key.(string), string(typedValue))
								case [][]byte:
									//log.Printf("\t\t%s (byte sl sl): %v", key.(string), [][]string(typedValue))
									sliceString := "["
									for _, byteSlice := range typedValue {
										fmt.Sprintf(sliceString, "%s, %s", sliceString, string(byteSlice))
									}
									fmt.Sprintf(sliceString, "%s]", sliceString)
									Log.Notice("\t\t%s (byte sl sl): %s", key.(string), sliceString)
								case rune, bool, int, uint, float32, float64, complex64, complex128, string:
									Log.Notice("\t\t%s (type): %v", key.(string), typedValue)
								case []rune, []bool, []int, []uint, []float32, []float64, []complex64, []complex128, []string:
									Log.Notice("\t\t%s (array): %v", key.(string), typedValue)
								default:
									Log.Notice("\t\t%s (default): %v", key.(string), value)
							}
						}
					default:
						Log.Error("Unknown hornet target for request messages: %v", p8Message.Target)
					}
				}
			}
		} // end select block
	} // end for loop
}
*/
