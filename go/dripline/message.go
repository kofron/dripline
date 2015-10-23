/*
* amqp.go
*
* the amqp functions takes care of communication via the AMQP protocol
*
* Two threads can be used for receiving and sending AMQP messages, respectively: AmqpReceiver and AmqpSender
 */

package dripline

import (
	"fmt"
	"time"
	"unsafe"

	"github.com/streadway/amqp"
	"github.com/ugorji/go/codec"

	"github.com/project8/swarm/Go/logging"
)

type SenderInfo struct {
	Package  string
	Exe      string
	Version  string
	Commit   string
	Hostname string
	Username string
}

type Message struct {
	exchange   string
    Target     string
	Encoding   string
	ReplyTo    string
	CorrId     string
	MsgType    MsgCodeT
	TimeStamp  string
	SenderInfo
	Payload    interface{}
}

type Request struct {
    Message
	MsgOp         MsgCodeT
}

type Reply struct {
    Message
	RetCode       MsgCodeT
	ReturnMessage string
}

type Alert struct {
    Message
}

type Info struct {
    Message
}

func (message *Message) messageBuffer() (buffer map[string]interface{}) {
	var senderInfo = map[string]interface{} {
		"package":  (*message).SenderInfo.Package,
		"exe":      (*message).SenderInfo.Exe,
		"version":  (*message).SenderInfo.Version,
		"commit":   (*message).SenderInfo.Commit,
		"hostname": (*message).SenderInfo.Hostname,
		"username": (*message).SenderInfo.Username,
	}
	buffer = make(map[string]interface{})
	buffer["msgtype"] = (*message).MsgType
	buffer["timestamp"] = (*message).TimeStamp
	buffer["sender_info"] = senderInfo
	buffer["payload"] = (*message).Payload
	return
}

// Encode converts an Request message into a byte stream
func (message *Request) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	buffer["msgop"] = (*message).MsgOp
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

// Encode converts an Reply message into a byte stream
func (message *Reply) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	buffer["retcode"] = (*message).RetCode
	buffer["return_msg"] = (*message).ReturnMessage
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

// Encode converts an Alert message into a byte stream
func (message *Alert) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

// Encode converts an Info message into a byte stream
func (message *Info) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

func encodeBuffer(buffer *map[string]interface{}, encoding string) (encoded []byte, e error) {
	encoded = make([]byte, 0, unsafe.Sizeof(*buffer))
	switch encoding {
	case "application/json":
		//log.Printf("this will be a json message")
		handle := new(codec.JsonHandle)
		encoder := codec.NewEncoderBytes(&(encoded), handle)
		jsonErr := encoder.Encode(buffer)
		if jsonErr != nil {
			e = fmt.Errorf("Unable to encode JSON-encoded message: %v", jsonErr)
		}
	case "application/msgpack":
		//log.Printf("this will be a msgpack message")
		handle := new(codec.MsgpackHandle)
		encoder := codec.NewEncoderBytes(&(encoded), handle)
		msgpackErr := encoder.Encode(buffer)
		if msgpackErr != nil {
			e = fmt.Errorf("Unable to encode msgpack-encoded message: %v", msgpackErr)
		}
	default:
		e = fmt.Errorf("Message content cannot be encoded with type <%s>", encoding)
	}
	return
}

// DecodeAndHandle converts an AMQP Delivery into one of the message objects, and calls the relevant callback function on it
func DecodeAndHandle(amqpMessage *(amqp.Delivery), reqFunc func(Request), replyFunc func(Reply), alertFunc func(Alert), infoFunc func(Info)) (e error) {
	if buffer, message, msgErr := decode(amqpMessage); msgErr != nil {
		e = msgErr
		return
	} else {
		message.ReplyTo = amqpMessage.ReplyTo
		switch message.MsgType {
		case MTRequest:
			if msgopIfc, msgopPresent := buffer["msgop"]; msgopPresent == false {
				logging.Log.Error("Request message is missing a required element:\n\tmsgop: %v", msgopPresent)
				e = fmt.Errorf("Request message is missing a required element:\n\tmsgop: %v", msgopPresent)
				return
			} else {
				request := Request {
					Message: message,
					MsgOp:   ConvertToMsgCode(msgopIfc),
				}
				reqFunc(request)
				return
			}
		case MTReply:
			retcodeIfc, retcodePresent := buffer["retcode"]
			if retcodePresent == false {
				e = fmt.Errorf("Message is missing a required element: retcode: %v", retcodePresent)
				return
			}
			retmsgIfc, retmsgPresent := buffer["return_msg"]
			if retmsgPresent == false {
				e = fmt.Errorf("Message is missing a required element: return_msg: %v", retmsgPresent)
				return
			}
			reply := Reply {
				Message: message,
				RetCode: ConvertToMsgCode(retcodeIfc),
				ReturnMessage: retmsgIfc.(string),
			}
			replyFunc(reply)
			return
		case MTAlert:
			alert := Alert {
				Message: message,
			}
			alertFunc(alert)
			return
		case MTInfo:
			info := Info {
				Message: message,
			}
			infoFunc(info)
			return
		default:
			e = fmt.Errorf("Unknown message type: %v", message.MsgType)
			return
		}
	}
	return
}

func decode(amqpMessage *amqp.Delivery) (buffer map[string]interface{}, message Message, e error) {
	buffer, e = decodeBuffer(amqpMessage.Body, amqpMessage.ContentEncoding)
	if e != nil {
		return
	}

	// Message contents validation
	// required elements: msgtype, timestamp, sender_info
	msgTypeIfc, msgtypePresent := buffer["msgtype"]
	timestampIfc, timestampPresent := buffer["timestamp"]
	senderInfoIfc, senderInfoPresent := buffer["sender_info"]
	if msgtypePresent && timestampPresent && senderInfoPresent == false {
		e = fmt.Errorf("Message is missing a required element:\n\tmsgtype: %v\n\ttimestamp: %v\n\tsender_info: %v", msgtypePresent, timestampPresent, senderInfoPresent)
		return
	}
	msgType := ConvertToMsgCode(msgTypeIfc)

	// Translate the body of the message into a P8Message object
	senderInfo := senderInfoIfc.(map[interface{}]interface{})
	message = Message {
		Target:     amqpMessage.RoutingKey,
		Encoding:   amqpMessage.ContentEncoding,
		CorrId:     amqpMessage.CorrelationId,
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

	if payloadIfc, hasPayload := buffer["payload"]; hasPayload {
		message.Payload = payloadIfc
	}
	return
}

func decodeBuffer(encoded []byte, encoding string) (buffer map[string]interface{}, e error) {
	switch encoding {
	case "application/json":
		//log.Printf("this is a json message")
		handle := new(codec.JsonHandle)
		decoder := codec.NewDecoderBytes(encoded, handle)
		jsonErr := decoder.Decode(&buffer)
		if jsonErr != nil {
			logging.Log.Error("Unable to decode JSON-encoded message:\n\t%v", jsonErr)
			e = jsonErr
		}
	case "application/msgpack":
		//log.Printf("this is a msgpack message")
		handle := new(codec.MsgpackHandle)
		decoder := codec.NewDecoderBytes(encoded, handle)
		msgpackErr := decoder.Decode(&buffer)
		if msgpackErr != nil {
			logging.Log.Error("Unable to decode msgpack-encoded message:\n\t%v", msgpackErr)
			e = msgpackErr
		}
	default:
		logging.Log.Error("Message content encoding is not understood: %s", encoding)
		e = fmt.Errorf("Message content encoding is not understood: %s", encoding)
	}
	return
}

// PrepareSenderInfo sets up the fields in a SenderInfo object.
func PrepareSenderInfo(package_name, exe, version, commit, hostname, username string) (info SenderInfo) {
	info = SenderInfo {
		Package:  package_name,
		Exe:      exe,
		Version:  version,
		Commit:   commit,
		Hostname: hostname,
		Username: username,
	}
	return
}

// PrepareRequest sets up most of the fields in a Request object.
// The payload is not set here.
func PrepareRequest(target string, encoding string, msgOp MsgCodeT, senderInfo SenderInfo) (message Request) {
	message = Request {
		Message:       Message {
			exchange:      "requests",
			Target:        target,
			Encoding:      encoding,
			MsgType:       MTRequest,
			TimeStamp:     time.Now().UTC().Format(TimeFormat),
			SenderInfo:    senderInfo,
		},
		MsgOp:         msgOp,
	}
	return
}

// PrepareReply sets up most of the fields in a Reply object.
// The payload is not set here.
func PrepareReply(target string, encoding string, corrId string, retCode MsgCodeT, returnMessage string, senderInfo SenderInfo) (message Reply) {
	message = Reply {
		Message:     Message {
			exchange:      "",
			Target:        target,
			Encoding:      encoding,
			CorrId:        corrId,
			MsgType:       MTReply,
			TimeStamp:     time.Now().UTC().Format(TimeFormat),
			SenderInfo:    senderInfo,
		},
		RetCode:    retCode,
		ReturnMessage: returnMessage,
	}
	return
}

func PrepareReplyToRequest(request Request, retCode MsgCodeT, returnMessage string, senderInfo SenderInfo) (message Reply) {
	message = PrepareReply(request.ReplyTo, request.Encoding, request.CorrId, retCode, returnMessage, senderInfo)
	return
}

// PrepareAlert sets up most of the fields in a Alert object.
// The payload is not set here.
func PrepareAlert(target string, encoding string, senderInfo SenderInfo) (message Alert) {
	message = Alert {
		Message:  Message {
			exchange:      "alerts",
			Target:        target,
			Encoding:      encoding,
			MsgType:       MTAlert,
			TimeStamp:     time.Now().UTC().Format(TimeFormat),
			SenderInfo:    senderInfo,
		},
	}
	return
}

// PrepareInfo sets up most of the fields in a Alert object.
// The payload is not set here.
func PrepareInfo(target string, encoding string, corrId string, senderInfo SenderInfo) (message Info) {
	message = Info {
		Message:  Message {
			exchange:      "",
			Target:        target,
			Encoding:      encoding,
			MsgType:       MTInfo,
			TimeStamp:     time.Now().UTC().Format(TimeFormat),
			SenderInfo:    senderInfo,
		},
	}
	return
}
