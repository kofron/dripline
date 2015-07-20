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

	"github.com/ugorji/go/codec"
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
	ReplyChan     chan Reply
	ReplyReceiver *AmqpReceiver
}

type Reply struct {
    Message
	RetCode    MsgCodeT
}

type Alert struct {
    Message
}

type Info struct {
    Message
}

func (message *Message) messageBuffer() (buffer map[string]interface{}) {
	var senderInfo = map[string]interface{} {
		"package": (*message).SenderInfo.Package,
		"exe": (*message).SenderInfo.Exe,
		"version": (*message).SenderInfo.Version,
		"commit": (*message).SenderInfo.Commit,
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

func (message *Request) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	buffer["msgop"] = (*message).MsgOp
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

func (message *Reply) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	buffer["retcode"] = (*message).RetCode
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

func (message *Alert) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

func (message *Info) Encode() (body []byte, e error) {
	buffer := (*message).messageBuffer()
	body, e = encodeBuffer(&buffer, (*message).Encoding)
	return
}

func encodeBuffer(bufferPtr *map[string]interface{}, encoding string) (encoded []byte, e error) {
	encoded = make([]byte, 0, unsafe.Sizeof(*bufferPtr))
	switch encoding {
	case "application/json":
		//log.Printf("this will be a json message")
		handle := new(codec.JsonHandle)
		encoder := codec.NewEncoderBytes(&(encoded), handle)
		jsonErr := encoder.Encode(bufferPtr)
		if jsonErr != nil {
			e = fmt.Errorf("Unable to decode JSON-encoded message: %v", jsonErr)
		}
	case "application/msgpack":
		//log.Printf("this will be a msgpack message")
		handle := new(codec.MsgpackHandle)
		encoder := codec.NewEncoderBytes(&(encoded), handle)
		msgpackErr := encoder.Encode(bufferPtr)
		if msgpackErr != nil {
			e = fmt.Errorf("Unable to decode msgpack-encoded message: %v", msgpackErr)
		}
	default:
		e = fmt.Errorf("Message content cannot be encoded with type <%s>", encoding)
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
func PrepareRequest(target string, encoding string, msgOp MsgCodeT, senderInfo SenderInfo, replyChan chan Reply, replyReceiver *AmqpReceiver) (message Request) {
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
		ReplyChan:     replyChan,
		ReplyReceiver: replyReceiver,
	}
	return
}

// PrepareReply sets up most of the fields in a Reply object.
// The payload is not set here.
func PrepareReply(target string, encoding string, corrId string, retCode MsgCodeT, senderInfo SenderInfo) (message Reply) {
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
	}
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
