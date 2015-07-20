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
	"github.com/spf13/viper"

	"github.com/project8/hornet/gogitver"
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
    Target     []string
	Encoding   string
	CorrId     string
	MsgType    MsgCodeT
	TimeStamp  string
	SenderInfo
	Payload    interface{}
}

type Request struct {
    Message
	MsgOp         MsgCodeT
	ReplyChan     chan Message
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

func (message Message) messageBuffer() (buffer map[string]interface{}) {
	var senderInfo = map[string]interface{} {
		"package": p8Message.SenderInfo.Package,
		"exe": p8Message.SenderInfo.Exe,
		"version": p8Message.SenderInfo.Version,
		"commit": p8Message.SenderInfo.Commit,
		"hostname": p8Message.SenderInfo.Hostname,
		"username": p8Message.SenderInfo.Username,
	}
	buffer["msgtype"] = p8Message.MsgType,
	buffer["timestamp"] = p8Message.TimeStamp,
	buffer["sender_info"] = senderInfo,
	buffer["payload"] = p8Message.Payload
}

func (message Request) Encode() (body []byte, e error) {
	buffer := message.messageBuffer()
	buffer["msgop"] = message.MsgOp
	body, e = encodeBuffer(&buffer, message.Encoding)
}

func (message Reply) Encode() (body []byte, e error) {
	buffer := message.messageBuffer()
	buffer["retcode"] = message.RetCode
	body, e = encodeBuffer(&buffer, message.Encoding)
}

func (message Alert) Encode() (body []byte, e error) {
	buffer := message.messageBuffer()
	body, e = encodeBuffer(&buffer, message.Encoding)
}

func (message Info) Encode() (body []byte, e error) {
	buffer := message.messageBuffer()
	body, e = encodeBuffer(&buffer, message.Encoding)
}

func encodeBuffer(bufferPtr *map[string]interface{}, encoding string) (encoded []byte, e error) {
	encoded = make([]byte, 0, unsafe.Sizeof(*bufferPtr))
	switch encoding {
	case "application/json":
		//log.Printf("this will be a json message")
		handle := new(codec.JsonHandle)
		encoder := codec.NewEncoderBytes(&(message.Body), handle)
		jsonErr := encoder.Encode(&body)
		if jsonErr != nil {
			e = fmt.Errorf("Unable to decode JSON-encoded message: %v", jsonErr)
		}
	case "application/msgpack":
		//log.Printf("this will be a msgpack message")
		handle := new(codec.MsgpackHandle)
		encoder := codec.NewEncoderBytes(&(message.Body), handle)
		msgpackErr := encoder.Encode(&body)
		if msgpackErr != nil {
			e = fmt.Errorf("Unable to decode msgpack-encoded message: %v", msgpackErr)
		}
	default:
		e = fmt.Errorf("Message content cannot be encoded with type <%s>", p8Message.Encoding)
	}
}

// PrepareSenderInfo sets up the fields in a SenderInfo object.
func PrepareSenderInfo(package_name, exe, version, commit, hostname, username string) (info SenderInfo) {
	info = SenderInfo {
		Package  package_name,
		Exe      exe,
		Version  version,
		Commit   commit,
		Hostname hostname,
		Username username,
	}
	return
}

// PrepareRequest sets up most of the fields in a Request object.
// The payload is not set here.
func PrepareRequest(target []string, encoding string, msgOp MsgCodeT, senderInfo SenderInfo, replyChan chan message, replyReceiver *AmqpReceiver) (message Request) {
	message = Request {
		exchange:      "requests",
		Target:        target,
		Encoding:      encoding,
		MsgType:       MTRequest,
		MsgOp:         msgOp,
		TimeStamp:     time.Now().UTC().Format(TimeFormat),
		SenderInfo:    senderInfo,
		ReplyChan:     replyChan,
		ReplyReceiver: replyReceiver,
	}
	return
}

// PrepareReply sets up most of the fields in a Reply object.
// The payload is not set here.
func PrepareReply(target []string, encoding string, corrId string, retCode MsgCodeT, senderInfo SenderInfo) (message Reply) {
	message = Reply {
		exchange:   "",
		Target:     target,
		Encoding:   encoding,
		CorrId:     corrId,
		MsgType:    MTReply,
		RetCode:    retCode,
		TimeStamp:  time.Now().UTC().Format(TimeFormat),
		SenderInfo: senderInfo
	}
	return
}

// PrepareAlert sets up most of the fields in a Alert object.
// The payload is not set here.
func PrepareAlert(target []string, encoding string, corrId string, senderInfo SenderInfo) (message Alert) {
	message = Alert {
		exchange:   "alerts",
		Target:     target,
		Encoding:   encoding,
		CorrId:     corrId,
		MsgType:    MTAlert,
		TimeStamp:  time.Now().UTC().Format(TimeFormat),
		SenderInfo: senderInfo
	}
	return
}

// PrepareInfo sets up most of the fields in a Alert object.
// The payload is not set here.
func PrepareAlert(target []string, encoding string, corrId string, senderInfo SenderInfo) (message Info) {
	message = Info {
		exchange:   "",
		Target:     target,
		Encoding:   encoding,
		CorrId:     corrId,
		MsgType:    MTInfo,
		TimeStamp:  time.Now().UTC().Format(TimeFormat),
		SenderInfo: senderInfo
	}
	return
}
