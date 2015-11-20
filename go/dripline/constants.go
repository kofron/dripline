/*
* constants.go
*
* the set of constants that might be used throughout hornet
*
*/

package dripline

// Time format
const TimeFormat = "2006-01-02T22:04:05Z"

// Project 8 Wire Protocol Standards
type MsgCodeT uint64

const (
	MTReply   MsgCodeT = 2
	MTRequest MsgCodeT = 3
	MTAlert   MsgCodeT = 4
	MTInfo    MsgCodeT = 5
)

const (
	MOSet     MsgCodeT = 0
	MOGet     MsgCodeT = 1
	MOConfig  MsgCodeT = 6
	MOSend    MsgCodeT = 7
	MORun     MsgCodeT = 8
	MOCommand MsgCodeT = 9
)

const (
	RCSuccess MsgCodeT = 0
	RCWarnNoAction  MsgCodeT = 1
	RCErrAMQP       MsgCodeT = 100
	RCErrAMQPConn   MsgCodeT = 101
	RCErrAMQPRK     MsgCodeT = 102
	RCErrHW         MsgCodeT = 200
	RCErrHWConn     MsgCodeT = 201
	RCErrHWNoResp   MsgCodeT = 202
	RCErrDrip       MsgCodeT = 300
	RCErrDripNoEnc  MsgCodeT = 301
	RCErrDripDecFail MsgCodeT = 302
	RCErrDripPayload MsgCodeT = 303
	RCErrDripValue   MsgCodeT = 304
	RCErrDripTimeout MsgCodeT = 305
	RCErrDripMethod  MsgCodeT = 306
	RCErrDripAccDen  MsgCodeT = 307
	RCErrDripInvKey  MsgCodeT = 308
	RCErrDB          MsgCodeT = 400
	RCErrUnhandled   MsgCodeT = 999
)

