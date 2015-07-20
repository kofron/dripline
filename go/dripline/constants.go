/*
* constants.go
*
* the set of constants that might be used throughout hornet
*
*/

package dripline

// Time format
const TimeFormat = "2006-01-02T22:04:05Z"

// Separator for the routing key/target parts
const TargetSeparator = "."

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
)

