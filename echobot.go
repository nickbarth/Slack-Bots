package main

import (
	"encoding/json"
	"fmt"
	"golang.org/x/net/websocket"
	"io/ioutil"
	"log"
	"net/http"
	"strings"
)

const SLACK_API_KEY = "xxx"

type SlackRTM struct {
	Self struct {
		ID string `json:"id"`
	} `json:"self"`
	URL string `json:"url"`
}

type Message struct {
	Type    string `json:"type"`
	User    string `json:"user"`
	Text    string `json:"text"`
	Channel string `json:"channel"`
}

func getRTM() SlackRTM {
	var rtm SlackRTM
	url := "https://slack.com/api/rtm.start?token=" + SLACK_API_KEY

	resp, err := http.Get(url)
	if err != nil {
		log.Fatal(err)
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)

	json.Unmarshal(body, &rtm)

	return rtm
}

func receive(ws *websocket.Conn, channel chan Message, id string) {
	for {
		var data string
		var message Message

		err := websocket.Message.Receive(ws, &data)

		if err != nil {
			log.Fatal(err)
		}

		json.Unmarshal([]byte(data), &message)

		if message.Type == "message" && message.Text != "" && strings.HasPrefix(message.Text, "<@"+id+">") {
			message.Text = strings.Replace(message.Text, "<@"+id+">", "", 1)
			channel <- message
		}
	}
}

func send(ws *websocket.Conn, message string, channel string) {
	resp := new(Message)
	resp.Type = "message"
	resp.Text = message
	resp.Channel = channel

	err := websocket.JSON.Send(ws, resp)

	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	// log.SetFlags(log.Flags() &^ (log.Ldate | log.Ltime))
	rtm := getRTM()

	ws, err := websocket.Dial(rtm.URL, "", rtm.URL)

	if err != nil {
		log.Fatal(err)
	}

	messages := make(chan Message)
	go receive(ws, messages, rtm.Self.ID)

	for {
		select {
		case message := <-messages:
			fmt.Println(`message Received:`, message.Text)
			send(ws, message.Text, message.Channel)
		}
	}
}
