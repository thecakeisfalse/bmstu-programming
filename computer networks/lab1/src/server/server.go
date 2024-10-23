package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"net"
	"sync"
	"time"

	log "github.com/mgutz/logxi/v1"
)

// Client - состояние клиента.
type Client struct {
	logger    log.Logger    // Объект для печати логов
	conn      *net.TCPConn  // Объект TCP-соединения
	enc       *json.Encoder // Объект для кодирования и отправки сообщений
	reminders []Reminder    // Список напоминаний
	mu        sync.Mutex
}

// NewClient - конструктор клиента, принимает в качестве параметра
// объект TCP-соединения.
func NewClient(conn *net.TCPConn) *Client {
	return &Client{
		logger:    log.New(fmt.Sprintf("client %s", conn.RemoteAddr().String())),
		conn:      conn,
		enc:       json.NewEncoder(conn),
		reminders: make([]Reminder, 0),
	}
}

// serve - метод, в котором реализован цикл взаимодействия с клиентом.
// Подразумевается, что метод serve будет вызаваться в отдельной go-программе.
func (client *Client) serve() {
	defer client.conn.Close()
	decoder := json.NewDecoder(client.conn)

	ticker := time.NewTicker(1 * time.Second)
	defer ticker.Stop()

	go func() {
		for range ticker.C {
			client.checkReminders()
		}
	}()

	for {
		var req Request
		if err := decoder.Decode(&req); err != nil {
			client.logger.Error("cannot decode message", "reason", err)
			break
		} else {
			client.logger.Info("received command", "command", req.Command)
			if client.handleRequest(&req) {
				client.logger.Info("shutting down connection")
				break
			}
		}
	}
}

// handleRequest - метод обработки запроса от клиента. Он возвращает true,
// если клиент передал команду "quit" и хочет завершить общение.
func (client *Client) handleRequest(req *Request) bool {
	switch req.Command {
	case "quit":
		client.respond("ok", nil)
		return true
	case "remind":
		errorMsg := ""
		if req.Data == nil {
			errorMsg = "data field is absent"
		} else {
			var message Reminder
			if err := json.Unmarshal(*req.Data, &message); err != nil {
				errorMsg = "malformed data field"
			} else {
				client.addReminder(message.Message, message.When)
			}
		}
		if errorMsg == "" {
			client.respond("ok", nil)
		} else {
			client.logger.Error("addition failed", "reason", errorMsg)
			client.respond("failed", errorMsg)
		}
	default:
		client.logger.Error("unknown command")
		client.respond("failed", "unknown command")
	}
	return false
}

func (client *Client) addReminder(message string, when int64) {
	client.mu.Lock()
	defer client.mu.Unlock()
	client.reminders = append(client.reminders, Reminder{
		Message: message,
		When:    when,
	})
	client.logger.Info("reminder added", "message", message, "time", when)
}

func (client *Client) checkReminders() {
	client.mu.Lock()
	defer client.mu.Unlock()

	now := time.Now().Unix()
	for i := 0; i < len(client.reminders); {
		if client.reminders[i].When <= now {
			client.sendReminder(client.reminders[i].Message)
			client.reminders = append(client.reminders[:i], client.reminders[i+1:]...)
		} else {
			i++
		}
	}
}

func (client *Client) sendReminder(message string) {
	client.respond("remind", message)
	client.logger.Info("sent reminder", "message", message)
}

// respond - вспомогательный метод для передачи ответа с указанным статусом
// и данными. Данные могут быть пустыми (data == nil).
func (client *Client) respond(status string, data interface{}) {
	var raw json.RawMessage
	raw, _ = json.Marshal(data)
	client.enc.Encode(&Response{status, &raw})
}

func main() {
	// Работа с командной строкой, в которой может указываться необязательный ключ -addr.
	var addrStr string
	flag.StringVar(&addrStr, "addr", "127.0.0.1:6000", "specify ip address and port")
	flag.Parse()

	// Разбор адреса, строковое представление которого находится в переменной addrStr.
	if addr, err := net.ResolveTCPAddr("tcp", addrStr); err != nil {
		log.Error("address resolution failed", "address", addrStr)
	} else {
		log.Info("resolved TCP address", "address", addr.String())

		// Инициация слушания сети на заданном адресе.
		if listener, err := net.ListenTCP("tcp", addr); err != nil {
			log.Error("listening failed", "reason", err)
		} else {
			// Цикл приёма входящих соединений.
			for {
				if conn, err := listener.AcceptTCP(); err != nil {
					log.Error("cannot accept connection", "reason", err)
				} else {
					log.Info("accepted connection", "address", conn.RemoteAddr().String())

					// Запуск go-программы для обслуживания клиентов.
					go NewClient(conn).serve()
				}
			}
		}
	}
}
