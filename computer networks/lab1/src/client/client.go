package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"net"
	"strconv"
	"time"

	"github.com/skorobogatov/input"
)

// interact - функция, содержащая цикл взаимодействия с сервером.
func interact(conn *net.TCPConn) {
	defer conn.Close()
	encoder, decoder := json.NewEncoder(conn), json.NewDecoder(conn)

	done := make(chan struct{})

	go func() {
		for {
			var resp Response
			if err := decoder.Decode(&resp); err != nil {
				fmt.Printf("error: %v\n", err)
				break
			}

			// Обработка ответа от сервера
			switch resp.Status {
			case "ok":
				fmt.Printf("ok\n")
			case "remind":
				var message string
				if resp.Data == nil {
					fmt.Printf("error: data field is absent in response\n")
				} else {
					if err := json.Unmarshal(*resp.Data, &message); err != nil {
						fmt.Printf("error: malformed data field in response\n")
					} else {
						fmt.Printf("reminder: %s\n", message)
					}
				}
			case "failed":
				var errorMsg string
				if resp.Data != nil {
					if err := json.Unmarshal(*resp.Data, &errorMsg); err != nil {
						fmt.Printf("error: malformed data field in response\n")
					} else {
						fmt.Printf("failed: %s\n", errorMsg)
					}
				} else {
					fmt.Printf("failed: no additional data\n")
				}
			default:
				fmt.Printf("error: server reports unknown status %q\n", resp.Status)
			}
		}
		// Сигнализируем о завершении при ошибке
		done <- struct{}{}
	}()

	for {
		select {
		case <-done:
			// Завершаем выполнение, если произошла ошибка в приёме данных
			return
		default:
			// Чтение команды из стандартного потока ввода
			fmt.Printf("command = ")
			command := input.Gets()

			// Отправка запроса
			switch command {
			case "quit":
				send_request(encoder, "quit", nil)

				return
			case "remind":
				var message Reminder
				fmt.Printf("message = ")
				message.Message = input.Gets()
				fmt.Printf("delay (in seconds) = ")
				temp := input.Gets()
				delay, _ := strconv.Atoi(temp)
				message.When = time.Now().Unix() + int64(delay)
				send_request(encoder, "remind", &message)
			default:
				fmt.Printf("error: unknown command\n")
			}
		}
	}
}

// send_request - вспомогательная функция для передачи запроса с указанной командой
// и данными. Данные могут быть пустыми (data == nil).
func send_request(encoder *json.Encoder, command string, data interface{}) {
	var raw json.RawMessage
	raw, _ = json.Marshal(data)
	encoder.Encode(&Request{command, &raw})
}

func main() {
	// Работа с командной строкой, в которой может указываться необязательный ключ -addr.
	var addrStr string
	flag.StringVar(&addrStr, "addr", "127.0.0.1:6000", "specify ip address and port")
	flag.Parse()

	// Разбор адреса, установка соединения с сервером и
	// запуск цикла взаимодействия с сервером.
	if addr, err := net.ResolveTCPAddr("tcp", addrStr); err != nil {
		fmt.Printf("error: %v\n", err)
	} else if conn, err := net.DialTCP("tcp", nil, addr); err != nil {
		fmt.Printf("error: %v\n", err)
	} else {
		interact(conn)
	}
}
