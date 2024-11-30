package main

import (
	"crypto/tls"
	"encoding/json"
	"fmt"
	"net/http"

	mail "github.com/xhit/go-simple-mail/v2"
)

const (
	BACKEND_ADDRESS = "localhost"
	BACKEND_PORT    = 7331
)

type User struct {
	Server   string `json:"smtp"`
	Login    string `json:"login"`
	Password string `json:"password"`
}

type Email struct {
	Subject string `json:"subject"`
	To      string `json:"to"`
	Message string `json:"message"`
}

type Content struct {
	Email Email
	User  User
}

func createSmtpClient(user User) *mail.SMTPServer {
	server := mail.NewSMTPClient()

	server.Host = user.Server
	server.Port = 587

	server.Username = user.Login
	server.Password = user.Password
	server.Encryption = mail.EncryptionSTARTTLS

	server.KeepAlive = false
	server.TLSConfig = &tls.Config{InsecureSkipVerify: true}

	return server
}

func isPasswordCorrect(server *mail.SMTPServer) bool {
	_, err := server.Connect()
	return err == nil
}

func sendEmail(server *mail.SMTPServer, email Email) error {
	client, _ := server.Connect()

	mail := mail.NewMSG().
		SetFrom(fmt.Sprintf("<%s>", server.Username)).
		AddTo(email.To).
		SetSubject(email.Subject).
		SetBody(mail.TextPlain, email.Message)

	if mail.Error != nil {
		return mail.Error
	}

	err := mail.Send(client)
	return err
}

func LoginApiMethod(w http.ResponseWriter, r *http.Request) {
	var user User
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, "Invalid request payload", http.StatusBadRequest)
		return
	}

	srv := createSmtpClient(user)
	w.Header().Set("Content-Type", "application/json")

	fmt.Println(user, isPasswordCorrect(srv))

	if isPasswordCorrect(srv) {
		fmt.Fprint(w, "true")
	} else {
		fmt.Fprint(w, "false")
	}
}

func SendEmailApiMethod(w http.ResponseWriter, r *http.Request) {
	var content Content
	err := json.NewDecoder(r.Body).Decode(&content)
	if err != nil {
		http.Error(w, "Invalid request payload", http.StatusBadRequest)
		return
	}

	fmt.Println(content)

	srv := createSmtpClient(content.User)
	w.Header().Set("Content-Type", "application/json")

	err = sendEmail(srv, content.Email)
	fmt.Println(err)

	var answer string

	if err == nil {
		answer = "Сообщение отправлено!"
	} else {
		answer = err.Error()
	}

	fmt.Fprint(w, answer)
}

func main() {
	http.HandleFunc("/api/login", LoginApiMethod)
	http.HandleFunc("/api/send", SendEmailApiMethod)

	http.ListenAndServe(fmt.Sprintf("%s:%d", BACKEND_ADDRESS, BACKEND_PORT), nil)
}
