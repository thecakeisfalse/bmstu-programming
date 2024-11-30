package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"time"

	"github.com/gorilla/websocket"
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

const (
	BACKEND_ADDRESS  = "localhost"
	BACKEND_PORT     = 7331
	FRONTEND_ADDRESS = "localhost"
	FRONTEND_PORT    = 8080
)

var sessionData = make(map[string]User)

func HomePage(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, "index.html")
}

func EmailPage(w http.ResponseWriter, r *http.Request) {
	cookie, err := r.Cookie("session_token")
	fmt.Println(r.Cookies(), cookie)

	if err != nil {
		http.Redirect(w, r, "/", http.StatusFound)
		return
	}

	if _, exists := sessionData[cookie.Value]; !exists {
		http.Redirect(w, r, "/", http.StatusFound)
		return
	}
	http.ServeFile(w, r, "email.html")
}

func LoginApiMethod(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Invalid request method", http.StatusMethodNotAllowed)
		return
	}

	var user User
	err := json.NewDecoder(r.Body).Decode(&user)
	if err != nil {
		http.Error(w, "Invalid request payload", http.StatusBadRequest)
		return
	}

	// Make POST-req to backend

	jsonData, err := json.Marshal(user)
	if err != nil {
		fmt.Println("Error marshaling JSON:", err)
		return
	}

	fmt.Println(user)

	rs, err := http.Post(fmt.Sprintf("http://%s:%d/api/login", BACKEND_ADDRESS, BACKEND_PORT), "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		log.Fatal(err)
	}

	defer rs.Body.Close()
	data := make([]byte, 1024)
	_, _ = rs.Body.Read(data)

	success := string(data)[:4] == "true"

	// --> result

	w.Header().Set("Content-Type", "application/json")

	if success {

		sessionToken := fmt.Sprintf("%d", time.Now().UnixNano())

		sessionData[sessionToken] = user

		http.SetCookie(w, &http.Cookie{
			Name:     "session_token",
			Value:    sessionToken,
			Expires:  time.Now().Add(24 * time.Hour),
			HttpOnly: false,
			Path:     "/",
		})

		json.NewEncoder(w).Encode(map[string]string{"success": "true"})

		fmt.Println(sessionData)
	} else {
		json.NewEncoder(w).Encode(map[string]string{"success": "false", "message": "Invalid email or password"})
	}
}

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true
	},
}

var Callbacks []*websocket.Conn

func SendEmailApiMethod(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Invalid request method", http.StatusMethodNotAllowed)
		return
	}

	cookie, err := r.Cookie("session_token")
	if err != nil {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	user, exists := sessionData[cookie.Value]
	if !exists {
		http.Error(w, "Unauthorized", http.StatusUnauthorized)
		return
	}

	var email Email

	if err := json.NewDecoder(r.Body).Decode(&email); err != nil {
		http.Error(w, "Invalid request payload", http.StatusBadRequest)
		return
	}

	// Make POST-req to backend

	jsonData, err := json.Marshal(Content{User: user, Email: email})
	if err != nil {
		fmt.Println("Error marshaling JSON:", err)
		return
	}

	rs, err := http.Post(fmt.Sprintf("http://%s:%d/api/send", BACKEND_ADDRESS, BACKEND_PORT), "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		log.Fatal(err)
	}

	defer rs.Body.Close()
	data := make([]byte, 1024)
	_, _ = rs.Body.Read(data)

	for _, conn := range Callbacks {
		conn.WriteMessage(websocket.TextMessage, data)
	}

	// ===

	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"success": "true"})
}

func main() {
	http.HandleFunc("/", HomePage)
	http.HandleFunc("/email", EmailPage)

	http.HandleFunc("/api/login", LoginApiMethod)
	http.HandleFunc("/api/send", SendEmailApiMethod)

	http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		if webSocket, err := upgrader.Upgrade(w, r, nil); err == nil {
			Callbacks = append(Callbacks, webSocket)
		}
	})

	http.ListenAndServe(fmt.Sprintf("%s:%d", FRONTEND_ADDRESS, FRONTEND_PORT), nil)
}
