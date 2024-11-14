package main

import (
	"database/sql"
	"fmt"
	"log"
	"net/http"
	"time"

	_ "github.com/go-sql-driver/mysql"
	"github.com/gorilla/websocket"
)

const (
	DASHBOARD_ADDRESS = "localhost"
	DASHBOARD_PORT    = 8080
	MYSQL_USERNAME    = "xxx"
	MYSQL_PASSWORD    = "xxx"
	MYSQL_ADDRESS     = "localhost"
	MYSQL_DATABASE    = "xxx"
	MYSQL_TABLE       = "username"
)

type RssPost struct {
	Title   string
	Summary string
	Link    string
	Date    int64
}

type Database struct {
	Handler *sql.DB
}

func initDB() *Database {
	dsn := fmt.Sprintf("%s:%s@tcp(%s:3306)/%s", MYSQL_USERNAME, MYSQL_PASSWORD, MYSQL_ADDRESS, MYSQL_DATABASE)

	db, err := sql.Open("mysql", dsn)

	if err != nil {
		log.Fatalf("Ошибка подключения к базе данных: %v", err)
	}

	if err = db.Ping(); err != nil {
		log.Fatalf("Ошибка проверки соединения с базой данных: %v", err)
	}

	return &Database{
		Handler: db,
	}
}

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true
	},
}

var Callbacks []*websocket.Conn

func dbMonitor() {
	db := initDB()
	defer db.Handler.Close()

	var lastID int

	for {
		req := fmt.Sprintf("SELECT * FROM `%s` WHERE `id` > %d", MYSQL_TABLE, lastID)
		rows, err := db.Handler.Query(req)

		if err != nil {
			log.Println("Ошибка базы данных:", err)
			time.Sleep(2 * time.Second)
			continue
		}

		for rows.Next() {
			var id int
			var post RssPost
			if err := rows.Scan(&id, &post.Title, &post.Summary, &post.Link, &post.Date); err != nil {
				log.Println("Ошибка при чтении строки:", err)
				continue
			}
			lastID = id
			message := fmt.Sprintf("Новое изменение: ID=%d, Data=%v", id, post)
			for _, conn := range Callbacks {
				conn.WriteMessage(websocket.TextMessage, []byte(message))
			}
		}

		time.Sleep(2 * time.Second)
		rows.Close()
	}
}

func main() {
	go dbMonitor()

	go http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		if webSocket, err := upgrader.Upgrade(w, r, nil); err == nil {
			Callbacks = append(Callbacks, webSocket)
		}
	})

	http.ListenAndServe(fmt.Sprintf("%s:%d", DASHBOARD_ADDRESS, DASHBOARD_PORT), nil)
}
