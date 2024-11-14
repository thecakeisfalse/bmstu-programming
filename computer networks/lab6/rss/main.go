package main

import (
	"database/sql"
	"fmt"
	"log"
	"time"

	"github.com/SlyMarbo/rss"
	_ "github.com/go-sql-driver/mysql"
)

const (
	MYSQL_USERNAME = "xxx"
	MYSQL_PASSWORD = "xxx"
	MYSQL_ADDRESS  = "localhost"
	MYSQL_DATABASE = "xxx"
	MYSQL_TABLE    = "username"
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

func getRssPosts(url string) []RssPost {
	if rssObject, err := rss.Fetch(url); err == nil {
		posts := []RssPost{}

		for _, item := range rssObject.Items {
			posts = append(posts, RssPost{
				Title:   item.Title,
				Summary: item.Summary,
				Link:    item.Link,
				Date:    item.Date.Unix(),
			})
		}

		return posts
	}

	return nil
}

func postExists(posts []RssPost, newPost RssPost) bool {
	for _, post := range posts {
		if post.Title == newPost.Title && post.Summary == newPost.Summary {
			return true
		}
	}
	return false
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

func (db *Database) fetchPosts() ([]RssPost, error) {
	req := fmt.Sprintf("SELECT * FROM `%s`", MYSQL_TABLE)
	rows, err := db.Handler.Query(req)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var posts []RssPost
	for rows.Next() {
		var post RssPost
		var temp int
		if err := rows.Scan(&temp, &post.Title, &post.Summary, &post.Link, &post.Date); err != nil {
			return nil, err
		}
		posts = append(posts, post)
	}
	return posts, nil
}

func (db *Database) addPost(post RssPost) error {
	req := fmt.Sprintf("INSERT INTO `%s` (`title`, `summary`, `link`, `date`) VALUES (?, ?, ?, ?)", MYSQL_TABLE)
	_, err := db.Handler.Exec(req, post.Title, post.Summary, post.Link, post.Date)
	return err
}

func (db *Database) addNewPosts(posts []RssPost) ([]RssPost, error) {
	existingPosts, err := db.fetchPosts()
	if err != nil {
		fmt.Println(err)
		return nil, err
	}

	newPosts := []RssPost{}
	for _, post := range posts {
		if !postExists(existingPosts, post) {
			fmt.Println(post)
			if err := db.addPost(post); err != nil {
				fmt.Println(err)
				return nil, err
			}
			newPosts = append(newPosts, post)
		}
	}
	return newPosts, nil
}

var db *Database

func main() {
	db = initDB()
	defer db.Handler.Close()

	for {

		posts := getRssPosts("https://news.rambler.ru/rss/tech/")
		if _, err := db.addNewPosts(posts); err != nil {
			fmt.Println(err)
		}

		time.Sleep(15 * time.Second)

	}
}
