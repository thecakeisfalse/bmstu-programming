package main

import (
	"html/template"
	"net/http"

	log "github.com/mgutz/logxi/v1"
)

const INDEX_HTML = `
    <!doctype html>
    <html lang="ru">
        <head>
            <meta charset="utf-8">
            <title>Курс валют</title>
        </head>
        <body>
            {{if .}}
                {{range .}}
                    <p><b>{{.Code}}</b>: {{.Nominal}} "{{.Name}}" {{.Rate}} {{.Difference}}</p>
                {{end}}
            {{else}}
                Не удалось загрузить курс!
            {{end}}
        </body>
    </html>
    `

var indexHtml = template.Must(template.New("index").Parse(INDEX_HTML))

var logger log.Logger

func serveClient(response http.ResponseWriter, request *http.Request) {
	path := request.URL.Path
	log.Info("got request", "Method", request.Method, "Path", path)
	if path != "/" && path != "/index.html" {
		log.Error("invalid path", "Path", path)
		response.WriteHeader(http.StatusNotFound)
	} else if err := indexHtml.Execute(response, downloadNews()); err != nil {
		log.Error("HTML creation failed", "error", err)
	} else {
		log.Info("response sent to client successfully")
	}
}

func main() {
	who := "mario"
	log.Info("Hello", "who", who)

	http.HandleFunc("/", serveClient)
	log.Info("starting listener")
	log.Error("listener failed", "error", http.ListenAndServe("127.0.0.1:6060", nil))
}
