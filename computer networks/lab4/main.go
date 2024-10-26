package main

import (
	"errors"
	"fmt"
	"io"
	"mime"
	"net/http"
	"os"
	"path"
	"slices"
	"strings"

	"github.com/PuerkitoBio/goquery"
)

const (
	HOST    = "localhost"
	PORT    = "8080"
	ADDRESS = HOST + ":" + PORT
)

const CACHE_DIR = ".cache"

var BLACKLIST = []string{"", ".net", ".ru", ".org", ".com", ".cn", ".gov", ".info"}
var CACHEABLE = []string{".scss", ".css", ".png", ".jpg", ".jpeg", ".gif", ".ico", ".svg", ".pdf", ".tex"}

func scrapeUrl(url string) *http.Response {
	client := &http.Client{}

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return nil
	}

	resp, err := client.Do(req)
	if err != nil {
		return nil
	}

	return resp
}

func getMIMEType(url string) string {
	ext := path.Ext(url)

	if slices.Contains(BLACKLIST, ext) {
		return "text/html"
	}

	return mime.TypeByExtension(ext)
}

func updateLinks(resp *http.Response, prefix string) string {
	doc, _ := goquery.NewDocumentFromReader(resp.Body)

	transform := func(url string) string {
		ownPage := strings.HasPrefix(url, "https://") ||
			strings.HasPrefix(url, "http://") ||
			strings.HasPrefix(url, "//")

		newPrefix := prefix
		if ownPage {
			urlList := strings.Split(url, "//")
			url = fmt.Sprintf("http://%s/%s", ADDRESS, urlList[len(urlList)-1])
		} else {
			if strings.Index(url, "/") <= 1 {
				urlList := strings.SplitN(url, "/", 2)
				if url[0] != '.' {
					prefixList := strings.SplitN(newPrefix, "/", 3)
					newPrefix = fmt.Sprintf("%s/%s", prefixList[0], prefixList[1])
				}
				url = urlList[len(urlList)-1]
			}
			url = fmt.Sprintf("http://%s/%s", newPrefix, url)
		}

		return url
	}

	for _, tag := range []string{"a", "style", "script", "img", "link"} {
		doc.Find(tag).Each(func(i int, s *goquery.Selection) {
			if href, exists := s.Attr("href"); exists {
				fmt.Println(i, transform(href))
				s.SetAttr("href", transform(href))
			}
			if src, exists := s.Attr("src"); exists {
				fmt.Println(i, transform(src))
				s.SetAttr("src", transform(src))
			}
		})
	}

	result, _ := doc.Html()
	return result
}

func HomeHandler(w http.ResponseWriter, r *http.Request) {
	var result string

	urlPrefix := strings.Trim(r.URL.String(), "/")
	newPrefix := fmt.Sprintf("%s/%s", ADDRESS, urlPrefix)

	if len(urlPrefix) == 0 {
		fmt.Fprintln(w, "<p>Good morning, Vietnam</p>")
		return
	}

	payload := func(resp *http.Response) string {
		mimeType := getMIMEType(urlPrefix)
		w.Header().Set("Content-Type", mimeType)

		if mimeType == "text/html" {
			result = updateLinks(resp, newPrefix)
		} else {
			bytes, err := io.ReadAll(resp.Body)
			if err != nil {
				return ""
			}
			result = string(bytes)
		}

		fmt.Fprintln(w, result)
		return result
	}

	isCached := func(url string) bool {
		_, err := os.Stat(fmt.Sprintf(".cache/%s", url))
		return !(errors.Is(err, os.ErrNotExist) || errors.Is(err, os.ErrPermission))
	}

	isCacheable := func(url string) bool {
		ext := path.Ext(url)
		return slices.Contains(CACHEABLE, ext)
	}

	DumpCache := func(url string, data string) {
		urlList := strings.Split(url, "/")
		cachePath := fmt.Sprintf("%s/%s", CACHE_DIR, strings.Join(urlList[:len(urlList)-1], "/"))
		os.MkdirAll(cachePath, 0776)

		if err := os.WriteFile(fmt.Sprintf("%s/%s", CACHE_DIR, url), []byte(data), 0666); err != nil {
			fmt.Println(err)
		}
	}

	LoadCache := func(url string) string {
		if data, err := os.ReadFile(fmt.Sprintf("%s/%s", CACHE_DIR, urlPrefix)); err == nil {
			return string(data)
		}
		return ""
	}

	if isCacheable(urlPrefix) && isCached(urlPrefix) {
		mimeType := getMIMEType(urlPrefix)
		w.Header().Set("Content-Type", mimeType)
		fmt.Fprintln(w, LoadCache(urlPrefix))
		fmt.Printf("Loaded from cache: %s\n", urlPrefix)
		return
	}

	for _, pref := range []string{"http", "https"} {
		if resp := scrapeUrl(fmt.Sprintf("%s://%s", pref, urlPrefix)); resp != nil {
			defer resp.Body.Close()
			if data := payload(resp); isCacheable(urlPrefix) {
				fmt.Printf("New cache: %s\n", urlPrefix)
				DumpCache(urlPrefix, data)
			}
			break
		}
	}
}

func main() {
	http.HandleFunc("/", HomeHandler)
	http.ListenAndServe(ADDRESS, nil)
}
