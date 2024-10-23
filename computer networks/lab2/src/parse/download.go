package main

import (
	"fmt"
	"net/http"

	log "github.com/mgutz/logxi/v1"
	"golang.org/x/net/html"
)

func getAttr(node *html.Node, key string) string {
	for _, attr := range node.Attr {
		if attr.Key == key {
			return attr.Val
		}
	}
	return ""
}

func getChildren(node *html.Node) []*html.Node {
	var children []*html.Node
	for c := node.FirstChild; c != nil; c = c.NextSibling {
		children = append(children, c)
	}
	return children
}

func isElem(node *html.Node, tag string) bool {
	return node != nil && node.Type == html.ElementNode && node.Data == tag
}

func isClass(node *html.Node, tag string) bool {
	return node != nil && node.Type == html.ElementNode && getAttr(node, "class") == tag
}

func isText(node *html.Node) bool {
	return node != nil && node.Type == html.TextNode
}

func isDiv(node *html.Node, class string) bool {
	return isElem(node, "div") && getAttr(node, "class") == class
}

type Item struct {
	Code, Nominal, Name, Rate, Difference string
}

func readItem(item *html.Node) *Item {
	var arr []string = make([]string, 0)
	for c := item.FirstChild; c != nil; c = c.NextSibling {
		// a -> span -> text
		if c.FirstChild == nil {
			continue
		}
		for as := c.FirstChild.FirstChild; as != nil; as = as.NextSibling {
			if isText(as.FirstChild) {
				arr = append(arr, as.FirstChild.Data)
			}
		}
	}
	return &Item{
		Code:       arr[0],
		Nominal:    arr[1],
		Name:       arr[2],
		Rate:       arr[3],
		Difference: arr[4],
	}
}

func search(node *html.Node) []*Item {
	if isElem(node, "table") && getAttr(node, "data-finance_media-desktop") == "currency-table" {
		var items []*Item
		for c := node.FirstChild; c != nil; c = c.NextSibling {
			if !isElem(c, "tbody") {
				continue
			}
			for as := c.FirstChild; as != nil; as = as.NextSibling {

				if item := readItem(as); item != nil {
					items = append(items, item)
				}

			}
		}
		return items
	}
	for c := node.FirstChild; c != nil; c = c.NextSibling {
		if items := search(c); items != nil {
			return items
		}
	}
	return nil
}

func downloadNews() []*Item {
	const link = "https://finance.rambler.ru/currencies/"
	log.Info("sending request to %s", link)
	if response, err := http.Get(link); err != nil {
		log.Error("request to %s failed", link, "error", err)
	} else {
		defer response.Body.Close()
		status := response.StatusCode
		if status == http.StatusOK {
			if doc, err := html.Parse(response.Body); err != nil {
				log.Error("invalid HTML from %s", link, "error", err)
			} else {
				log.Info("HTML parsed successfully")
				fmt.Println("HTML PARSER")

				return search(doc)
			}
		}
	}
	return nil
}
