package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"net"
	"net/http"
	"os"
	"strconv"
	"strings"

	"github.com/gorilla/websocket"
)

const HELP_MSG = `* quit/exit
* connect address port
* edit string
* show
* help`

const PAGE_TEMPLATE = `
<!DOCTYPE html>
<html>
  <head></head>
  <body>
    <div class="container">
      <h2>Enter command</h2>
      <p>Possible commands:</p>
	  <ul>
		<li>connect <i>address</i> <i>port</i></li>
		<li>edit <i>string</i></li>
      </ul>
	  <form id="commandForm" method="post" action="/">
		<label for="command">Command:</label>
		<input type="text" id="command" name="command" required>
		<br />
		<label for="arguments">Arguments:</label>
		<input type="text" id="arguments" name="args" required>
		<br />
		<button type="submit">Execute</button>
      </form>
	</div>
  </body>
  <style>
	a,body,p,span{color:#e0e0e0}.button,button,input,select,table,td,textarea,th{border:1px solid #555}.button,button,
    th{background-color:#333;color:#fff}h1,h2,h3,h4,h5,h6,th{color:#fff}body{background-color:#121212;font-family:Arial,
    sans-serif;margin:0;padding:0}.container{padding:20px 40px}a{color:#1e90ff;text-decoration:none}a:hover{color:#4682b4;
    text-decoration:underline}.button,button{padding:10px 20px;cursor:pointer;transition:background-color .3s,border-color .3s}
    .button:hover,button:hover{background-color:#444;border-color:#666}input,select,textarea,tr:nth-child(2n){background-color:#222}
    footer,tr:nth-child(odd){background-color:#1e1e1e}input,select,textarea{color:#e0e0e0;padding:10px;margin:5px 0}input:focus,select:focus,
    textarea:focus{border-color:#1e90ff;outline:0}table{width:100%;border-collapse:collapse}td,th{padding:10px;text-align:left}
    footer{color:#e0e0e0;text-align:center;padding:10px 0}
  </style>
</html>
`

type NetworkPeer struct {
	ipAddress string
	peerPort  int
}

func (p NetworkPeer) toString() string {
	return fmt.Sprintf("%s:%d", p.ipAddress, p.peerPort)
}

func (p *NetworkPeer) fromString(s string) {
	p.ipAddress = strings.Split(s, ":")[0]
	p.peerPort, _ = strconv.Atoi(strings.Split(s, ":")[1])
}

type Peer struct {
	Self      NetworkPeer
	Server    *net.TCPListener
	Peers     []*NetworkPeer
	Note      string
	Callbacks []*websocket.Conn
}

type Command struct {
	Command string   `json:"command"`
	Args    []string `json:"args"`
}

type Request struct {
	From string           `json:"from"` // Автор запроса
	Data *json.RawMessage `json:"data"`
}

// =====

func wsPort(port int) int {
	return 10*port + 1
}

func (p *NetworkPeer) getWsAddress() NetworkPeer {
	return NetworkPeer{
		ipAddress: p.ipAddress,
		peerPort:  wsPort(p.peerPort),
	}
}

// =====

func (p *Peer) handleCommands(from string, command Command) {
	who := NetworkPeer{}
	who.fromString(from)

	switch command.Command {
	case "peers":
		{
			var args []string
			for _, peer := range p.Peers {
				args = append(args, peer.toString())
			}

			cmd := Command{
				Command: "list",
				Args:    args,
			}

			p.sendRequest(cmd, who)
			p.sendRequest(Command{Command: "edit", Args: []string{p.Note}}, who)
		}
	case "list":
		{

			for _, addr := range command.Args {
				if addr == p.Self.toString() {
					continue
				}
				peer := NetworkPeer{}
				peer.fromString(addr)
				p.Peers = append(p.Peers, &peer)
				p.sendRequest(Command{Command: "add", Args: []string{p.Self.toString()}}, peer)
			}
		}
	case "add":
		{
			for _, peer := range p.Peers {
				if peer.toString() == command.Args[0] {
					return
				}
			}

			p.Peers = append(p.Peers, &who)
			p.addWsPeer(&who)
		}
	// основные операции с пиром
	case "edit":
		{
			p.Note = strings.Join(command.Args, " ")
			if len(p.Note) > 0 /* && p.Self != who */ {
				fmt.Println(p.Note)
			}

			for _, conn := range p.Callbacks {
				if err := conn.WriteMessage(1, []byte(p.Note)); err != nil {
					continue
				}
			}
		}
	}
}

func (p *Peer) handleRequests(conn *net.TCPConn) {
	defer conn.Close()

	buf := bufio.NewReader(conn)
	data, err := buf.ReadString('\n')
	if err != nil {
		return
	}
	data = strings.TrimSpace(data)

	var req Request
	var command Command

	err = json.Unmarshal([]byte(data), &req)
	if err != nil {
		return
	}

	err = json.Unmarshal(*req.Data, &command)
	if err != nil {
		return
	}

	p.handleCommands(req.From, command)
}

func (p *Peer) handlePeers() {
	for {
		if conn, err := p.Server.AcceptTCP(); err == nil {
			go p.handleRequests(conn)
		}
	}
}

// =====

func (p *Peer) sendRequest(data interface{}, who NetworkPeer) {
	var raw json.RawMessage

	raw, err := json.Marshal(data)
	if err != nil {
		return
	}

	request, err := json.Marshal(Request{
		From: p.Self.toString(),
		Data: &raw,
	})
	if err != nil {
		return
	}

	conn, err := net.Dial("tcp", who.toString())
	if err != nil {
		return
	}
	defer conn.Close()

	fmt.Fprintln(conn, string(request))
}

func (p *Peer) broadcast(data interface{}) {
	for _, peer := range p.Peers {
		p.sendRequest(data, *peer)
	}
}

// =====

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
	CheckOrigin: func(r *http.Request) bool {
		return true
	},
}

func (p *Peer) sendWsMessage(ws *websocket.Conn, data string) {
	_ = ws.WriteMessage(websocket.TextMessage, []byte(data))
}

func (p *Peer) addWsPeer(peer *NetworkPeer) {
	peerWs := peer.getWsAddress()
	message := fmt.Sprintf("$c:%s", peerWs.toString())

	for _, callback := range p.Callbacks {
		p.sendWsMessage(callback, message)
	}
}

func (p *Peer) handleWsRequests(ws *websocket.Conn) {
	for {
		_, messageContent, err := ws.ReadMessage()
		if err != nil {
			return
		}

		switch content := string(messageContent); content {
		case "connect":
			{
				p.Callbacks = append(p.Callbacks, ws)
				for _, peer := range p.Peers {
					p.addWsPeer(peer)
				}
			}
		case "callback":
			{
				p.Callbacks = append(p.Callbacks, ws)
				if p.Note != "" {
					p.sendWsMessage(ws, p.Note)
				}
			}
		}
	}
}

func (p *Peer) HomeRouterHandler(w http.ResponseWriter, r *http.Request) {
	_ = r.ParseForm()

	if path := strings.Trim(r.URL.Path, "/"); path == "" {
		var command Command
		switch r.Method {
		case "GET":
			{
				command = Command{
					Command: r.Form.Get("command"),
					Args:    strings.Split(r.Form.Get("args"), " "),
				}
			}
		case "POST":
			{
				command = Command{
					Command: r.PostForm.Get("command"),
					Args:    strings.Split(r.PostForm.Get("args"), " "),
				}
			}
		}

		p.executeCommand(command)

		_, _ = fmt.Fprint(w, PAGE_TEMPLATE)
	}
}

func (p *Peer) handleWebsocket(ipAddress string, port int) {
	http.HandleFunc("/", p.HomeRouterHandler)

	http.HandleFunc("/ws", func(w http.ResponseWriter, r *http.Request) {
		if ws, err := upgrader.Upgrade(w, r, nil); err == nil {
			p.handleWsRequests(ws)
		}
	})

	_ = http.ListenAndServe(fmt.Sprintf("%s:%d", ipAddress, port), nil)
}

// =====

func (p *Peer) executeCommand(command Command) {
	switch command.Command {
	case "connect":
		{
			if len(command.Args) != 2 {
				fmt.Println("expected 2 arguments")
				break
			}

			netAddress := command.Args[0]
			netPort, err := strconv.Atoi(command.Args[1])
			if err != nil {
				fmt.Println(err)
				break
			}

			if len(p.Peers) <= 1 {
				p.sendRequest(
					Command{Command: "peers"},
					NetworkPeer{netAddress, netPort},
				)
			}
		}
	case "edit":
		{
			p.broadcast(command)
		}
	}
}

// =====

func NewPeer(ipAddress string, peerPort int) (*Peer, error) {
	netPeer := NetworkPeer{ipAddress: ipAddress, peerPort: peerPort}

	addr, err := net.ResolveTCPAddr("tcp", netPeer.toString())
	if err != nil {
		return nil, err
	}

	server, err := net.ListenTCP("tcp", addr)
	if err != nil {
		return nil, err
	}

	return &Peer{
		Self:   netPeer,
		Server: server,
		Peers:  []*NetworkPeer{&netPeer},
	}, nil
}

func main() {
	var peerPort int
	var ipAddress string

	flag.IntVar(&peerPort, "p", 1337, "Provide peer port")
	flag.StringVar(&ipAddress, "addr", "127.0.0.1", "Provide ip address")
	flag.Parse()

	peer, err := NewPeer(ipAddress, peerPort)
	if err != nil {
		return
	}

	go peer.handleWebsocket(ipAddress, wsPort(peerPort))
	go peer.handlePeers()

	fmt.Printf("HTTP server: http://%s\n", peer.Self.getWsAddress().toString())

	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("Enter command: ")
		text, _ := reader.ReadString('\n')
		if text = strings.TrimSpace(text); len(text) == 0 {
			continue
		}

		switch s := strings.Split(text, " "); s[0] {
		case "exit", "quit":
			return
		case "connect", "edit":
			peer.executeCommand(Command{
				Command: s[0],
				Args:    s[1:],
			})
		case "show":
			fmt.Println(peer.Note)
		case "help":
			fmt.Println(HELP_MSG)
		}
	}
}
