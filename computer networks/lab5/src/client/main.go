package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	osuser "os/user"
	"slices"
	"strings"
	"sync"

	"github.com/melbahja/goph"
	"golang.org/x/crypto/ssh"
	"golang.org/x/crypto/ssh/terminal"
)

var (
	auth goph.Auth
	addr string
	user string
	port uint
)

var DEFAULT_YES_ARRAY = []string{"y", "yes", "да"}

const DEFAULT_PROMPT = "# "

func verifyHost(host string, remote net.Addr, key ssh.PublicKey) error {
	if hostFound, err := goph.CheckKnownHost(host, remote, key, ""); hostFound {
		return err
	}

	if !askIsHostTrusted(host, key) {
		return errors.New("you typed no, aborted")
	}

	return goph.AddKnownHost(host, remote, key, "")
}

func main() {
	usr, err := osuser.Current()

	if err != nil {
		fmt.Println("couldn't determine current user.  defaulting to 'root'")
		usr.Username = "root"
	}

	flag.StringVar(&addr, "ip", "127.0.0.1", "machine ip address.")
	flag.StringVar(&user, "user", usr.Username, "ssh user.")
	flag.UintVar(&port, "port", 22, "ssh port number.")

	flag.Parse()

	auth = goph.Password(askPass("Enter SSH Password: "))

	if err != nil {
		panic(err)
	}

	client, err := goph.NewConn(&goph.Config{
		User:     user,
		Addr:     addr,
		Port:     port,
		Auth:     auth,
		Callback: verifyHost,
	})

	if err != nil {
		panic(err)
	}

	defer client.Close()

	sshCommandHandler(client)
}

func askPass(prompt string) string {
	fmt.Print(prompt)

	pass, err := terminal.ReadPassword(0)

	if err != nil {
		panic(err)
	}

	fmt.Println("")

	return strings.TrimSpace(string(pass))
}

func askIsHostTrusted(host string, key ssh.PublicKey) bool {
	reader := bufio.NewReader(os.Stdin)

	fmt.Printf("Unknown Host: %s \nFingerprint: %s \n", host, ssh.FingerprintSHA256(key))
	fmt.Print("Would you like to add it? type yes or no: ")

	a, err := reader.ReadString('\n')

	if err != nil {
		log.Fatal(err)
	}

	return slices.Contains(DEFAULT_YES_ARRAY, strings.ToLower(strings.TrimSpace(a)))
}

func sshCommandHandler(client *goph.Client) {
	fmt.Printf("Connected to %s\n", client.Config.Addr)
	fmt.Println("Type your shell command and enter.")
	fmt.Println("To exit type: exit")

	scanner := bufio.NewScanner(os.Stdin)

	fmt.Print(DEFAULT_PROMPT)

	for scanner.Scan() {
		cmd := scanner.Text()
		parts := strings.Split(cmd, " ")

		if len(parts) < 1 {
			continue
		}

		switch parts[0] {
		case "exit", "quit":
			{
				os.Exit(0)
			}
		default:
			{
				command, err := client.Command(parts[0], parts[1:]...)
				if err != nil {
					panic(err)
				}

				stdout, _ := command.StdoutPipe()

				var wg sync.WaitGroup
				wg.Add(1)

				go func() {
					defer wg.Done()
					reader := bufio.NewReader(stdout)
					for {
						readString, err := reader.ReadString('\n')
						if err != nil || err == io.EOF {
							return
						}
						fmt.Print(readString)
					}
				}()

				command.Start()
				wg.Wait()
				command.Wait()
			}
		}

		fmt.Print(DEFAULT_PROMPT)
	}
}
