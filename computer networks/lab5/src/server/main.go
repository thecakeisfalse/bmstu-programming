package main

import (
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"syscall"
	"time"
	"unsafe"

	"github.com/creack/pty"
	"github.com/gliderlabs/ssh"
)

const (
	DEFAULT_PREFIX     = ".ssh"
	DEFAULT_KEY_FILE   = "key"
	DEFAULT_DEATH_TIME = 5 * time.Second
)

var DEFAULT_SHELL = os.Getenv("SHELL")

// var DEFAULT_SHELL = "/bin/bash"

func executeShellCommand(command string) (string, error) {
	out, err := exec.Command(DEFAULT_SHELL, "-c", command).Output()
	return string(out), err
}

func doesPathExist(path string) bool {
	_, err := os.Stat(path)
	return !(errors.Is(err, os.ErrNotExist) || errors.Is(err, os.ErrPermission))
}

func generateKeyFile() string {
	if !doesPathExist(DEFAULT_PREFIX) {
		os.Mkdir(DEFAULT_PREFIX, 0777)
	}

	keyPath := fmt.Sprintf("%s/%s", DEFAULT_PREFIX, DEFAULT_KEY_FILE)
	if doesPathExist(keyPath) {
		return keyPath
	}

	cmd := fmt.Sprintf("yes | ssh-keygen -f %s -t rsa -N \"\"", keyPath)
	executeShellCommand(cmd)

	return keyPath
}

func isPasswordInShadow(password string) bool {
	return password == "secret"
}

func setWinsize(f *os.File, w, h int) {
	syscall.Syscall(syscall.SYS_IOCTL, f.Fd(), uintptr(syscall.TIOCSWINSZ),
		uintptr(unsafe.Pointer(&struct{ h, w, x, y uint16 }{uint16(h), uint16(w), 0, 0})))
}

func main() {
	ssh.Handle(func(s ssh.Session) {
		ptyReq, winCh, isPty := s.Pty()

		if isPty {
			os.Chdir(os.Getenv("HOMEc"))
			cmd := exec.Command(DEFAULT_SHELL)
			cmd.Env = append(cmd.Env, fmt.Sprintf("TERM=%s", ptyReq.Term))
			f, err := pty.Start(cmd)
			if err != nil {
				panic(err)
			}
			go func() {
				for win := range winCh {
					setWinsize(f, win.Width, win.Height)
				}
			}()
			go func() {
				io.Copy(f, s)
			}()
			io.Copy(s, f)
			cmd.Wait()
		} else {
			command := s.RawCommand()

			cmd := exec.Command(DEFAULT_SHELL, "-c", command)
			cmd.Stdout = s
			cmd.WaitDelay = DEFAULT_DEATH_TIME

			cmd.Start()
			timer := time.AfterFunc(DEFAULT_DEATH_TIME, func() {
				cmd.Stdout = nil
				err := cmd.Process.Kill()
				if err != nil {
					panic(err)
				}
			})
			cmd.Wait()
			timer.Stop()
		}
	})

	ssh.ListenAndServe(":2222", nil,
		ssh.PasswordAuth(func(ctx ssh.Context, pass string) bool {
			return isPasswordInShadow(pass)
		}),
		ssh.HostKeyFile(generateKeyFile()),
	)
}
