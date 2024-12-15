package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log"
	"math/big"
	"net/http"
	"os"
	"path/filepath"
	"time"

	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/ethclient"
)

type Config struct {
	Metamask     string `json:"metamask"`
	FirebaseDB   string `json:"firedb"`
	FirebaseAuth string `json:"fireauth"`
}

type Metamask struct {
	API    string
	Client *ethclient.Client
}

type Transaction struct {
	ChainId  string `json:"id"`
	Hash     string `json:"hash"`
	Value    string `json:"value"`
	Cost     string `json:"cost"`
	To       string `json:"to"`
	Gas      uint64 `json:"gas"`
	GasPrice string `json:"gas-price"`
}

type Block struct {
	Number            uint64 `json:"number"`
	Time              uint64 `json:"timestamp"`
	Difficulty        uint64 `json:"difficulty"`
	Hash              string `json:"hash"`
	TransactionsCount int    `json:"transactions"`
}

func createMetamask(apiToken string) *Metamask {
	if len(apiToken) == 0 {
		log.Fatal("Invalid Metamask token")
	}

	m := &Metamask{
		API: fmt.Sprintf("https://mainnet.infura.io/v3/%s", apiToken),
	}

	client, err := ethclient.Dial(m.API)
	if err != nil {
		log.Fatalln(err)
	}

	m.Client = client

	return m
}

func (m *Metamask) fetchLatestBlock() *types.Block {
	header, err := m.Client.HeaderByNumber(context.Background(), nil)
	if err != nil {
		log.Fatal(err)
	}

	blockNumber := big.NewInt(header.Number.Int64())
	block, err := m.Client.BlockByNumber(context.Background(), blockNumber)
	if err != nil {
		log.Fatal(err)
	}

	return block
}

func extractBlock(block *types.Block) (*Block, []*Transaction) {
	blockData := &Block{
		Number:            block.Number().Uint64(),
		Time:              block.Time(),
		Difficulty:        block.Difficulty().Uint64(),
		Hash:              block.Hash().Hex(),
		TransactionsCount: len(block.Transactions()),
	}

	var transactions []*Transaction

	for _, transaction := range block.Transactions() {
		transactionData := &Transaction{
			ChainId:  transaction.ChainId().String(),
			Hash:     transaction.Hash().Hex(),
			Value:    transaction.Value().String(),
			Cost:     transaction.Cost().String(),
			To:       transaction.To().Hex(),
			Gas:      transaction.Gas(),
			GasPrice: transaction.GasPrice().String(),
		}

		transactions = append(transactions, transactionData)
	}

	return blockData, transactions
}

type Firebase struct {
	Database  string
	AuthToken string
}

func createFirebase(database string, apiToken string) *Firebase {
	if len(database) == 0 || len(apiToken) == 0 {
		log.Fatal("Invalid configuration")
	}

	return &Firebase{
		Database:  database,
		AuthToken: apiToken,
	}
}

func (f *Firebase) send(url string, data interface{}) {
	jsonData, err := json.Marshal(data)
	if err != nil {
		log.Fatal(err)
	}

	resp, err := http.Post(url, "application/json", bytes.NewBuffer(jsonData))
	if err != nil {
		log.Fatal(err)
	}

	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		log.Fatal(resp.StatusCode)
	}
}

func (f *Firebase) sendBlock(block *Block) {
	blockUrlBase := "%s/blocks/%s.json?auth=%s"

	blockUrl := fmt.Sprintf(blockUrlBase, f.Database, block.Hash, f.AuthToken)

	f.send(blockUrl, block)
}

func (f *Firebase) sendTransaction(hash string, transaction *Transaction) {
	transactionUrlBase := "%s/blocks/%s/transactions/%s.json?auth=%s"

	transactionUrl := fmt.Sprintf(transactionUrlBase, f.Database, hash, transaction.Hash, f.AuthToken)

	f.send(transactionUrl, transaction)
}

func main() {
	filename, _ := filepath.Abs("./config.json")
	file, err := os.ReadFile(filename)
	if err != nil {
		log.Fatal(err)
	}

	var config Config
	if err = json.Unmarshal(file, &config); err != nil {
		log.Fatal(err)
	}

	fmt.Println(config)

	m := createMetamask(config.Metamask)
	f := createFirebase(config.FirebaseDB, config.FirebaseAuth)

	for {
		b := m.fetchLatestBlock()

		block, transactions := extractBlock(b)

		f.sendBlock(block)

		for _, transaction := range transactions {
			f.sendTransaction(block.Hash, transaction)
		}

		time.Sleep(5 * time.Second)
	}
}
