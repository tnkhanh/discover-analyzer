package main

import (
	"fmt"
  "time"

	"github.com/hyperledger/fabric-contract-api-go/contractapi"
)

type DoTime struct {
	contractapi.Contract
}

func main() {
	chaincode, err := contractapi.NewChaincode(new(DoTime))
	if err != nil {
		fmt.Printf("Error starting DoTime: %s", err)
	}
	if err := chaincode.Start(); err != nil {
		fmt.Printf("Error starting DoTime: %s", err.Error())
	}
}

func (t *DoTime) Invoke(ctx contractapi.TransactionContextInterface) (time.Time, error) {
  current_time := time.Now()
	return current_time, nil
}
