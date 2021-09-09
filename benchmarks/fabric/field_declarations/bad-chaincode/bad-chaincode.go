package main

import (
	"fmt"

	"github.com/hyperledger/fabric-contract-api-go/contractapi"
)

type BadChainCode struct {
  globalValue string // this is a risk
	contractapi.Contract
}

func main() {
	chaincode, err := contractapi.NewChaincode(new(BadChainCode))
	if err != nil {
		fmt.Printf("Error starting BadChainCode: %s", err)
	}
	if err := chaincode.Start(); err != nil {
		fmt.Printf("Error starting BadChainCode: %s", err.Error())
	}
}

func (t *BadChainCode) Invoke(ctx contractapi.TransactionContextInterface) error {
  t.globalValue = "Hello"
	return nil
}
