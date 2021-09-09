package main

import (
	"fmt"

	"github.com/hyperledger/fabric-contract-api-go/contractapi"
)

type DoGetHistory struct {
	contractapi.Contract
}

func main() {
	chaincode, err := contractapi.NewChaincode(new(DoGetHistory))
	if err != nil {
		fmt.Printf("Error starting DoGetHistory: %s", err)
	}
	if err := chaincode.Start(); err != nil {
		fmt.Printf("Error starting DoGetHistory: %s", err.Error())
	}
}


func (t *DoGetHistory) Invoke(ctx contractapi.TransactionContextInterface) ([]byte, error) {
  iterator, _ := ctx.GetStub().GetHistoryForKey("key")
  data, _ := iterator.Next()
  value := data.Value
	return value, nil
}
