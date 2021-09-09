package main

import (
	"fmt"
  "math/rand"

	"github.com/hyperledger/fabric-contract-api-go/contractapi"
)

type DoRandom struct {
	contractapi.Contract
}

func main() {
	chaincode, err := contractapi.NewChaincode(new(DoRandom))
	if err != nil {
		fmt.Printf("Error starting DoRandom: %s", err)
	}
	if err := chaincode.Start(); err != nil {
		fmt.Printf("Error starting DoRandom: %s", err.Error())
	}
}


func (t *DoRandom) Invoke(ctx contractapi.TransactionContextInterface) (int, error) {
  rand.Seed(7)
  result := rand.Intn(10)
	return result, nil
}
