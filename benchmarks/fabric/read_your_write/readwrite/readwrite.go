package main

import (
	"fmt"

	"github.com/hyperledger/fabric-contract-api-go/contractapi"
)

type DoReadWrite struct {
	contractapi.Contract
}

func main() {
	chaincode, err := contractapi.NewChaincode(new(DoReadWrite))
	if err != nil {
		fmt.Printf("Error starting DoReadWrite: %s", err)
	}
	if err := chaincode.Start(); err != nil {
		fmt.Printf("Error starting DoReadWrite: %s", err.Error())
	}
}

func (t *DoReadWrite) Invoke(ctx contractapi.TransactionContextInterface) error {
  stub := ctx.GetStub()
  val := "val"
  err := stub.PutState("key", []byte(val))
  if err != nil {
    return err
  }

  _, err = stub.GetState("key")
  if err != nil {
    return err
  }
	return nil
}
