/*
 * @source: https://github.com/sigp/solidity-security-blog
 * @author: Suhabe Bugrara
 * @vulnerable_at_lines: 31,38
 */

//added pragma version
// pragma solidity ^0.4.0;
pragma solidity ^0.8.11;

contract FibonacciBalance {

    address public fibonacciLibrary;
    // the current fibonacci number to withdraw
    uint public calculatedFibNumber;
    // the starting fibonacci sequence number
    uint public start = 3;
    uint public withdrawalCounter;
    // the fibonancci function selector
    // bytes4 constant fibSig = bytes4(sha3("setFibonacci(uint256)"));   // TRUNG: deprecated
    bytes4 constant fibSig = bytes4(keccak256("setFibonacci(uint256)")); // adjusted to Solidity 0.8.11

    // constructor - loads the contract with ether
    constructor(address _fibonacciLibrary) payable {
        fibonacciLibrary = _fibonacciLibrary;
    }

    // function withdraw() {
    function withdraw() public {  // TRUNG: adjusted to Solidity 0.8.11
        withdrawalCounter += 1;
        // calculate the fibonacci number for the current withdrawal user
        // this sets calculatedFibNumber
        // <yes> <report> ACCESS_CONTROL
        // require(fibonacciLibrary.delegatecall(fibSig, withdrawalCounter));          // TRUNG: deprecated code
        (bool success, ) = fibonacciLibrary.delegatecall(abi.encode(fibSig, withdrawalCounter));
        if (success == false) {
          revert ("DelegateCall reverted");
        }
        // msg.sender.transfer(calculatedFibNumber * 1 ether);        // TRUNG: deprecated code
        payable(msg.sender).transfer(calculatedFibNumber * 1 ether);  // adjusted to Solidity 0.8.11
    }

    // allow users to call fibonacci library functions
    // function() public {   // TRUNG: deprecated code
    fallback() external {      // adjusted to Solidity 0.8.11
        // <yes> <report> ACCESS_CONTROL
        // require(fibonacciLibrary.delegatecall(msg.data));          // TRUNG: deprecated code
      (bool success, ) = fibonacciLibrary.delegatecall(msg.data);
        if (success == false) {
          revert ("DelegateCall reverted");
        }
    }
}

// library contract - calculates fibonacci-like numbers;
contract FibonacciLib {
    // initializing the standard fibonacci sequence;
    uint public start;
    uint public calculatedFibNumber;

    // modify the zeroth number in the sequence
    function setStart(uint _start) public {
        start = _start;
    }

    function setFibonacci(uint n) public {
        calculatedFibNumber = fibonacci(n);
    }

    function fibonacci(uint n) internal returns (uint) {
        if (n == 0) return start;
        else if (n == 1) return start + 1;
        else return fibonacci(n - 1) + fibonacci(n - 2);
    }
}
