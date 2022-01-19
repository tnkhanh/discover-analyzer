/*
 * @source: https://smartcontractsecurity.github.io/SWC-registry/docs/SWC-112#proxysol
 * @author: -
 * @vulnerable_at_lines: 19
 */

// pragma solidity ^0.4.24;
pragma solidity ^0.8.11;

contract Proxy {

  address owner;

  // constructor() public {
  constructor() {                     // TRUNG: updated to Solidity 0.8.11
    owner = msg.sender;
  }

  function forward(address callee, bytes calldata _data) public {
    // <yes> <report> ACCESS_CONTROL
    (bool success, ) = callee.delegatecall(_data); //Use delegatecall with caution and make sure to never call into untrusted contracts
    require(success);
  }

}
