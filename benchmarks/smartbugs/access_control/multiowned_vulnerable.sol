/*
 * @source: https://github.com/SmartContractSecurity/SWC-registry/blob/master/test_cases/solidity/unprotected_critical_functions/multiowned_vulnerable/multiowned_vulnerable.sol
 * @author: -
 * @vulnerable_at_lines: 38
 */

// pragma solidity ^0.4.23;
pragma solidity ^0.8.11;

/**
 * @title MultiOwnable
 */
contract MultiOwnable {
  address public root;
  mapping (address => address) public owners; // owner => parent of owner

  /**
  * @dev The Ownable constructor sets the original `owner` of the contract to the sender
  * account.
  */
  // constructor() public {
  constructor() {          // TRUNG: updated to Solidity 0.8.11
    root = msg.sender;
    owners[root] = root;
  }

  /**
  * @dev Throws if called by any account other than the owner.
  */
  modifier onlyOwner() {
    // require(owners[msg.sender] != 0);
    require(owners[msg.sender] != address(0));   // TRUNG: updated to Solidity 0.8.11
    _;
  }

  /**
  * @dev Adding new owners
  * Note that the "onlyOwner" modifier is missing here.
  */
  // <yes> <report> ACCESS_CONTROL
  function newOwner(address _owner) external returns (bool) {
    // require(_owner != 0);
    require(_owner != address(0));               // TRUNG: updated to Solidity 0.8.11
    owners[_owner] = msg.sender;
    return true;
  }

  /**
    * @dev Deleting owners
    */
  function deleteOwner(address _owner) onlyOwner external returns (bool) {
    // require(owners[_owner] == msg.sender || (owners[_owner] != 0 && msg.sender == root));
    require(owners[_owner] == msg.sender || (owners[_owner] != address(0) && msg.sender == root));        // TRUNG: updated to Solidity 0.8.11
    // owners[_owner] = 0;
    owners[_owner] = address(0);                 // TRUNG: updated to Solidity 0.8.11
    return true;
  }
}

contract TestContract is MultiOwnable {

  // function withdrawAll() public onlyOwner {
  function withdrawAll() public onlyOwner {      // TRUNG: updated to Solidity 0.8.11
    // msg.sender.transfer(this.balance);
    payable(msg.sender).transfer(address(this).balance);  // TRUNG: updated to Solidity 0.8.11
  }

  // function() payable {
  receive () external payable {                  // TRUNG: updated to Solidity 0.8.11
  }

}
