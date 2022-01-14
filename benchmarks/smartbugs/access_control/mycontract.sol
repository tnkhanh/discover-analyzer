/*
 * @source: https://consensys.github.io/smart-contract-best-practices/recommendations/#avoid-using-txorigin
 * @author: Consensys Diligence
 * @vulnerable_at_lines: 20
 * Modified by Gerhard Wagner
 */

// pragma solidity ^0.4.24;
pragma solidity ^0.8.11;

contract MyContract {

    address owner;

    // function MyContract() public {
    constructor () {               // TRUNG: updated to Solidity 0.8.11
        owner = msg.sender;
    }

    function sendTo(address receiver, uint amount) public {
        // <yes> <report> ACCESS_CONTROL
        require(tx.origin == owner);
        payable(receiver).transfer(amount);
    }

}
