/*
 * @source: https://github.com/sigp/solidity-security-blog
 * @author: -
 * @vulnerable_at_lines: 20
 */

 // pragma solidity ^0.4.22;
 pragma solidity ^0.8.11;

 contract Phishable {
    address public owner;

    constructor (address _owner) {
        owner = _owner;
    }

    // function () public payable {} // collect ether
    receive () external payable {} // collect ether           // TRUNG: updated to Solidity 0.8.11

    function withdrawAll(address _recipient) public {
        // <yes> <report> ACCESS_CONTROL
        require(tx.origin == owner);
        // _recipient.transfer(this.balance);
        payable(_recipient).transfer(address(this).balance);  // TRUNG: updated to Solidity 0.8.11
    }
}
