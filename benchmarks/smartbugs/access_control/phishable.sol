/*
 * @source: https://github.com/sigp/solidity-security-blog
 * @author: -
 * @vulnerable_at_lines: 20
 */

 pragma solidity ^0.8.11;

 contract Phishable {
    address public owner;

    constructor (address _owner) {
        owner = _owner;
    }

    receive () external payable {} // collect ether

    function withdrawAll(address _recipient) public {
        // <yes> <report> ACCESS_CONTROL
        require(tx.origin == owner);
        payable(_recipient).transfer(address(this).balance);
    }
}
