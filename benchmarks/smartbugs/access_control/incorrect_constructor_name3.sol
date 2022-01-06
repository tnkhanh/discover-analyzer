/*
 * @source: https://smartcontractsecurity.github.io/SWC-registry/docs/SWC-118#incorrect-constructor-name2sol
 * @author: Ben Perez
 * @vulnerable_at_lines: 17
 */

// pragma solidity ^0.4.24;
pragma solidity ^0.8.11;

contract Missing{
    // address private owner;       // TRUNG: original code
    address payable private owner;  // updated to comply with Solidity 0.8.11

    modifier onlyowner {
        require(msg.sender==owner);
        _;
    }
    // <yes> <report> ACCESS_CONTROL
    function Constructor()
        public
    {
        owner = payable(msg.sender);
    }

    // function () payable {}      // TRUNG: original code
    receive () external payable {} // updated to comply with Solidity 0.8.11

    function withdraw()
        public
        onlyowner
    {
        // owner.transfer(this.balance);       // TRUNG: original code
        owner.transfer(payable(this).balance); // updated to comply with Solidity 0.8.11
    }

}
